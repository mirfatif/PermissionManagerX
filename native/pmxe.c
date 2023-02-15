#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <android/log.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <libgen.h>
#include <linux/securebits.h>
#include <netinet/tcp.h>
#include <sched.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/file.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/xattr.h>
#include <unistd.h>

static char *MY_NAME = "pmx";

static int log_android_info(char *format, ...)
{
    va_list args;
    va_start(args, format);
    __android_log_vprint(ANDROID_LOG_INFO, MY_NAME, format, args);
    va_end(args);
    return 0;
}

static void log_android_err(char *format, va_list args, bool print_code)
{
    if (print_code)
    {
        char args_[256];
        vsnprintf(args_, sizeof(args_), format, args);
        __android_log_print(ANDROID_LOG_ERROR, MY_NAME, "%s: %s", args_, strerror(errno));
    }
    else
        __android_log_vprint(ANDROID_LOG_ERROR, MY_NAME, format, args);
}

static int print_err(char *format, ...)
{
    fprintf(stderr, "%d %s: ", getpid(), MY_NAME);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    log_android_err(format, args, false);
    va_end(args);

    fprintf(stderr, "\n");
    fflush(NULL);
    return 1;
}

static int print_err_code(char *format, ...)
{
    fprintf(stderr, "%d %s: ", getpid(), MY_NAME);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    log_android_err(format, args, true);
    va_end(args);

    fprintf(stderr, ": %s\n", strerror(errno));
    fflush(NULL);
    return 1;
}

static void print_out(char *format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stdout, format, args);
    va_end(args);
    fprintf(stdout, "\n");
    fflush(NULL);
}

static bool is_number(char *num, char *type)
{
    int len = strlen(num);
    if (!len)
        return false;

    for (int i = 0; i < len; i++)
    {
        if (!isdigit(num[i]))
            return print_err("Invalid %s: %s", type, num) == 0;
    }

    return true;
}

static int close_std_fd(int std_fd, int rw_flags)
{
    int fd = open("/dev/null", rw_flags);

    if (fd < 0)
        return print_err_code("Failed to open /dev/null");

    int err = 0;
    if (dup2(fd, std_fd) == -1)
        err = print_err_code("Failed to redirect %d to /dev/null", std_fd);

    close(fd);
    return err;
}

static int set_namespace(char *pid)
{
    char path[25];
    snprintf(path, sizeof(path), "/proc/%s/ns/mnt", pid);
    int fd = open(path, O_RDONLY);

    int err = 0;
    if (fd < 0)
        return print_err_code("Failed to read %s", path);
    else if (setns(fd, 0))
        err = print_err_code("Failed to switch namespaces");

    close(fd);

    return err;
}

static int set_cgroups()
{
    FILE *file = fopen("/proc/self/mountinfo", "r");
    if (!file)
        return print_err("Failed to open /proc/self/mountinfo");

    int err = 0;

    char line[1024], path[1024];

    while (fgets(line, sizeof(line), file))
    {
        int col = 1, offset = -1;
        for (int i = 0; i < strlen(line); i++)
        {
            if (line[i] == ' ')
            {
                col++;
                if (col == 5 || col == 9)
                    offset = i + 1;
                continue;
            }

            if (offset != -1 && col == 6)
            {
                size_t size = i - 1 - offset;
                strncpy(path, line + offset, size);
                strcpy(path + size, "/cgroup.procs");
                offset = -1;
            }
            else if (offset != -1 && col == 10)
            {
                if (strncmp("cgroup", line + offset, 6) && strncmp("cgroup2", line + offset, 7))
                    break;
                FILE *procs_file = fopen(path, "r+");
                if (!procs_file)
                    err = print_err("Failed to open %s", path);
                else
                {
                    if (fprintf(procs_file, "%d", getpid()) < 0)
                        err = print_err("Failed to add myself to %s", path);
                    fclose(procs_file);
                }
                break;
            }
        }
    }

    fclose(file);

    return err;
}

static int get_last_cap()
{
    int last_cap = CAP_LAST_CAP;
    int fd = open("/proc/sys/kernel/cap_last_cap", O_RDONLY);
    if (fd < 0)
        print_err_code("Failed to open cap_last_cap");
    else
    {
        char buf[8];
        int num = read(fd, buf, sizeof(buf));
        if (num <= 0)
            print_err_code("Failed to read cap_last_cap");
        else
        {
            num = strtoul(buf, NULL, 10);
            if (num <= 0 || num > CAP_LAST_CAP)
                print_err("Failed to parse cap_last_cap");
            else
                last_cap = num;
        }
        close(fd);
    }
    return last_cap;
}

static cap_t get_caps()
{
    cap_t caps = cap_get_proc();
    if (caps)
        return caps;

    print_err_code("Failed to interpret capabilities");
    return NULL;
}

static bool has_cap(cap_t caps, const cap_value_t cap, cap_flag_t type)
{
    const cap_value_t cap_arr[1] = {cap};
    cap_flag_value_t val;
    return !cap_get_flag(caps, *cap_arr, type, &val) && val == CAP_SET;
}

static bool has_eff_cap(const cap_value_t cap)
{
    cap_t caps = get_caps();
    if (!caps)
        return false;

    bool res = has_cap(caps, cap, CAP_EFFECTIVE);
    cap_free(caps);
    return res;
}

static void print_missing_caps(cap_flag_t type)
{
    char *type_name = "Effective";
    if (type == CAP_PERMITTED)
        type_name = "Permitted";
    else if (type == CAP_INHERITABLE)
        type_name = "Inheritable";

    cap_t caps = get_caps();
    if (!caps)
        return;

    int last_cap = get_last_cap();
    fprintf(stderr, "%s: Missing %s caps:", MY_NAME, type_name);
    for (int i = 0; i <= last_cap; i++)
    {
        if (!has_cap(caps, i, type))
            fprintf(stderr, " %s(%d)", cap_to_name(i), i);
    }
    print_err("");
    cap_free(caps);
}

static bool raise_cap(cap_value_t cap)
{
    cap_t caps = get_caps();
    if (!caps)
        return false;

    const cap_value_t cap_arr[1] = {cap};
    cap_set_flag(caps, CAP_EFFECTIVE, 1, cap_arr, CAP_SET);

    bool res = true;
    if (cap_set_proc(caps))
        res = false;

    cap_free(caps);
    return res;
}

static int raise_setuid_cap()
{
    if (!raise_cap(CAP_SETUID))
        return print_err_code("Failed to raise SETUID capability");
    return 0;
}

static int raise_setgid_cap()
{
    if (!raise_cap(CAP_SETGID))
        return print_err_code("Failed to raise SETGID capability");
    return 0;
}

static int raise_eff_caps()
{
    cap_t caps = get_caps();
    if (!caps)
        return 1;

    int err = 0;

    bool caps_missing = false;
    int last_cap = get_last_cap();
    for (int i = 0; i <= last_cap; i++)
    {
        const cap_value_t cap_arr[1] = {i};
        if (has_cap(caps, i, CAP_PERMITTED))
            cap_set_flag(caps, CAP_EFFECTIVE, 1, cap_arr, CAP_SET);
        else
            caps_missing = true;
    }
    if (cap_set_proc(caps))
    {
        err = print_err_code("Failed to raise capabilities");
        caps_missing = true;
    }

    if (caps_missing)
        print_missing_caps(CAP_EFFECTIVE);

    cap_free(caps);

    return err;
}

static int raise_amb_caps()
{
    if (!CAP_AMBIENT_SUPPORTED())
        return print_err("Ambient caps not supported");

    cap_t caps = get_caps();
    if (!caps)
        return 1;

    int last_cap = get_last_cap();

    for (int i = 0; i <= last_cap; i++)
    {
        const cap_value_t cap_arr[1] = {i};
        if (has_cap(caps, i, CAP_PERMITTED) && cap_get_bound(i) == CAP_SET)
            cap_set_flag(caps, CAP_INHERITABLE, 1, cap_arr, CAP_SET);
    }

    int err = 0;

    if (cap_set_proc(caps))
        err = print_err_code("Failed to raise inheritable capabilities");

    cap_free(caps);

    caps = get_caps();
    if (!caps)
        return 1;

    for (int i = 0; i <= last_cap; i++)
    {
        if (!has_cap(caps, i, CAP_PERMITTED) || !has_cap(caps, i, CAP_INHERITABLE))
            continue;

        if (cap_set_ambient(i, CAP_SET))
            err = print_err_code("Failed to raise ambient capability: %s", cap_to_name(i));
    }

    cap_free(caps);
    return err;
}

static int lock_caps()
{
    if (prctl(PR_SET_SECUREBITS, SECBIT_NO_SETUID_FIXUP, 0, 0, 0))
        return print_err_code("Failed to set no_setuid_fixup security bit");
    return 0;
}

static int set_uid(uid_t uid)
{
    int err = raise_setuid_cap();

    if (setresuid(uid, uid, uid))
        return print_err_code("Failed to set uid");

    return err;
}

static int set_gid(gid_t gid)
{
    int err = raise_setgid_cap();

    if (setresgid(gid, gid, gid))
        return print_err_code("Failed to set gid", true);

    if (setgroups(0, 0))
        return print_err_code("Failed to clear groups", true);

    return err;
}

static int set_groups(char *groups)
{
    int err = raise_setgid_cap();

    char groups_cpy[strlen(groups) + 1];
    strcpy(groups_cpy, groups);

    int size = 1;
    char *token = strtok(groups, ",");
    while ((token = strtok(0, ",")))
        size++;

    token = strtok(groups_cpy, ",");
    gid_t gids[size];
    for (int i = 0; i < size; i++)
    {
        if (!is_number(token, "gid"))
            return 1;
        gids[i] = strtol(token, NULL, 10);
        token = strtok(0, ",");
    }

    if (setgroups(size, gids))
        return print_err_code("Failed to set groups");

    return err;
}

static int set_context(char *context)
{
    int fd = open("/proc/self/attr/current", O_WRONLY);

    int err = 0;
    if (fd < 0)
        return print_err_code("Failed to open /proc/self/attr/current");

    if (flock(fd, LOCK_EX))
        err = print_err_code("Failed to get flock on /proc/self/attr/current");

    else if (write(fd, context, strlen(context)) == -1)
        err = print_err_code("Failed to switch context");

    close(fd);
    return err;
}

static int set_privs(char *ns_pid, bool cgroups, int gid, char *groups, bool retain_caps, int uid, char *context)
{
    int err = 0;
    if (ns_pid && set_namespace(ns_pid))
        err = 1;

    if (cgroups && set_cgroups())
        err = 1;

    if (gid != -1 && set_gid(gid))
        err = 1;

    if (groups && set_groups(groups))
        err = 1;

    if (retain_caps)
        if (raise_eff_caps() || raise_amb_caps() || lock_caps())
            err = 1;

    if (uid != -1 && set_uid(uid))
        err = 1;

    if (context && set_context(context))
        err = 1;

    return err;
}

static int exec_it(char **argv)
{
    char *exe = argv[0];
    bool exists = access(exe, F_OK) != -1;
    if (!exists)
    {
        char *path = getenv("PATH");
        char path_cpy[strlen(path) + 1];
        strcpy(path_cpy, path);
        char *path_dir = strtok(path_cpy, ":");
        while (path_dir)
        {
            char file[strlen(path_dir) + strlen(exe) + 2];
            snprintf(file, sizeof(file), "%s/%s", path_dir, exe);
            if (access(file, F_OK) != -1)
            {
                exists = true;
                break;
            }
            path_dir = strtok(0, ":");
        }
    }

    if (!exists)
        return print_err("Failed to execute %s: No such file", exe);

    execvp(exe, argv);
    return print_err_code("Failed to execute %s", exe);
}

#define OOM_VAL_MIN "-1000"

static void set_oom_scr_adj(pid_t pid, char *value)
{
    if (!has_eff_cap(CAP_SYS_RESOURCE))
        return;

    char path[30];
    snprintf(path, sizeof(path), "/proc/%i/oom_score_adj", pid);

    int fd = open(path, O_WRONLY);
    if (fd < 0)
        return (void)print_err_code("Failed to open %s", path);

    if (flock(fd, LOCK_EX))
        print_err_code("Failed to get flock on %s", path);
    else if (write(fd, value, strlen(value)) == -1)
        print_err_code("failed writing %s to %s", value, path);

    close(fd);
}

static void run_cmd(int argc, char *cmd, bool wait_for_child)
{
    char *argv[argc + 1];

    argv[0] = strtok(cmd, " ");
    for (int i = 1; i < argc; i++)
        argv[i] = strtok(0, " ");
    argv[argc] = NULL;

    if (wait_for_child)
        signal(SIGCHLD, SIG_DFL);

    pid_t pid = fork();
    if (pid == -1)
        print_err_code("Failed to fork");
    else if (pid == 0)
        exit(exec_it(argv));
    else if (pid > 0 && wait_for_child)
        wait(NULL);

    signal(SIGCHLD, SIG_IGN);
}

#define RUN_DAEMON_MIN_ARGS 5
#define NO_SELABEL "null"
#define DAEMON_GROUPS "2000,3003,3009,1015,1023,1078,9997"

static void run_daemon(int argc, char *args)
{
    char *cp = strtok(args, " ");
    char *uid_str = strtok(0, " ");
    char *context = strtok(0, " ");
    char *nice_name = strtok(0, " ");
    char *class = strtok(0, " ");

    char nice_name_str[256];
    snprintf(nice_name_str, sizeof(nice_name_str), "--nice-name=%s", nice_name);

    char *argv[argc];
    argv[0] = "app_process";
    argv[1] = "/";
    argv[2] = nice_name_str;
    argv[3] = class;
    for (int i = 4; i < argc - 1; i++)
        argv[i] = strtok(0, " ");
    argv[argc - 1] = NULL;

    if (access(cp, F_OK))
        return (void)print_err_code("%s does not exists", cp);

    pid_t pid = fork();
    if (pid == -1)
        return (void)print_err_code("Failed to fork");
    else if (pid > 0)
        return (void)log_android_info("Starting daemon with PID %d", pid);

    close_std_fd(STDOUT_FILENO, O_WRONLY);
    close_std_fd(STDIN_FILENO, O_RDONLY);

    set_oom_scr_adj(getpid(), OOM_VAL_MIN);
    setenv("CLASSPATH", cp, 1);

    if (geteuid() == 0)
    {
        if (!is_number(uid_str, "uid"))
            return;

        uid_t uid = strtol(uid_str, NULL, 10);

        char groups[strlen(DAEMON_GROUPS) + 1];
        strncpy(groups, DAEMON_GROUPS, sizeof(groups));
        context = strncmp(context, NO_SELABEL, strlen(NO_SELABEL)) ? context : NULL;

        if (set_privs("1", true, uid, groups, true, uid, context))
            return;
    }

    exit(exec_it(argv));
}

#define LOCAL_HOST "127.0.0.1"

static int send_stderr(int port)
{
    struct sockaddr_in addr;

    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd == -1)
        return print_err_code("Failed to create socket");

    int opt = 1;
    if (setsockopt(sock_fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(opt)))
        print_err_code("Failed to set socket option TCP_NODELAY");

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = inet_addr(LOCAL_HOST);

    int err = 0;
    if (connect(sock_fd, (struct sockaddr *)&addr, sizeof(addr)) == -1)
        err = print_err_code("Failed to connect to socket");
    else if (dup2(sock_fd, STDERR_FILENO) == -1)
        err = print_err_code("Failed to redirect stderr to socket");

    close(sock_fd);
    return err;
}

static int save_file(size_t size, char *path)
{
    FILE *file = fopen(path, "w");
    if (!file)
        return print_err_code("Failed to create file");

    clearerr(stdin);
    clearerr(file);

    size_t buf_size = 8192;
    unsigned char buf[buf_size];

    size_t to_read = size;
    int err = 0;

    while (to_read > 0)
    {
        if (to_read < buf_size)
            buf_size = to_read;

        size_t read = fread(buf, 1, buf_size, stdin);

        if (read == 0 || ferror(stdin))
        {
            err = print_err("Failed to read data for %s", path);
            break;
        }

        if (fwrite(buf, read, 1, file) != 1 || ferror(file))
        {
            err = print_err("Failed to write to %s", path);
            break;
        }

        to_read -= read;
    }

    fclose(file);

    if (to_read != 0)
        err = 1;

    if (!err)
    {
        struct stat sbuf;
        if (stat(path, &sbuf))
            err = print_err_code("Failed to get file size");
        else if (sbuf.st_size != size)
            err = print_err("Saved file size is different");
        else
            log_android_info("Successfully saved %s", path);
    }

    if (err)
        unlink(path);

    return err;
}

#define SELINUX_XATTR "security.selinux"
#define SELABEL_PARENT "PARENT"

static int set_perms(char *cmd)
{
    char *col;

    if (!(col = strtok(cmd, " ")))
        return print_err("perm requires uid");
    if (!is_number(col, "uid"))
        return 1;
    uid_t uid = strtoul(col, NULL, 10);

    if (!(col = strtok(0, " ")))
        return print_err("perm requires gid");
    if (!is_number(col, "gid"))
        return 1;
    gid_t gid = strtoul(col, NULL, 10);

    if (!(col = strtok(0, " ")))
        return print_err("perm requires mode");
    if (!is_number(col, "mode"))
        return 1;
    mode_t mode = strtoul(col, NULL, 8);

    if (!(col = strtok(0, " ")))
        return print_err("perm requires context");
    char *context = col;

    if (!(col = strtok(0, " ")))
        return print_err("perm requires path");
    char *path = col;

    int err = 0;

    if (chown(path, uid, gid))
        err = print_err_code("Failed to set owner");

    if (chmod(path, mode))
        err = print_err_code("Failed to set mode");

    char *val = NULL;

    if (!strcmp(context, SELABEL_PARENT))
    {
        context = NULL;
        size_t len = getxattr(dirname(path), SELINUX_XATTR, val, 0);
        if (len == -1)
            err = print_err_code("Failed to get parent context length");
        else if (len == 0)
            err = print_err("No context set on parent");
        else
        {
            val = malloc(len);
            if (getxattr(dirname(path), SELINUX_XATTR, val, len) != len)
                err = print_err_code("Failed to get parent context");
            else
                context = val;
        }
    }

    if (context && setxattr(path, SELINUX_XATTR, context, strlen(context), 0))
        err = print_err_code("Failed to set context");

    free(val);
    return err;
}

static bool exit_on_app_died = false;

#define CMD_HELLO "hello"
#define CMD_EXIT "exit"
#define CMD_AUTO_EXIT "auto_exit"
#define CMD_RUN "run"
#define CMD_RUN_BG "run_bg"
#define CMD_RUN_DMN "run_dmn"
#define CMD_SAVE_FILE "save_file"
#define CMD_FILE_EXIST "exist"
#define CMD_SET_PERM "perm"

#define RESP_PORT "PORT:"
#define RESP_UID "UID:"
#define RESP_EXIST "EXIST"
#define RESP_SAVED "SAVED"

static void wait_for_commands()
{
    char *col, *line = NULL, *line_cpy;
    size_t line_alloc = 0;
    int n;

    while ((n = getline(&line, &line_alloc, stdin)) > 0)
    {
        if (line[n - 1] == '\n')
            line[n - 1] = '\0';

        line_cpy = malloc(strlen(line) + 1);
        strcpy(line_cpy, line);

        if (!(col = strtok(line, " ")))
            goto end;

        if (!strcmp(col, CMD_HELLO))
        {
            col = strtok(0, " ");
            if (!col)
                print_err("%s requires port", CMD_HELLO);
            else if (is_number(col, "port") && !send_stderr(strtol(col, NULL, 10)))
                print_out("%s %d", RESP_UID, getuid());
        }

        else if (!strcmp(col, CMD_FILE_EXIST))
        {
            col = strtok(0, " ");
            if (!col)
                print_err("%s requires path", CMD_FILE_EXIST);
            else
                print_out((access(col, F_OK) ? "" : RESP_EXIST));
        }

        else if (!strcmp(col, CMD_SAVE_FILE))
        {
            col = strtok(0, " ");
            if (!col)
                print_err("%s requires file name", CMD_SAVE_FILE);
            else if (is_number(col, "size"))
            {
                size_t size = strtoul(col, NULL, 10);
                if (!(col = strtok(0, " ")))
                    print_err("%s requires file path", CMD_SAVE_FILE);
                else
                    print_out(save_file(size, col) ? "" : RESP_SAVED);
            }
        }

        else if (!strcmp(col, CMD_SET_PERM))
            set_perms(line_cpy + 1 + strlen(col));

        else if (!strcmp(col, CMD_RUN) || !strcmp(col, CMD_RUN_BG) || !strcmp(col, CMD_RUN_DMN))
        {
            char *cmd = col;

            int cnt = 0;
            while ((col = strtok(0, " ")))
                cnt++;

            if (!strcmp(cmd, CMD_RUN_DMN))
            {
                if (cnt < RUN_DAEMON_MIN_ARGS)
                    print_err("%s requires at least %d arguments", CMD_RUN_DMN, RUN_DAEMON_MIN_ARGS);
                else
                    run_daemon(cnt, line_cpy + 1 + strlen(cmd));
            }
            else if (cnt == 0)
                print_err("%s requires argument", cmd);
            else
                run_cmd(cnt, line_cpy + 1 + strlen(cmd), !strcmp(cmd, CMD_RUN));
        }

        else if (!strcmp(line, CMD_EXIT))
        {
            close(STDERR_FILENO);

            log_android_info("Bye bye from PID %d", getpid());
            exit(EXIT_SUCCESS);
        }

        else if (!strcmp(col, CMD_AUTO_EXIT))
        {
            col = strtok(0, " ");
            if (!col)
                print_err("%s requires bool value", CMD_AUTO_EXIT);
            else
            {
                if (!strcmp(col, "true"))
                    exit_on_app_died = true;
                else if (!strcmp(col, "false"))
                    exit_on_app_died = false;
                else
                    print_err("%s requires bool value", CMD_AUTO_EXIT);
            }
        }

        else
        {
            int err = 0;
            for (char *c = line_cpy; *c; c++)
            {
                if (!isprint(c[0]))
                {
                    err = print_err("Bad command: [non-printable]");
                    break;
                }
            }

            if (!err)
                print_err("Bad command: %s", line_cpy);
        }

        fflush(NULL);

    end:
        free(line_cpy);
    }

    free(line);
}

static int wait_for_app(int sock_fd)
{
    if ((listen(sock_fd, 1)))
        return print_err_code("Failed to listen on socket");

    log_android_info("Waiting for clients");

    struct sockaddr_in add;
    socklen_t len = sizeof(add);

    while (true)
    {
        int fd = accept(sock_fd, (struct sockaddr *)&add, &len);
        if (fd < 0)
            print_err_code("Failed to accept on socket");
        else
        {
            int err = 0;
            if (dup2(fd, STDOUT_FILENO) == -1)
                err = print_err_code("Failed to redirect stdout to socket");
            else if (dup2(fd, STDIN_FILENO) == -1)
                err = print_err_code("Failed to redirect stdin to socket");

            close(fd);

            if (!err)
            {
                log_android_info("New connection established");
                wait_for_commands();
                log_android_info("Connection lost");
            }

            close_std_fd(STDOUT_FILENO, O_WRONLY);
            close_std_fd(STDIN_FILENO, O_RDONLY);
            close_std_fd(STDERR_FILENO, O_WRONLY);
        }

        if (exit_on_app_died)
        {
            log_android_info("App died. Bye bye from PID %d", getpid());
            return 0;
        }

        sleep(1);
    }

    return log_android_info("Why are you here!");
}

static int bind_server(int sock_fd)
{
    struct sockaddr_in addr, assigned;
    bzero(&addr, sizeof(addr));

    bzero(&assigned, sizeof(assigned));
    socklen_t len = sizeof(assigned);

    addr.sin_family = AF_INET;
    addr.sin_port = 0;
    addr.sin_addr.s_addr = inet_addr(LOCAL_HOST);

    char ip[16];

    if (bind(sock_fd, (struct sockaddr *)&addr, sizeof(addr)))
        return print_err_code("Failed to bind");
    else if (getsockname(sock_fd, (struct sockaddr *)&assigned, &len))
        return print_err_code("Failed to get port");
    else if (!inet_ntop(AF_INET, &assigned.sin_addr, ip, sizeof(ip)))
        return print_err_code("Failed to get address string");

    unsigned int port = ntohs(assigned.sin_port);

    pid_t pid = fork();
    if (pid == -1)
        return print_err_code("Failed to fork");
    else if (pid > 0)
    {

        usleep(100 * 1000);
        print_out("%s %d", RESP_PORT, port);
        sleep(1);
        return 0;
    }

    if (setsid() == -1)
        print_err_code("Failed to create session");

    if (signal(SIGHUP, SIG_IGN) == SIG_ERR)
        print_err_code("Failed to ignore SIGHUP");

    char *path = malloc(64);
    snprintf(path, 64, "/proc/%d/fd", getpid());

    DIR *fds = opendir(path);
    if (fds == NULL)
        print_err_code("Failed to open %s", path);
    else
    {
        struct dirent *d;
        while ((d = readdir(fds)))
        {
            int fd = atoi(d->d_name);
            if (fd > 2 && fd != sock_fd && close(fd))
                print_err_code("Failed to close fd %d", fd);
        }

        closedir(fds);
    }

    free(path);

    close_std_fd(STDOUT_FILENO, O_WRONLY);
    close_std_fd(STDERR_FILENO, O_WRONLY);
    close_std_fd(STDIN_FILENO, O_RDONLY);

    return wait_for_app(sock_fd);
}

static int kill_duplicate()
{
    DIR *proc = opendir("/proc");
    if (proc == NULL)
        return print_err_code("Failed to open /proc");

    struct dirent *d;
    while ((d = readdir(proc)))
    {
        int pid = atoi(d->d_name);
        if (pid <= 0 || pid == getpid())
            continue;

        char buf[2048];
        snprintf(buf, sizeof(buf), "/proc/%d/cmdline", pid);

        int fd = open(buf, O_RDONLY | O_CLOEXEC);
        if (fd < 0)
            continue;

        int len = read(fd, buf, sizeof(buf) - 1);
        close(fd);

        if (len < 0)
            continue;

        buf[len] = 0;

        if (!strncmp(MY_NAME, buf, strlen(MY_NAME)))
        {
            log_android_info("Killing %d", pid);
            kill(pid, SIGKILL);
        }
    }

    closedir(proc);
    return 0;
}

static int daemonize(int argc, char **argv, char *suffix)
{
    int exe_len = strlen(argv[0]);

    size_t name_size = strlen(suffix) + 16;
    if (exe_len < name_size)
        name_size = exe_len;

    char name[name_size];
    snprintf(name, name_size, "pmxd-%s-%d", suffix, geteuid());
    MY_NAME = name;

    for (int i = 0; i < argc; i++)
    {
        int len = strlen(argv[i]);
        for (int j = 0; j < len; j++)
            argv[i][j] = '\0';
    }

    strncpy(argv[0], name, name_size);

    kill_duplicate();

    set_oom_scr_adj(getpid(), OOM_VAL_MIN);

    signal(SIGCHLD, SIG_IGN);

    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd == -1)
        return print_err_code("Failed to create socket");

    if (fcntl(sock_fd, F_SETFD, FD_CLOEXEC))
        print_err_code("Failed to set fd option FD_CLOEXEC");

    int opt = 1;
    if (setsockopt(sock_fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(opt)))
        print_err_code("Failed to set socket option TCP_NODELAY on listening socket");

    int err = bind_server(sock_fd);
    close(sock_fd);
    return err;
}

int main(int argc, char **argv)
{
    if (argc != 3)
        return 1;

    if (geteuid() == 0)
        set_privs("1", true, -1, NULL, true, -1, NULL);

    return daemonize(argc, argv, argv[2]);
}
