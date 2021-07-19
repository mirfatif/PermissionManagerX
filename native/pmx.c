// setns(), setresuid(), setresgid()
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <errno.h>
#include <stdbool.h>
#include <sys/xattr.h>
#include <sys/wait.h>

// va_start()
#include <stdarg.h>

// stdout, stderr
#include <stdio.h>

// isdigit()
#include <ctype.h>

// strtol(), getenv()
#include <stdlib.h>

// getopt_long() and 'option' struct
#include <getopt.h>

// read(), write(), close(), setresuid(), setresgid(), setgroups(), access(), execvp(), chown()
#include <unistd.h>

// setns()
#include <sched.h>

// open() and flock() flags
#include <sys/file.h>

// strlen(), strerror(), strtok(), strcpy(), strcmp(), strncpy(), strncmp()
#include <string.h>

// chmod()
#include <sys/stat.h>

// dirname()
#include <libgen.h>

// Capabilities
#include <sys/capability.h>

// prctl()
#include <sys/prctl.h>

// Secure bits
#include <linux/securebits.h>

// socket()
#include <sys/socket.h>

// struct sockaddr_in
#include <netinet/in.h>

// inet_addr()
#include <arpa/inet.h>

// TCP_NODELAY
#include <netinet/tcp.h>

// offsetof()
#include <stddef.h>

#ifndef ANDROID

// For setgroups(). Android have it in <unistd.h>
#include <grp.h>

#else

#include <android/log.h>

#endif

/////////////////////////////////////////////////////////////////////////

char *MY_NAME = "pmx";

int print_err(char *format, ...)
{
    fprintf(stderr, "%s: ", MY_NAME);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, "\n");
    fflush(NULL);
    return 1;
}

int print_err_code(char *format, ...)
{
    fprintf(stderr, "%s: ", MY_NAME);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, ": %s\n", strerror(errno));
    fflush(NULL);
    return 1;
}

int dup_error(char *str)
{
    return print_err("duplicate %s", str);
}

void print_out(char *format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stdout, format, args);
    va_end(args);
    fprintf(stdout, "\n");
    fflush(NULL);
}

bool is_number(char *num, char *type)
{
    int len = strlen(num);
    if (!len)
        return false;

    for (int i = 0; i < len; i++)
    {
        if (!isdigit(num[i]))
            return print_err("invalid %s: %s", type, num) == 0;
    }

    return true;
}

int show_usage()
{
    print_out("");
    print_out("Usage:");
    print_out(" %s <options> [-- <program> [<argument,...>]]", MY_NAME);
    print_out("");
    print_out("Options:");
    print_out(" --ns <pid>                Enter namespaces of given process");
    print_out(" --set-cg                  Set all cgroups to root");
    print_out(" -g, --gid <gid>           Set real and effective gid");
    print_out(" --groups <group,...>      Set supplementary groups");
    print_out(" -u, --uid <uid>           Set real and effective uid");
    print_out(" --rcaps                   Try to raise/retain all capabilities");
    print_out(" --cxt <context>           Switch SELinux context");
    print_out(" --pr[=arg]                Print current privileges");
    print_out("                           Optional arg to print supported caps");
    print_out(" -D[port], --daemon[=port] Run daemon and listen on stdin");
    print_out("                           Optional port for stdin and stdout");
    print_out("                           Commands:");
    print_out("                           exit, exist <file_name>");
    print_out("                           run <file_name>, save <size> <file_name>");
    print_out("                           perm <uid> <gid> <mode> <cxt> <file_name>");
    print_out(" -h, --help                Show this help");
    print_out("");
    return 1;
}

/////////////////////////////////////////////////////////////////////////

void set_namespace(char *pid)
{
    char path[25];
    snprintf(path, sizeof(path), "/proc/%s/ns/mnt", pid);
    int fd = open(path, O_RDONLY);
    if (fd < 0)
        return (void)print_err_code("failed to read %s", path);
    else if (setns(fd, 0))
        print_err_code("failed to switch namespaces");
    close(fd);
}

/////////////////////////////////////////////////////////////////////////

void set_cgroups()
{
    FILE *file = fopen("/proc/self/mountinfo", "r");
    if (!file)
        return (void)print_err("failed to open /proc/self/mountinfo");

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
                    break; // Go to next line

                FILE *procs_file = fopen(path, "r+");
                if (!procs_file)
                    print_err("failed to open %s", path);
                else
                {
                    if (fprintf(procs_file, "%d", getpid()) < 0)
                        print_err("failed to add myself to %s", path);
                    fclose(procs_file);
                }
                break;
            }
        }
    }
    fclose(file);
}

/////////////////////////////////////////////////////////////////////////

int get_last_cap()
{
    int last_cap = CAP_LAST_CAP;
    int fd = open("/proc/sys/kernel/cap_last_cap", O_RDONLY);
    if (fd < 0)
        print_err_code("failed to open cap_last_cap");
    else
    {
        char buf[8];
        int num = read(fd, buf, sizeof(buf));
        if (num <= 0)
            print_err_code("failed to read cap_last_cap");
        else
        {
            num = strtoul(buf, NULL, 10);
            if (num <= 0 || num > CAP_LAST_CAP)
                print_err("failed to parse cap_last_cap");
            else
                last_cap = num;
        }
        close(fd);
    }
    return last_cap;
}

bool has_cap(cap_t caps, const cap_value_t cap, cap_flag_t type)
{
    const cap_value_t cap_arr[1] = {cap};
    cap_flag_value_t val;
    return !cap_get_flag(caps, *cap_arr, type, &val) && val == CAP_SET;
}

void print_missing_caps(cap_flag_t type)
{
    char *type_name = "Effective";
    if (type == CAP_PERMITTED)
        type_name = "Permitted";
    else if (type == CAP_INHERITABLE)
        type_name = "Inheritable";

    cap_t caps = cap_get_proc();
    int last_cap = get_last_cap();
    fprintf(stderr, "%s: Missing %s caps:", MY_NAME, type_name);
    for (int i = 0; i <= last_cap; i++)
    {
        if (!has_cap(caps, i, type))
            fprintf(stderr, " %s(%d)", cap_to_name(i), i);
    }
    fprintf(stderr, "\n");
    cap_clear(caps);
}

bool raise_cap(cap_value_t cap)
{
    bool res = true;
    cap_t caps = cap_get_proc();
    const cap_value_t cap_arr[1] = {cap};
    cap_set_flag(caps, CAP_EFFECTIVE, 1, cap_arr, CAP_SET);
    if (cap_set_proc(caps))
        res = false;
    cap_free(caps);
    return res;
}

void raise_setuid_cap()
{
    if (!raise_cap(CAP_SETUID))
        print_err_code("failed to raise SETUID capability");
}

void raise_setgid_cap()
{
    if (!raise_cap(CAP_SETGID))
        print_err_code("failed to raise SETGID capability");
}

void raise_eff_caps()
{
    /*
     * New Effective set must be a subset of new Permitted set.
     * New Permitted set must be a subset of current Permitted set.
     * "Programmatically adjusting capability sets":
     * https://man7.org/linux/man-pages/man7/capabilities.7.html
     */
    cap_t caps = cap_get_proc();
    if (!caps)
        return (void)print_err_code("failed to interpret capabilities");

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
        print_err_code("failed to raise capabilities");
        caps_missing = true;
    }

    if (caps_missing)
        print_missing_caps(CAP_EFFECTIVE);
    cap_free(caps);
}

int raise_amb_caps()
{
    /*
     * Only Ambient caps are transferred to Effective (and Permitted) sets of execve()'d program:
     * P'(ambient)   = (file is privileged) ? 0 : P(ambient)
     * P'(effective) = F(effective) ? P'(permitted) : P'(ambient)
     */
    if (!CAP_AMBIENT_SUPPORTED())
        return print_err("ambient caps not supported");

    int last_cap = get_last_cap();
    cap_t caps = cap_get_proc();
    if (!caps)
        return print_err_code("failed to interpret capabilities");

    /*
     * Permitted and Inheritable sets are limiting supersets of Ambient set.
     * Bounding set is limiting superset of Inheritable set.
     */
    for (int i = 0; i <= last_cap; i++)
    {
        const cap_value_t cap_arr[1] = {i};
        if (has_cap(caps, i, CAP_PERMITTED) && cap_get_bound(i) == CAP_SET)
            cap_set_flag(caps, CAP_INHERITABLE, 1, cap_arr, CAP_SET);
    }
    if (cap_set_proc(caps))
        print_err_code("failed to raise inheritable capabilities");

    cap_free(caps);

    // Get new (Effective, Inheritable and Permitted) caps with raised Inheritable set.
    caps = cap_get_proc();
    if (!caps)
        return print_err_code("failed to interpret capabilities");

    for (int i = 0; i <= last_cap; i++)
    {
        if (!has_cap(caps, i, CAP_PERMITTED) || !has_cap(caps, i, CAP_INHERITABLE))
            continue;

        if (cap_set_ambient(i, CAP_SET))
            print_err_code("failed to raise ambient capability: %s", cap_to_name(i));
    }

    cap_free(caps);
    return 0;
}

int lock_caps()
{
    /*
     * Retain Permitted, Effective and Ambient set on UID change. SECBIT_KEEP_CAPS retains
     * only Permitted set. So Effective and Ambient sets need to be raised after UID change.
     */
    if (prctl(PR_SET_SECUREBITS, SECBIT_NO_SETUID_FIXUP, 0, 0, 0))
        return print_err_code("failed to set no_setuid_fixup security bit");
    return 0;
}

/////////////////////////////////////////////////////////////////////////

void print_caps(bool print_all)
{
    int last_cap = get_last_cap();

    print_out("");

    if (print_all)
    {
        fputs("All caps :", stdout);
        for (int i = 0; i <= last_cap; i++)
            fprintf(stdout, " %s(%i)", cap_to_name(i), i);
        print_out("\n");
    }

    // Effective, Inheritable and Permitted sets
    cap_t current = cap_get_proc();
    if (current == NULL)
        print_err_code("capabilities not available");
    else
    {
        char *text = cap_to_text(current, NULL);
        if (text == NULL)
            print_err_code("failed to textualize capabilities");
        else
            print_out("EIP caps    : %s", text);
        cap_free(text);
    }
    cap_free(current);

    fputs("Ambient set :", stdout);
    if (!CAP_AMBIENT_SUPPORTED())
        fputs("not supported", stdout);
    else
        for (int i = 0; i <= last_cap; i++)
            if (cap_get_ambient(i) == 1)
                fprintf(stdout, " %s(%i)", cap_to_name(i), i);
    print_out("");

    fputs("Bounding set:", stdout);
    for (int i = 0; i <= last_cap; i++)
        if (cap_get_bound(i) == 1)
            fprintf(stdout, " %s(%i)", cap_to_name(i), i);
    print_out("\n");

    int bits = prctl(PR_GET_SECUREBITS, 0, 0, 0, 0);
    if (bits < 0)
        print_err_code("failed to get securebits");
    else
    {
        fputs("Secure bits :", stdout);
        if (bits & SECBIT_KEEP_CAPS)
            fputs(" keep_caps", stdout);
        if (bits & SECBIT_KEEP_CAPS_LOCKED)
            fputs(" keep_caps_locked", stdout);
        if (bits & SECBIT_NO_SETUID_FIXUP)
            fputs(" no_setuid_fixup", stdout);
        if (bits & SECBIT_NO_SETUID_FIXUP_LOCKED)
            fputs(" no_setuid_fixup_locked", stdout);
        if ((bits & SECBIT_NOROOT) == 1)
            fputs(" no_root", stdout);
        if (bits & SECBIT_NOROOT_LOCKED)
            fputs(" no_root_locked", stdout);
        if (bits & SECBIT_NO_CAP_AMBIENT_RAISE)
            fputs(" no_cap_ambient_raise", stdout);
        if (bits & SECBIT_NO_CAP_AMBIENT_RAISE_LOCKED)
            fputs(" no_cap_ambient_raise_locked", stdout);
        print_out("");
    }

    int r = prctl(PR_GET_NO_NEW_PRIVS, 0, 0, 0, 0);
    if (r < 0)
        print_err_code("failed to get NO_NEW_PRIVS");
    else
        print_out("no_new_privs: %s", (r == 1 ? "set" : "unset"));

    print_out("");

    uid_t ruid, euid, suid;
    if (getresuid(&ruid, &euid, &suid))
        print_err_code("failed to get uid");
    else
        print_out("resuid: %i, %i, %i", ruid, euid, suid);

    gid_t rgid, egid, sgid;
    if (getresgid(&rgid, &egid, &sgid))
        print_err_code("failed to get gid");
    else
        print_out("resgid: %i, %i, %i", rgid, egid, sgid);

    gid_t groups[100];
    int count = getgroups(100, groups);
    if (count < 0)
        print_err_code("failed to get supplementary groups");
    else if (count > 0)
    {
        fputs("groups:", stdout);
        for (int i = 0; i < count; i++)
            fprintf(stdout, " %i", groups[i]);
        print_out("");
    }

    print_out("");
}

/////////////////////////////////////////////////////////////////////////

int set_gid(gid_t gid)
{
    raise_setgid_cap();

    if (setresgid(gid, gid, gid))
        return print_err_code("failed to set gid", true);
    if (setgroups(0, 0))
        return print_err_code("failed to clear groups", true);
    return 0;
}

/////////////////////////////////////////////////////////////////////////

int set_groups(char *groups)
{
    raise_setgid_cap();

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
        return print_err_code("failed to set groups");
    return 0;
}

/////////////////////////////////////////////////////////////////////////

int set_uid(uid_t uid)
{
    raise_setuid_cap();

    if (setresuid(uid, uid, uid))
        return print_err_code("failed to set uid");
    return 0;
}

/////////////////////////////////////////////////////////////////////////

// setcon() or setexeccon() require libselinux
void set_context(char *context)
{
    int fd = open("/proc/self/attr/current", O_WRONLY);
    if (fd < 0)
        return (void)print_err_code("failed to open /proc/self/attr/current");
    if (flock(fd, LOCK_EX))
        print_err_code("failed to get flock on /proc/self/attr/current");
    else if (write(fd, context, strlen(context)) == -1)
        print_err_code("failed to switch context");
    close(fd); // Lock is also released in fd closed
}

/////////////////////////////////////////////////////////////////////////

int exec_it(char **argv, int optind)
{
    char *exe = argv[optind];
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
        return print_err("failed to execute %s: No such file", exe);

    execvp(exe, argv + optind);
    return print_err_code("failed to execute %s", exe);
}

/////////////////////////////////////////////////////////////////////////

int save_file(size_t total, char *path)
{
    FILE *file = fopen(path, "w");
    if (!file)
    {
        print_err_code("failed to create file");
        return 1;
    }

    size_t buf_size = 8192;
    unsigned char buf[buf_size];

    size_t to_read = total;
    bool failed = false;

    while (to_read > 0)
    {
        if (to_read < buf_size)
            buf_size = to_read;
        size_t read = fread(buf, 1, buf_size, stdin);
        fwrite(buf, read, 1, file);
        to_read -= read;
    }
    fclose(file);

    if (!failed)
    {
        struct stat sbuf;
        if (stat(path, &sbuf))
            print_err_code("failed to get file size");
        else if (sbuf.st_size == total)
            return 0;
    }
    return 1;
}

#define SELINUX_XATTR "security.selinux"

int set_perms(char *cmd)
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

    if (chown(path, uid, gid))
        print_err_code("failed to set owner");

    if (chmod(path, mode))
        print_err_code("failed to set mode");

    char *val = NULL;

    if (!strcmp(context, "PARENT"))
    {
        context = NULL;
        size_t len = getxattr(dirname(path), SELINUX_XATTR, val, 0);
        if (len == -1)
            print_err_code("failed to get parent context length");
        else if (len == 0)
            print_err("no context set on parent");
        else
        {
            // Retunred length includes trailing NULL byte
            val = malloc(len);
            if (getxattr(dirname(path), SELINUX_XATTR, val, len) != len)
                print_err_code("failed to get parent context");
            else
                context = val;
        }
    }

    if (context && setxattr(path, SELINUX_XATTR, context, strlen(context), 0))
        print_err_code("failed to set context");

    free(val);
    return 0;
}

/////////////////////////////////////////////////////////////////////////

void run_cmd(int argc, char *cmd)
{
    char *argv[argc + 1];
    argv[argc] = NULL;

    char *token = strtok(cmd, " ");
    for (int i = 0; i < argc; i++)
    {
        argv[i] = token;
        token = strtok(0, " ");
    }

    pid_t pid = fork();
    if (pid == -1)
        print_err_code("failed to fork");
    else if (pid > 0)
        wait(NULL);
    else if (pid == 0)
        exit(exec_it(argv, 0));
}

/////////////////////////////////////////////////////////////////////////

int daemonize(int argc, char **argv, int port)
{
    char name[12];
    snprintf(name, sizeof(name), "pmxd-%d", geteuid());
    MY_NAME = name;

    int name_len = strlen(argv[0]);

    for (int i = 0; i < argc; i++)
    {
        int len = strlen(argv[i]);
        for (int j = 0; j < len; j++)
            argv[i][j] = '\0';
    }

    strncpy(argv[0], MY_NAME, name_len);

#ifdef ANDROID
    __android_log_write(ANDROID_LOG_INFO, MY_NAME, "Starting daemon");
#endif

    if (port != -1)
    {
        struct sockaddr_in addr;
        int opt = 1;

        int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
        if (sock_fd == -1)
            return print_err_code("failed to create socket");
        else if (setsockopt(sock_fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(opt)) == -1)
            print_err_code("failed to set socket options");

        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        addr.sin_addr.s_addr = inet_addr("127.0.0.1");

        int failed = 0;
        if (connect(sock_fd, (struct sockaddr *)&addr, sizeof(addr)) == -1)
            failed = print_err_code("failed to connect to socket");
        else if (dup2(sock_fd, STDOUT_FILENO) == -1)
            failed = print_err_code("failed to redirect stdout to socket");
        else if (dup2(sock_fd, STDIN_FILENO) == -1)
            failed = print_err_code("failed to redirect stdin to socket");

        close(sock_fd);
        if (failed)
            return 1;
    }

    print_out("HELLO %d", geteuid());

    char *col, *line = NULL;
    size_t line_alloc = 0;

    while (getline(&line, &line_alloc, stdin) > 0)
    {
        if (!(line = strtok(line, "\n")))
            continue;

        char line_cpy[strlen(line) + 1];
        strcpy(line_cpy, line);

        if (!(col = strtok(line, " ")))
            continue;

        if (!strcmp(col, "exist"))
        {
            col = strtok(0, " ");
            if (!col)
                print_err("exists requires path");
            else
                print_out((access(col, F_OK) ? "" : "EXIST"));
        }
        else if (!strcmp(col, "save"))
        {
            col = strtok(0, " ");
            if (!col)
                print_err("save requires size");
            else if (is_number(col, "size"))
            {
                size_t size = strtoul(col, NULL, 10);
                col = strtok(0, " ");
                if (!col)
                    print_err("save requires path");
                else if (save_file(size, col))
                    print_out("");
                else
                    print_out("SAVED");
            }
        }
        else if (!strcmp(col, "perm"))
            set_perms(line_cpy + 5);
        else if (!strcmp(col, "run"))
        {
            int cnt = 0;
            while ((col = strtok(0, " ")))
                cnt++;
            if (cnt == 0)
                print_err("run requires argument");
            else
                run_cmd(cnt, line_cpy + 4);
        }
        else if (!strcmp(line, "exit"))
            break;
        else
            print_err("bad option");

        fflush(NULL);
    }
    free(line);

#ifdef ANDROID
    __android_log_write(ANDROID_LOG_INFO, MY_NAME, "Bye bye");
#endif

    return 0;
}

/////////////////////////////////////////////////////////////////////////

/*
 * In C, 'const' qualifier does not create a compile-time constant. It merely designates
 * that a run-time variable is read-only. So we need them to be here.
 * May also define them as macros.
 */
enum
{
    DUMMY, // Start from 1
    NS,
    SET_CG,
    GROUPS,
    R_CAPS,
    CXT,
    PR_CAPS
};

int main(int argc, char **argv)
{
    if (argc == 1)
        return show_usage();

    // https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Options.html
    static const struct option long_opts[] = {
        {"ns", required_argument, 0, NS},
        {"set-cg", no_argument, 0, SET_CG},
        {"gid", required_argument, 0, 'g'},
        {"groups", required_argument, 0, GROUPS},
        {"uid", required_argument, 0, 'u'},
        {"rcaps", no_argument, 0, R_CAPS},
        {"cxt", required_argument, 0, CXT},
        {"pr", optional_argument, 0, PR_CAPS},
        {"daemon", optional_argument, 0, 'D'},
        {"help", no_argument, 0, 'h'},
        {0, no_argument, 0, 0}};

    char *ns_pid = NULL;
    bool cgroups = false;
    int uid = -1;
    int gid = -1;
    char *groups = NULL;
    bool retain_caps = false;
    char *context = NULL;
    int show_caps = -1;
    bool daemon = false;
    int port = -1;

    int opt = -1;
    bool get_opts = false;

    while ((opt = getopt_long(argc, argv, "g:u:D::h", long_opts, NULL)) != -1)
    {
        get_opts = true;
        switch (opt)
        {
        case NS:
            if (ns_pid != NULL)
                return dup_error("ns");
            if (!is_number(optarg, "ns"))
                return 1;
            ns_pid = optarg;
            break;
        case SET_CG:
            cgroups = true;
            break;
        case 'g':
            if (gid != -1)
                return dup_error("gid");
            if (!is_number(optarg, "gid"))
                return 1;
            gid = strtol(optarg, NULL, 10);
            break;
        case GROUPS:
            if (groups)
                return dup_error("groups");
            groups = optarg;
            break;
        case 'u':
            if (uid != -1)
                return dup_error("uid");
            if (!is_number(optarg, "uid"))
                return 1;
            uid = strtol(optarg, NULL, 10);
            break;
        case R_CAPS:
            retain_caps = true;
            break;
        case CXT:
            if (context != NULL)
                return dup_error("cxt");
            context = optarg;
            break;
        case PR_CAPS:
            show_caps = optarg == NULL ? 0 : 1;
            break;
        case 'D':
            daemon = true;
            if (optarg != NULL)
            {
                if (!is_number(optarg, "port"))
                    return 1;
                port = strtol(optarg, NULL, 10);
            }
            break;
        case 'h':
            show_usage();
            return 0;
        case '?':
            return show_usage();
        }
    }

    if (!get_opts)
    {
        print_err("no option provided");
        return show_usage();
    }

    /*
     * optind is the index of next remaining arg.
     * If we have reached the end of args but no action provided.
     */
    if (argc <= optind)
    {
        if (!daemon && show_caps == -1)
            return print_err("provide --daemon or --pr, or specify a program to execute");
    }
    else if (daemon)
        return print_err("--daemon and a program are mutually exclusive");

    if (daemon && show_caps != -1)
        return print_err("--daemon and --pr are mutually exclusive");

    if (daemon && geteuid() == 0)
    {
        ns_pid = "1";
        cgroups = retain_caps = true;
    }

    if (ns_pid)
        set_namespace(ns_pid);

    if (cgroups)
        set_cgroups();

    if (gid != -1)
        set_gid(gid);

    if (groups)
        set_groups(groups);

    if (retain_caps)
    {
        raise_eff_caps();
        raise_amb_caps();
        lock_caps();
    }

    if (uid != -1)
        set_uid(uid);

    if (context)
        set_context(context);

    if (daemon)
        return daemonize(argc, argv, port);

    if (show_caps != -1)
        print_caps(show_caps != 0);

    // If no prgram to run
    if (argc == optind)
        return 0;

    return exec_it(argv, optind);
}
