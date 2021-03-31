// For setns(), getresuid(), getresgid(), setresuid(), setresgid()
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <errno.h>
#include <stdbool.h>

// stdout, stderr
#include <stdio.h>

// isdigit()
#include <ctype.h>

// strtol(), getenv()
#include <stdlib.h>

// basename()
#include <libgen.h>

// getopt_long() and 'option' struct
#include <getopt.h>

// read(), write(), close(), getgroups(), access(), execvp()
// getresuid(), getresgid(), setresuid(), setresgid()
#include <unistd.h>

// setns()
#include <sched.h>

// For open() and flock() flags
#include <sys/file.h>

// For capabilities
#include <sys/capability.h>

// prctl()
#include <sys/prctl.h>

// Secure bits
#include <linux/securebits.h>

/////////////////////////////////////////////////////////////////////////

// For ULONG_MAX on Ubuntu
#include <limits.h>

// For setgroups() on Ubuntu. Android have them in <unistd.h>.
#include <grp.h>

// On Ubuntu for:
// strlen(), strerror(), strtok(), strcpy(), strcmp(), strncpy(), strncmp()
#include <string.h>

/////////////////////////////////////////////////////////////////////////

char *myName = "set_priv";

int printErr(char *msg)
{
    fprintf(stderr, "%s: %s\n", myName, msg);
    return 1;
}

int printErrCode(char *msg, int err)
{
    fprintf(stderr, "%s: %s: %s\n", myName, msg, strerror(errno));
    return 1;
}

int dupError(char *str)
{
    fprintf(stderr, "%s: duplicate %s\n", myName, str);
    return 1;
}

bool isNumber(char *num, char *type)
{
    int len = strlen(num);
    if (!len)
        return false;

    for (int i = 0; i < len; i++)
    {
        if (!isdigit(num[i]))
        {
            fprintf(stderr, "%s: invalid %s: %s\n", myName, type, num);
            return false;
        }
    }

    return true;
}

int showUsage()
{
    fputs("\nUsage:\n ", stdout);
    fputs(myName, stdout);
    fputs(" <options> [-- <program> [<argument,...>]]\n\n", stdout);
    fputs("Options:\n", stdout);
    fputs(" -u, --uid <uid>           Set real and effective uid\n", stdout);
    fputs(" -g, --gid <gid>           Set real and effective gid\n", stdout);
    fputs(" --groups <group,...>      Set supplementary groups\n", stdout);
    fputs(" --ns <pid>                Enter namespaces of given process\n", stdout);
    fputs(" --context <context>       Switch SELinux context\n", stdout);
    fputs(" --rcaps 0|1|2             Try to raise/retain all capabilities\n", stdout);
    fputs("                           0: ignore errors, 1: print errors\n", stdout);
    fputs("                           2: print and exit on error\n", stdout);
    fputs(" --pr[=arg]                Print current privileges\n", stdout);
    fputs("                           Optional arg to print supported caps\n\n", stdout);
    return 1;
}

/////////////////////////////////////////////////////////////////////////

unsigned int last_cap = -1;

unsigned int get_last_cap()
{
    if (last_cap != -1)
        return last_cap;

    int fd = open("/proc/sys/kernel/cap_last_cap", O_RDONLY);
    if (fd < 0)
    {
        printErrCode("failed to open cap_last_cap", errno);
        last_cap = CAP_LAST_CAP;
    }
    else
    {
        char buf[8];
        int num = read(fd, buf, sizeof(buf));
        if (num < 0)
            printErrCode("failed to read cap_last_cap", errno);
        else if (num > 0)
        {
            last_cap = strtoul(buf, NULL, 10);
            if (last_cap == 0 || last_cap == ULONG_MAX)
            {
                printErr("failed to parse cap_last_cap");
                last_cap = CAP_LAST_CAP;
            }
        }
        close(fd);
    }

    if (last_cap != CAP_LAST_CAP)
    {
        fprintf(stderr, "%s: cap_last_cap(%i) and CAP_LAST_CAP(%i) differ", myName, last_cap, CAP_LAST_CAP);
        while (last_cap > CAP_LAST_CAP)
            last_cap--;
        fprintf(stderr, ", using %i\n", last_cap);
    }

    return last_cap;
}

/////////////////////////////////////////////////////////////////////////

// In C, 'const' qualifier does not create a compile-time constant. It merely designates
// that a run-time variable is read-only. So we need them to be here.
// May also define them as macros.
enum
{
    __DUMMY__, // Start from 1
    GROUPS,
    NS,
    CXT,
    R_CAPS,
    PR_CAPS
};

int main(int argc, char **argv)
{
    char *name = basename(argv[0]);
    if (strcmp(name, myName))
    {
        fprintf(stderr, "%s?\n", name);
        return 1;
    }

    if (argc == 1)
    {
        showUsage();
        return 0;
    }

    // https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Options.html
    static const struct option longOpts[] = {
        {"uid", required_argument, 0, 'u'},
        {"gid", required_argument, 0, 'g'},
        {"groups", required_argument, 0, GROUPS},
        {"ns", required_argument, 0, NS},
        {"context", required_argument, 0, CXT},
        {"rcaps", required_argument, 0, R_CAPS},
        {"pr", optional_argument, 0, PR_CAPS},
        {"help", no_argument, 0, 'h'},
        {0, no_argument, 0, 0}};

    uid_t uid = -1;
    gid_t gid = -1;
    char *groups = NULL;
    char *nsPid = NULL;
    char *context = NULL;
    int retainCaps = -1;
    int showCaps = -1;

    int opt = -1;
    bool getOpts = false;

    while ((opt = getopt_long(argc, argv, "u:g:h", longOpts, NULL)) != -1)
    {
        getOpts = true;
        switch (opt)
        {
        case 'u':
            if (uid != -1)
                return dupError("uid");
            if (!isNumber(optarg, "uid"))
                return 1;
            uid = strtol(optarg, NULL, 10);
            break;
        case 'g':
            if (gid != -1)
                return dupError("gid");
            if (!isNumber(optarg, "gid"))
                return 1;
            gid = strtol(optarg, NULL, 10);
            break;
        case GROUPS:
            if (groups != NULL)
                return dupError("groups");
            groups = optarg;
            break;
        case NS:
            if (nsPid != NULL)
                return dupError("ns");
            if (!isNumber(optarg, "ns"))
                return 1;
            nsPid = optarg;
            break;
        case CXT:
            if (context != NULL)
                return dupError("context");
            context = optarg;
            break;
        case R_CAPS:
            if (retainCaps != -1)
                return dupError("rcaps");
            if (!isNumber(optarg, "rcaps argument"))
                return 1;
            retainCaps = strtol(optarg, NULL, 10);
            if (retainCaps < 0 || retainCaps > 2)
            {
                fprintf(stderr, "%s: invalid rcaps argument: %i\n", myName, retainCaps);
                return 1;
            }
            break;
        case PR_CAPS:
            showCaps = optarg == NULL ? 0 : 1;
            break;
        case 'h':
            showUsage();
            return 0;
        case '?':
            return showUsage();
        }
    }

    if (!getOpts)
    {
        printErr("no option provided");
        return showUsage();
    }

    // optind is the index of next remaining arg.
    // If we have reached the end of args but no action provided.
    if (argc <= optind && showCaps == -1)
    {
        printErr("assuming --pr since no program is specified to execute");
        showCaps = 0;
    }

    if (nsPid != NULL)
    {
        char path[25];
        snprintf(path, sizeof(path), "/proc/%s/ns/mnt", nsPid);
        int fd = open(path, O_RDONLY);
        if (fd < 0)
            return printErrCode("failed to read namespaces", errno);
        if (setns(fd, 0))
        {
            close(fd);
            return printErrCode("failed to switch namespaces", errno);
        }
        close(fd);
    }

    if (gid != -1)
    {
        if (setresgid(gid, gid, gid))
            return printErrCode("failed to set gid", errno);

        if (setgroups(0, 0))
            return printErrCode("failed to clear groups", errno);
    }

    if (groups != NULL)
    {
        char _groups[strlen(groups)];
        strcpy(_groups, groups);

        int arraySize = 1;
        char *token = strtok(groups, ",");
        while ((token = strtok(0, ",")))
            arraySize++;

        token = strtok(_groups, ",");
        gid_t gidsArray[arraySize];
        for (int i = 0; i < arraySize; i++)
        {
            if (!isNumber(token, "gid"))
                return 1;
            gidsArray[i] = strtol(token, NULL, 10);
            token = strtok(0, ",");
        }

        if (setgroups(arraySize, gidsArray))
            return printErrCode("failed to set groups", errno);
    }

    if (retainCaps != -1)
    {
        // To raise an Ambient capability, it must be in Permitted and Inheritable sets.
        cap_t caps = cap_from_text("all+eip");
        if (caps == NULL)
        {
            if (retainCaps != 0)
                printErrCode("failed to interpret capabilities", errno);
            if (retainCaps == 2)
                return 1;
        }
        else if (cap_set_proc(caps))
        {
            if (retainCaps != 0)
                printErrCode("failed to raise capabilities", errno);
            if (retainCaps == 2)
                return 1;
        }
        cap_free(caps);

        // Only Ambient caps are transferred to Effective (and Permitted) sets of execve()'d program:
        // P'(ambient)   = (file is privileged) ? 0 : P(ambient)
        // P'(effective) = F(effective) ? P'(permitted) : P'(ambient)
        for (int i = 0; i <= get_last_cap(); i++)
            if (cap_set_ambient(i, CAP_SET))
            {
                if (retainCaps != 0)
                    fprintf(stderr, "%s: failed to raise ambient capability: %s: %s\n", myName, cap_to_name(i), strerror(errno));
                if (retainCaps == 2)
                    return 1;
            }

        // Retain Permitted set on UID change.
        // Another way is to set SECBIT_KEEP_CAPS here and raise Ambient set after changing UID.
        // But in that case Effective set is cleared in current thread.
        if (prctl(PR_SET_SECUREBITS, SECBIT_NO_SETUID_FIXUP, 0, 0, 0))
        {
            if (retainCaps != 0)
                printErrCode("failed to set no_setuid_fixup security bit", errno);
            if (retainCaps == 2)
                return 1;
        }

        // TODO avoid clearing E and P sets if a binary with file capabilities is execve()'d
    }

    if (uid != -1 && setresuid(uid, uid, uid))
        return printErrCode("failed to set uid", errno);

    if (context != NULL)
    {
        int fd = open("/proc/self/attr/current", O_WRONLY);
        if (fd < 0)
            return printErrCode("failed to switch context", errno);
        if (flock(fd, LOCK_EX) || write(fd, context, strlen(context)) < 0)
        {
            close(fd);
            return printErrCode("failed to switch context", errno);
        }
        close(fd);
    }

    if (showCaps != -1)
    {
        int last_cap = get_last_cap();

        fputs("\n", stdout);

        if (showCaps != 0)
        {
            fputs("All caps :", stdout);
            for (int i = 0; i <= last_cap; i++)
                fprintf(stdout, " %s(%i)", cap_to_name(i), i);
            fputs("\n\n", stdout);
        }

        // Effective, Inheritable and Permitted sets
        cap_t current = cap_get_proc();
        if (current == NULL)
            printErrCode("capabilities not available", errno);
        else
        {
            char *text = cap_to_text(current, NULL);
            if (text == NULL)
                printErrCode("failed to textualize capabilities", errno);
            else
                fprintf(stdout, "EIP caps    : %s\n", text);
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
        fputs("\n", stdout);

        fputs("Bounding set:", stdout);
        for (int i = 0; i <= last_cap; i++)
            if (cap_get_bound(i) == 1)
                fprintf(stdout, " %s(%i)", cap_to_name(i), i);
        fputs("\n\n", stdout);

        int bits = prctl(PR_GET_SECUREBITS, 0, 0, 0, 0);
        if (bits < 0)
            printErrCode("failed to get securebits", errno);
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
            fputs("\n", stdout);
        }

        int r = prctl(PR_GET_NO_NEW_PRIVS, 0, 0, 0, 0);
        if (r < 0)
            printErrCode("failed to get NO_NEW_PRIVS", errno);
        else
            fprintf(stdout, "no_new_privs: %s\n", (r == 1 ? "set" : "unset"));

        fputs("\n", stdout);

        uid_t ruid, euid, suid;
        if (getresuid(&ruid, &euid, &suid))
            printErrCode("failed to get uid", errno);
        else
            fprintf(stdout, "resuid: %i, %i, %i\n", ruid, euid, suid);

        gid_t rgid, egid, sgid;
        if (getresgid(&rgid, &egid, &sgid))
            printErrCode("failed to get gid", errno);
        else
            fprintf(stdout, "resgid: %i, %i, %i\n", rgid, egid, sgid);

        gid_t groups[100];
        int count = getgroups(100, groups);
        if (count < 0)
            printErrCode("failed to get supplementary groups", errno);
        else if (count > 0)
        {
            fputs("groups:", stdout);
            for (int i = 0; i < count; i++)
                fprintf(stdout, " %i", groups[i]);
            fputs("\n", stdout);
        }

        fputs("\n", stdout);
    }

    // If no prgram to run
    if (argc == optind)
        return 0;

    bool exists = access(argv[optind], F_OK) != -1;
    if (!exists)
    {
        // https://wiki.sei.cmu.edu/confluence/display/c/STR06-C.+Do+not+assume+that+strtok%28%29+leaves+the+parse+string+unchanged
        // malloc (dynamic allocation) is unnecessary for small allocations for small time: https://stackoverflow.com/a/10575576/9165920
        char *path = getenv("PATH");
        char pathCopy[strlen(path) + 1];
        strcpy(pathCopy, path);
        char *pathDir = strtok(pathCopy, ":");
        while (pathDir)
        {
            char file[200];
            snprintf(file, sizeof(file), "%s/%s", pathDir, argv[optind]);
            if (access(file, F_OK) != -1)
            {
                exists = true;
                break;
            }
            pathDir = strtok(0, ":");
        }
    }

    if (!exists)
    {
        fprintf(stderr, "%s: failed to execute %s: No such file\n", myName, argv[optind]);
        return 1;
    }

    execvp(argv[optind], argv + optind);
    fprintf(stderr, "%s: failed to execute %s: %s\n", myName, argv[optind], strerror(errno));
    return 1;
}
