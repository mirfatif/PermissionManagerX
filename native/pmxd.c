#include <stdbool.h>
#include <errno.h>

// close(), dup2(), STDERR_FILENO
#include <unistd.h>

// open()
#include <fcntl.h>

// snprintf()
#include <stdlib.h>

// strlen(), strerror()
#include <string.h>

// inet_addr(), htons(), connect(), setsockopt()
#include <arpa/inet.h>

// TCP_NODELAY
#include <netinet/tcp.h>

// For regex matching
#include <regex.h>

// Android logging
#include <android/log.h>

#include <jni.h>

////////////////////////////////////////////////////////////////////////////

#define JNI_TAG "JNI"

static int log_android_err(bool print_code, const char *tag, char *format, ...)
{
    char t[strlen(JNI_TAG) + strlen(tag) + 5];
    snprintf(t, sizeof(t), "%s: %s()", JNI_TAG, tag);

    va_list args;
    va_start(args, format);

    if (print_code)
    {
        char args_[256];
        vsnprintf(args_, sizeof(args_), format, args);
        __android_log_print(ANDROID_LOG_ERROR, t, "%s: %s", args_, strerror(errno));
    }
    else
        __android_log_vprint(ANDROID_LOG_ERROR, t, format, args);

    va_end(args);

    return 1;
}

////////////////////////////////////////////////////////////////////////////

#define LOCAL_HOST "127.0.0.1"

static bool send_stderr(int port)
{
    const char *tag = "send_stderr";
    struct sockaddr_in addr;

    int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd == -1)
    {
        log_android_err(true, tag, "Failed to create socket");
        return false;
    }

    int opt = 1;
    if (setsockopt(sock_fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(opt)))
        log_android_err(true, tag, "Failed to set socket option TCP_NODELAY");

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = inet_addr(LOCAL_HOST);

    int err = 0;
    if (connect(sock_fd, (struct sockaddr *)&addr, sizeof(addr)) == -1)
        err = log_android_err(true, tag, "Failed to connect to socket");
    else if (dup2(sock_fd, STDERR_FILENO) == -1)
        err = log_android_err(true, tag, "Failed to redirect stderr to socket");

    close(sock_fd);

    return err == 0;
}

JNIEXPORT jboolean JNICALL Java_com_mirfatif_privdaemon_Jni_sendStdErr(JNIEnv *env, jobject obj, int port)
{
    return send_stderr(port);
}

static bool close_stderr()
{
    int fd = open("/dev/null", O_WRONLY);

    if (fd < 0)
        return log_android_err(true, "close_stderr", "Failed to open /dev/null") == 0;

    int err = 0;
    if (dup2(fd, STDERR_FILENO) == -1)
        err = log_android_err(true, "close_stderr", "Failed to redirect stderr to /dev/null");

    close(fd);
    return err == 0;
}

JNIEXPORT jboolean JNICALL Java_com_mirfatif_privdaemon_Jni_closeStdErr(JNIEnv *env, jobject obj)
{
    return close_stderr();
}

////////////////////////////////////////////////////////////////////////////

static bool matches(const char *str, const char *reg, const char *tag)
{
    regex_t regex;

    int err = regcomp(&regex, reg, REG_EXTENDED | REG_NOSUB | REG_NEWLINE);
    if (err)
    {
        char errbuf[256];
        errbuf[0] = '0';
        regerror(err, &regex, errbuf, sizeof(errbuf));
        log_android_err(false, tag, "matches(): Failed to compile regex for '%s': %s (%d)", reg, errbuf, err);
        return false;
    }

    bool res = regexec(&regex, str, 0, NULL, 0) == 0;
    regfree(&regex);
    return res;
}

JNIEXPORT jboolean JNICALL Java_com_mirfatif_privdaemon_Jni_matches(JNIEnv *env, jobject obj, jstring string, jstring regex, jstring log_tag)
{
    const char *str = (*env)->GetStringUTFChars(env, string, NULL);
    const char *reg = (*env)->GetStringUTFChars(env, regex, NULL);
    const char *tag = (*env)->GetStringUTFChars(env, log_tag, NULL);

    if (!str || !reg || !tag)
    {
        log_android_err(false, "matches", "Bad parameters: string: %s, regex: %s, tag: %s", str, reg, tag);
        return false;
    }

    bool res = matches(str, reg, tag);

    (*env)->ReleaseStringUTFChars(env, string, str);
    (*env)->ReleaseStringUTFChars(env, regex, reg);
    (*env)->ReleaseStringUTFChars(env, log_tag, tag);

    return res;
}
