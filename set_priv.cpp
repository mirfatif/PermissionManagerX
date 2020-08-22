#include <iostream>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <fcntl.h>
#include <libgen.h>
#include <sys/file.h>

char* myName;

void showUsage() {
  std::cout << "\nUsage:\n " << myName
            << " [options] -- <program> [<argument,...>]\n\n"
               "Options:\n"
               " -u, --uid <uid>       Set real and effective uid\n"
               " -g, --gid <gid>       Set real and effective gid\n"
               " --groups <group,...>  Set supplementary groups\n"
               " --ns <pid>            Enter namespaces of given process\n"
               " --context <context>   Switch SELinux context\n\n";
}

void dupError(std::string str) {
  std::cerr << myName << ": duplicate " << str << "\n";
}

bool isNumber(std::string num, std::string type) {
  for (int i = 0; i < num.length(); i++) {
    if (!isdigit(num[i])) {
      std::cerr << myName << ": invalid " << type << ": " << num << "\n";
      return false;
    }
  }
  return true;
}

int main(int argc, char** argv) {
  myName = basename(argv[0]);

  if (argc == 1) {
    showUsage();
    return 0;
  }

  int opt = -1;
  uid_t uid = -1;
  gid_t gid = -1;
  char* groups = nullptr;
  char* pid = nullptr;
  char* context = nullptr;

  const option longOpts[] = {{"uid", required_argument, nullptr, 'u'},
                             {"gid", required_argument, nullptr, 'g'},
                             {"groups", required_argument, nullptr, '1'},
                             {"ns", required_argument, nullptr, '2'},
                             {"context", required_argument, nullptr, '3'},
                             {"help", no_argument, nullptr, 'h'},
                             {nullptr, no_argument, nullptr, 0}};

  while ((opt = getopt_long(argc, argv, "u:g:h", longOpts, nullptr)) != -1) {
    switch (opt) {
      case 'u':
        if (uid != -1) {
          dupError("uid");
          return 1;
        }
        if (!isNumber(optarg, "uid")) return 1;
        uid = std::stoi(optarg);
        break;
      case 'g':
        if (gid != -1) {
          dupError("gid");
          return 1;
        }
        if (!isNumber(optarg, "gid")) return 1;
        gid = std::stoi(optarg);
        break;
      case '1':
        if (groups != NULL) {
          dupError("groups");
          return 1;
        }
        groups = optarg;
        break;
      case '2':
        if (pid != NULL) {
          dupError("pid");
          return 1;
        }
        if (!isNumber(optarg, "pid")) return 1;
        pid = optarg;
        break;
      case '3':
        if (context != NULL) {
          dupError("context");
          return 1;
        }
        context = optarg;
        break;
      case 'h':
        showUsage();
        return 0;
      case '?':
        showUsage();
        return 1;
    }
  }

  if (argc <= optind) {
    std::cerr << myName << ": no program specified\n";
    showUsage();
    return 1;
  }

  if (pid != NULL) {
    std::string path("/proc/" + std::string(pid) + "/ns/mnt");
    int fd = open(path.c_str(), O_RDONLY);
    if (fd < 0) {
      std::cerr << "failed switching namespaces: " << strerror(errno) << "\n";
      return 1;
    }
    if (setns(fd, 0) != 0) {
      std::cerr << "failed switching namespaces: " << strerror(errno) << "\n";
      close(fd);
      return 1;
    }
    close(fd);
  }

  if (gid != -1) {
    if (setresgid(gid, gid, gid) != 0) {
      std::cerr << "failed setting gid: " << strerror(errno) << "\n";
      return 1;
    }

    if (setgroups(0, NULL) != 0) {
      std::cerr << "failed clearing groups: " << strerror(errno) << "\n";
      return 1;
    }
  }

  if (groups != NULL) {
    char _groups[strlen(groups)];
    strcpy(_groups, groups);

    int arraySize = 1;
    char* token = strtok(groups, ",");
    while ((token = strtok(NULL, ",")) != NULL) arraySize++;

    token = strtok(_groups, ",");
    gid_t gidsArray[arraySize];
    for (int i = 0; i < arraySize; i++) {
      if (!isNumber(token, "gid")) return 1;
      gidsArray[i] = std::stoi(token);
      token = strtok(NULL, ",");
    }

    if (setgroups(arraySize, gidsArray) != 0) {
      std::cerr << "failed setting groups: " << strerror(errno) << "\n";
      return 1;
    }
  }

  if (uid != -1 && setresuid(uid, uid, uid) != 0) {
    std::cerr << "failed setting uid: " << strerror(errno) << "\n";
    return 1;
  }

  if (context != NULL) {
    int fd = open("/proc/self/attr/current", O_WRONLY);
    if (fd < 0) {
      std::cerr << "failed switching context: " << strerror(errno) << "\n";
      return 1;
    }
    if (flock(fd, LOCK_EX) != 0) {
      std::cerr << "failed switching context: " << strerror(errno) << "\n";
      close(fd);
      return 1;
    }
    if (write(fd, context, strlen(context)) < 0) {
      std::cerr << "failed switching context: " << strerror(errno) << "\n";
      close(fd);
      return 1;
    }
    close(fd);
  }

  execvp(argv[optind], argv + optind);
  std::cerr << "failed to execute: " << argv[optind] << "\n";
  return 1;
}
