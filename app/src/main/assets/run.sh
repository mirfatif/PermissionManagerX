#!/system/bin/sh

# ADB shell badly handles input stream with AdbLib.
# Perhaps because of some garbage appended with write()

# AdbLib doesn't return exit code
trap 'echo EXIT_CODE:$?' EXIT

if [ -z "$*" ]; then
  while read -r cmd; do $cmd; done
  read -r cmd
  set -- "$cmd"
fi

# shellcheck disable=SC2048
$*
