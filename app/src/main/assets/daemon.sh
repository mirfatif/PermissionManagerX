#!/system/bin/sh
set -e

trap 'Error occurred in $0 >&2' EXIT

echo "My UID: $(id -u)"

read -r DEBUG UID USER_ID CLASSPATH PKG_NAME CLASS PATH ARGS

for f in DEBUG UID USER_ID CLASSPATH PKG_NAME CLASS PATH; do
  var=$(eval echo '$'$f)
  if [ -z "$var" ]; then
    echo "Empty $f"
    exit 1
  fi
  echo "$f: $var"
done

[ -z "$ARGS" ] || echo "ARGS: $ARGS"

if [ "$(id -u)" -eq 0 ]; then
  grep ' cgroup ' /proc/self/mounts | cut -d' ' -f2 | while read -r cgroup; do
    if $DEBUG; then
      echo "Adding to root cgroup: $cgroup"
    fi
    printf '%s' $$ >"$cgroup/cgroup.procs"
  done

  CMD="set_priv -u $UID -g $UID --groups 1015,3003"
  if [ -e /proc/$$/attr/current ]; then
    if $DEBUG; then
      echo "SELinux enabled, switching context to u:r:shell:s0"
    fi
    CMD="$CMD --context u:r:shell:s0"
  fi
  if [ -e /proc/1/ns/mnt ]; then
    if $DEBUG; then
      echo "Switching mount namespace to that of PID 1"
    fi
    CMD="$CMD --ns 1"
  fi
  CMD="$CMD --"
fi

echo "Starting $PKG_NAME VM"

export PATH
export CLASSPATH

exec $CMD app_process / --nice-name=${PKG_NAME}.${USER_ID} ${PKG_NAME}.${CLASS} $DEBUG $ARGS
