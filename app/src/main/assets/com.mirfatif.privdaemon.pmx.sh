#!/system/bin/sh
set -e

trap 'Error occurred in $0 >&2' EXIT

echo "My UID: $(id -u)"

read -r DEBUG APP_ID UID CONTEXT USER_ID CODE_WORD CLASSPATH PKG_NAME CLASS SUFFIX PATH ARGS

for f in DEBUG APP_ID UID CONTEXT USER_ID CODE_WORD CLASSPATH PKG_NAME CLASS SUFFIX PATH; do
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

  CMD="set_priv -u $UID -g $UID --groups 2000,3003,1015,1023,1078,9997"
  if [ -e /proc/$$/attr/current ] && [ "$CONTEXT" != "default" ]; then
    if $DEBUG; then
      echo "SELinux enabled, switching context to $CONTEXT"
    fi
    CMD="$CMD --context $CONTEXT"
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

exec $CMD app_process / --nice-name=${PKG_NAME}.${SUFFIX} ${PKG_NAME}.${CLASS} $DEBUG $APP_ID $USER_ID $CODE_WORD $ARGS
