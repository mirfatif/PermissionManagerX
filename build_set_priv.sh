#!/bin/sh -e

[ -n "$ANDROID_NDK" ]

BIN="$ANDROID_NDK"/toolchains/llvm/prebuilt/linux-x86_64/bin

cd "$(dirname "$0")"

[ "$1" = '-f' ] && FORCE=true || FORCE=false

ARM_SET_PRIV_FILE='app/src/main/assets/set_priv_arm'
X86_SET_PRIV_FILE='app/src/main/assets/set_priv_x86'

LIBCAP_DIR=$(realpath libcap/libcap)

API=24

ARM_TARGET=armv7a-linux-androideabi
X86_TARGET=i686-linux-android

ARM_CC="$BIN/${ARM_TARGET}${API}-clang"
X86_CC="$BIN/${X86_TARGET}${API}-clang"

alias clean_libcap="( cd $LIBCAP_DIR; make --debug=b clean )"

buildLibcap() (
	set -ex
	cd $LIBCAP_DIR
	CC=$1

	make --debug=b clean cap_names.h

	$CC \
		-Oz \
		-D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Dlinux \
		-fPIC \
		-I $LIBCAP_DIR/include -I $LIBCAP_DIR/include/uapi \
		-c cap_alloc.c cap_proc.c cap_extint.c cap_flag.c cap_text.c cap_file.c

	$BIN/llvm-ar rcs libcap.a cap_alloc.o cap_proc.o cap_extint.o cap_flag.o cap_text.o cap_file.o
)

build_set_priv() {
	CC=$1
	OUT_FILE=$2

	$CC -o $OUT_FILE \
		-Oz \
		-Wl,--gc-sections -ffunction-sections -fdata-sections \
		-fno-exceptions -fno-rtti \
		-Wl,-z,norelro -Wl,--hash-style=gnu \
		set_priv.c \
		-I $LIBCAP_DIR/include -Wl,-Bstatic -L $LIBCAP_DIR -lcap -Wl,-Bdynamic

	$BIN/llvm-strip $OUT_FILE \
		-s -S --strip-unneeded \
		-R=.eh_frame -R=.eh_frame_ptr \
		-R .note.android.ident -R .note.gnu.gold-version -R .comment -R .note \
		-R .note.gnu.build-id -R .note.gnu.property -R .note.ABI-tag
}

set -x

if $FORCE || [ ! -f $ARM_SET_PRIV_FILE ] || [ set_priv.c -nt $ARM_SET_PRIV_FILE ]; then
	buildLibcap $ARM_CC
	build_set_priv $ARM_CC $ARM_SET_PRIV_FILE
	clean_libcap
fi

if $FORCE || [ ! -f $X86_SET_PRIV_FILE ] || [ set_priv.c -nt $X86_SET_PRIV_FILE ]; then
	buildLibcap $X86_CC
	build_set_priv $X86_CC $X86_SET_PRIV_FILE
	clean_libcap
fi
