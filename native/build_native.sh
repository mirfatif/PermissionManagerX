#!/bin/sh -e

[ -n "$ANDROID_NDK" ]
BIN="$ANDROID_NDK"/toolchains/llvm/prebuilt/linux-x86_64/bin

cd "$(dirname "$0")"

[ "$1" = '-f' ] && FORCE=true || FORCE=false

PMX_FILE='pmx.c'
ARM_PMX_FILE='../app/src/main/jniLibs/armeabi-v7a/libpmxe.so'
ARM64_PMX_FILE='../app/src/main/jniLibs/arm64-v8a/libpmxe.so'
X86_PMX_FILE='../app/src/main/jniLibs/x86/libpmxe.so'
X64_PMX_FILE='../app/src/main/jniLibs/x86_64/libpmxe.so'

LIBCAP_DIR=$(realpath libcap/libcap)

API=24

ARM_TARGET=armv7a-linux-androideabi
ARM_LINKER_TARGET=arm-linux-androideabi
ARM64_TARGET=aarch64-linux-android
X86_TARGET=i686-linux-android
X64_TARGET=x86_64-linux-android

#VERBOSE='-v'

ARM_CC="$BIN/${ARM_TARGET}${API}-clang $VERBOSE"
ARM64_CC="$BIN/${ARM64_TARGET}${API}-clang $VERBOSE"
X86_CC="$BIN/${X86_TARGET}${API}-clang $VERBOSE"
X64_CC="$BIN/${X64_TARGET}${API}-clang $VERBOSE"

STRIP=$BIN/llvm-strip
AR=$BIN/llvm-ar

CC_OPTS="-Oz -Wl,--gc-sections -ffunction-sections -fdata-sections -Wl,-x -Wl,-X \
	-fno-exceptions -fno-rtti -Wl,-z,norelro -Wl,--hash-style=gnu -D ANDROID -Wall"
STRIP_OPTS="-s -S --strip-unneeded -R=.eh_frame -R=.eh_frame_ptr -R .note.android.ident -R .comment \
	-R .note -R .note.gnu.gold-version -R .note.gnu.build-id -R .note.gnu.property -R .note.ABI-tag"

build_libcap() (
	CC=$1

	cd $LIBCAP_DIR
	make --debug=b clean cap_names.h

	LIBCAP_MAKE_OPTS="
	    -Oz \
		-Wall \
	    -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Dlinux \
	    -fPIC \
	    -I $LIBCAP_DIR/include -I $LIBCAP_DIR/include/uapi"

	set -ex

	$CC $LIBCAP_MAKE_OPTS -c cap_alloc.c cap_proc.c cap_extint.c cap_flag.c cap_text.c cap_file.c
	$AR rcs libcap.a cap_alloc.o cap_proc.o cap_extint.o cap_flag.o cap_text.o cap_file.o
)

alias clean_libcap="( cd $LIBCAP_DIR; make --debug=b clean )"

build_pmx() (
	CC=$1
	OUT_FILE=$2

	$FORCE || [ ! -f $OUT_FILE ] || [ $PMX_FILE -nt $OUT_FILE ] || return 0

	mkdir -p $(dirname $OUT_FILE)

	set -ex

	build_libcap $CC

	$CC -pie -o $OUT_FILE $CC_OPTS $PMX_FILE \
		-I $LIBCAP_DIR/include -Wl,-Bstatic -L $LIBCAP_DIR -lcap -Wl,-Bdynamic -llog

	$STRIP $OUT_FILE $STRIP_OPTS

	clean_libcap
)

build_pmx $ARM_CC $ARM_PMX_FILE
build_pmx $ARM64_CC $ARM64_PMX_FILE
build_pmx $X86_CC $X86_PMX_FILE
build_pmx $X64_CC $X64_PMX_FILE
