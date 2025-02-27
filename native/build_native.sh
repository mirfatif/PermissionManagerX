#!/bin/sh -e

[ -n "$ANDROID_NDK" ]
BIN="$ANDROID_NDK"/toolchains/llvm/prebuilt/linux-x86_64/bin

[ "$1" = '-f' ] && FORCE=true || FORCE=false

cd "$(dirname "$0")"

[ "$SRC_FILE" ] || SRC_FILE="pmxe.c"
SRC_FILES="$SRC_FILES $SRC_FILE"
[ "$OUT_DIR" ] || OUT_DIR='../app/src/main/jniLibs'
ARM_PMXE_LIB="$OUT_DIR/armeabi-v7a/libpmxe.so"
ARM64_PMXE_LIB="$OUT_DIR/arm64-v8a/libpmxe.so"
X86_PMXE_LIB="$OUT_DIR/x86/libpmxe.so"
X64_PMXE_LIB="$OUT_DIR/x86_64/libpmxe.so"

LIBCAP_DIR='libcap/libcap'
CAP_NAMES_H='cap_names.h'
LIBCAP_CFLAGS="-D _LARGEFILE64_SOURCE -D _FILE_OFFSET_BITS=64 -D linux -I $LIBCAP_DIR/include"
for file in cap_alloc.c cap_proc.c cap_flag.c cap_text.c; do
	LIBCAP_SRC_FILES="$LIBCAP_SRC_FILES $LIBCAP_DIR/$file"
done

PMXD_FILE='pmxd.c'
ARM_PMXD_LIB='../app/src/main/jniLibs/armeabi-v7a/libpmxd.so'
ARM64_PMXD_LIB='../app/src/main/jniLibs/arm64-v8a/libpmxd.so'
X86_PMXD_LIB='../app/src/main/jniLibs/x86/libpmxd.so'
X64_PMXD_LIB='../app/src/main/jniLibs/x86_64/libpmxd.so'

API=24

ARM_TARGET=armv7a-linux-androideabi
ARM64_TARGET=aarch64-linux-android
X86_TARGET=i686-linux-android
X64_TARGET=x86_64-linux-android

ARM_CC="$BIN/${ARM_TARGET}${API}-clang"
ARM64_CC="$BIN/${ARM64_TARGET}${API}-clang"
X86_CC="$BIN/${X86_TARGET}${API}-clang"
X64_CC="$BIN/${X64_TARGET}${API}-clang"

STRIP="$BIN/llvm-strip"

#VERBOSE='-v'; MAKE_DEBUG='--debug=b'

up_to_date() {
	! $FORCE || return 1

	OUT_FILE=$1
	[ -f "$OUT_FILE" ] || return 1

	for file in $SRC_FILES; do
		[ "$OUT_FILE" -nt "$file" ] || return 1
	done
}

trap 'cd "$LIBCAP_DIR"; [ ! -f "$CAP_NAMES_H" ] || make $MAKE_DEBUG clean' EXIT

build_libcap_names_h() (
	cd "$LIBCAP_DIR"
	[ ! -f "$CAP_NAMES_H" ] || return 0
	set -x
	make $MAKE_DEBUG $CAP_NAMES_H
)

build_pmx_bin() (
	CC=$1
	OUT_FILE=$2

	! up_to_date "$OUT_FILE" || return 0

	mkdir -p "$(dirname "$OUT_FILE")"

	build_libcap_names_h

	set -x

	$CC $VERBOSE -pie -o $OUT_FILE \
		-Oz -Wl,--gc-sections -ffunction-sections -fdata-sections -Wl,-x -Wl,-X \
		-fno-exceptions -fno-rtti -Wl,-z,norelro -Wl,--hash-style=gnu -D ANDROID -Wall \
		$CFLAGS $LIBCAP_CFLAGS $SRC_FILE $LIBCAP_SRC_FILES -llog

	$STRIP $OUT_FILE \
		-s -S --strip-unneeded -R=.eh_frame -R=.eh_frame_ptr -R .note.android.ident -R .comment \
		-R .note -R .note.gnu.gold-version -R .note.gnu.build-id -R .note.gnu.property -R .note.ABI-tag
)

build_pmx_lib() (
	CC=$1
	SRC_FILE=$2
	OUT_FILE=$3

	$FORCE || [ ! -f $OUT_FILE ] || [ $SRC_FILE -nt $OUT_FILE ] || return 0

	mkdir -p "$(dirname $OUT_FILE)"

	set -x

	$CC -o $OUT_FILE $CC_OPTS -shared -fvisibility=hidden $SRC_FILE -llog

	$STRIP $OUT_FILE $STRIP_OPTS
)

build_pmx_bin "$ARM_CC" "$ARM_PMXE_LIB"
build_pmx_bin "$ARM64_CC" "$ARM64_PMXE_LIB"
build_pmx_bin "$X86_CC" "$X86_PMXE_LIB"
build_pmx_bin "$X64_CC" "$X64_PMXE_LIB"

build_pmx_lib "$ARM_CC" "$PMXD_FILE" "$ARM_PMXD_LIB"
build_pmx_lib "$ARM64_CC" "$PMXD_FILE" "$ARM64_PMXD_LIB"
build_pmx_lib "$X86_CC" "$PMXD_FILE" "$X86_PMXD_LIB"
build_pmx_lib "$X64_CC" "$PMXD_FILE" "$X64_PMXD_LIB"
