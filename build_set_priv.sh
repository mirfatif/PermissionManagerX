#!/bin/sh -ex

[ -n "$ANDROID_NDK" ]

cd "$(dirname "$0")"

if [ -f app/src/main/assets/set_priv_arm -a -f app/src/main/assets/set_priv_x86 ]; then
	[ set_priv.cpp -nt app/src/main/assets/set_priv_arm ] || exit 0
fi

"$ANDROID_NDK"/toolchains/llvm/prebuilt/linux-x86_64/bin/armv7a-linux-androideabi24-clang++ \
	-Os -fno-exceptions -Wl,--gc-sections -Wl,-z,norelro -Wl,--hash-style=gnu \
	-static-libstdc++ set_priv.cpp -o app/src/main/assets/set_priv_arm

"$ANDROID_NDK"/toolchains/llvm/prebuilt/linux-x86_64/bin/arm-linux-androideabi-strip \
	-s -S --strip-unneeded \
	-R .note.android.ident -R .note.gnu.gold-version -R .comment -R .note -R .note.gnu.build-id \
	app/src/main/assets/set_priv_arm

"$ANDROID_NDK"/toolchains/llvm/prebuilt/linux-x86_64/bin/i686-linux-android24-clang++ \
	-Os -fno-exceptions -Wl,--gc-sections -Wl,-z,norelro -Wl,--hash-style=gnu \
	-static-libstdc++ set_priv.cpp -o app/src/main/assets/set_priv_x86

"$ANDROID_NDK"/toolchains/llvm/prebuilt/linux-x86_64/bin/i686-linux-android-strip \
	-s -S --strip-unneeded \
	-R .note.android.ident -R .note.gnu.gold-version -R .comment -R .note -R .note.gnu.build-id \
	app/src/main/assets/set_priv_x86

#if command -v sstrip >/dev/null; then
#	sstrip -z app/src/main/assets/set_priv_*
#fi

#if command -v upx >/dev/null; then
#	upx --ultra-brute app/src/main/assets/set_priv_*
#fi
