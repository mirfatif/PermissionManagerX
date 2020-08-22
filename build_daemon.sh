#!/bin/sh -ex

[ -n "$ANDROID_SDK" ]
[ -n "$ANDROID_STUDIO" ]

BCP="$ANDROID_SDK/platforms/android-29/android.jar"

MY_DIR=$(realpath $(dirname $0))

cd $MY_DIR/app/src/main/java/com/mirfatif/privdaemon

SOURCE=*.java
DEX=$MY_DIR/app/src/main/assets/daemon.dex

if [ -f $DEX ]; then
	for file in $SOURCE; do
		if [ $file -nt $DEX ]; then
			BUILD=Y
			break
		fi
	done
	[ "$BUILD" ] || exit 0
fi

#LIBS=:$MY_DIR/app/libs/*.jar

rm -rf build
mkdir -p build
$ANDROID_STUDIO/jre/bin/javac -g -deprecation -source 1.8 -target 1.8 -d build \
	-bootclasspath $BCP -classpath $MY_DIR/app/src/main/java${LIBS} *.java

#cd $MY_DIR/app/build/intermediates/javac/debug/classes
#cd $MY_DIR/app/build/intermediates/javac/release/classes

rm -f $DEX

export PATH=$ANDROID_STUDIO/jre/bin:$PATH

$ANDROID_SDK/build-tools/30.0.2/dx --min-sdk-version=24 --verbose --dex --output=$DEX build/

rm -rf build
