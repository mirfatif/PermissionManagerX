#!/bin/sh -ex

# must be set in order to get CLASSPATH and to use 'dx' tool
[ -n "$ANDROID_SDK" ]

BCP="$ANDROID_SDK/platforms/android-29/android.jar"
LAMBDA="$ANDROID_SDK/build-tools/30.0.2/core-lambda-stubs.jar"

# 'java' and 'javac' must be on PATH
[ -z "$JAVA_HOME" ] || export PATH=${JAVA_HOME}/bin:$PATH

MY_DIR=$(realpath "$(dirname "$0")")

cd "$MY_DIR/app/src/main/java/com/mirfatif/privdaemon"

SOURCE=*.java
DEX=$MY_DIR/app/src/main/assets/daemon.dex

if [ -f "$DEX" ]; then
	for file in $SOURCE; do
		if [ "$file" -nt "$DEX" ]; then
			BUILD=Y
			break
		fi
	done
	[ "$BUILD" ] || exit 0
fi

#LIBS=:$MY_DIR/app/libs/*.jar

rm -rf build
mkdir -p build
javac -g -deprecation -source 1.8 -target 1.8 -d build \
	-bootclasspath "${BCP}:${LAMBDA}" -classpath "$MY_DIR/app/src/main/java${LIBS}" *.java

#cd $MY_DIR/app/build/intermediates/javac/debug/classes
#cd $MY_DIR/app/build/intermediates/javac/release/classes

rm -f "$DEX"

#"$ANDROID_SDK"/build-tools/30.0.2/dx --min-sdk-version=24 --verbose --dex --output="$DEX" build/

"$ANDROID_SDK"/build-tools/30.0.2/d8 build/com/mirfatif/privdaemon/*.class \
	--lib "$ANDROID_SDK"/platforms/android-29/android.jar \
	--release --min-api 24 --output "$(dirname "$DEX")"
mv "$(dirname "$DEX")/classes.dex" "$DEX"

rm -rf build
