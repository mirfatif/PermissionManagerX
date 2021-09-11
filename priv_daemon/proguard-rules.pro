# Preserve the line number information for debugging stack traces.
-keepattributes SourceFile,LineNumberTable

# Keep the line number information but hide the original source file name.
-renamesourcefileattribute SourceFile

# Move all obfuscated classes into the root package.
-repackageclasses
-allowaccessmodification

# VM entry point must be kept.
-keep class com.mirfatif.privdaemon.PrivDaemon {
  public static void main(java.lang.String[]);
}

# Throwable names must not be obfuscated to correctly print e.toString()
-keepnames class com.mirfatif.** extends java.lang.Exception
-keepnames class com.mirfatif.** extends java.lang.Error
