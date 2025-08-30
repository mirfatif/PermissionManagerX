# Preserve the line number information for debugging stack traces.
-keepattributes SourceFile,LineNumberTable

# Keep the line number information but hide the original source file name.
-renamesourcefileattribute SourceFile

# Move all obfuscated classes into the root package.
-repackageclasses
-allowaccessmodification

# Preference keys are accesses through reflection to reset filters and for backup / restore.
-keepclassmembers class com.mirfatif.permissionmanagerx.R$string {
  int pref_*_key;
}

# Throwable names must not be obfuscated to correctly print e.toString()
-keepnames class ** extends java.lang.Throwable

-dontwarn io.github.muntashirakon.adb.AdbProtocol$AuthType
-dontwarn jakarta.annotation.Nullable
