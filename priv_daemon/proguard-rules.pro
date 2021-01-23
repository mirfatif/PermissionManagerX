-keepattributes SourceFile,LineNumberTable
-renamesourcefileattribute SourceFile

# Because of serialzation
-keep class com.mirfatif.privtasks.MyPackageOps* { *; }

# main method must be accessible to VM for start
-keep class com.mirfatif.privdaemon.PrivDaemon {
  public static void main(java.lang.String[]);
}

# AOSP Non-SKD interfaces
-keep class android.app** { *; }
-keep class android.content** { *; }
-keep class android.os** { *; }
-keep class android.permission** { *; }
-keep class com.android** { *; }

# Obfuscate everything else, but don't optimize (remove)
-keep, allowobfuscation class * { *; }
