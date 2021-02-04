-keepattributes SourceFile,LineNumberTable
-renamesourcefileattribute SourceFile

# Because of serialzation
-keep class com.mirfatif.privtasks.MyPackageOps* { *; }

# main method must be accessible to VM for start
-keep class com.mirfatif.privdaemon.PrivDaemon {
  public static void main(java.lang.String[]);
}

# Throwable names must not be obfuscated to correctly print e.toString()
-keepnames class com.mirfatif.privtasks.hiddenapis.HiddenAPIsError
-keepnames class com.mirfatif.privtasks.hiddenapis.HiddenAPIsException

# Obfuscate everything else, but don't optimize (remove)
-keep, allowobfuscation class * { *; }
