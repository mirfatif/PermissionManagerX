-keepattributes SourceFile,LineNumberTable
-renamesourcefileattribute SourceFile

# Because of serialzation
-keep class com.mirfatif.privdaemon.MyPackageOps* { *; }

# main method must be accessible to VM for start
-keep class com.mirfatif.privdaemon.PrivDaemon {
  public static void main(java.lang.String[]);
}

# Obfuscate everything else, but don't optimize (remove)
-keep, allowobfuscation class * { *; }
