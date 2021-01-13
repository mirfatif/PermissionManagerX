# Add project specific ProGuard rules here.
# You can control the set of applied configuration files using the
# proguardFiles setting in build.gradle.
#
# For more details, see
#   http://developer.android.com/guide/developing/tools/proguard.html

# If your project uses WebView with JS, uncomment the following
# and specify the fully qualified class name to the JavaScript interface
# class:
#-keepclassmembers class fqcn.of.javascript.interface.for.webview {
#   public *;
#}

# Uncomment this to preserve the line number information for
# debugging stack traces.
-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
-renamesourcefileattribute SourceFile

# Because of serialzation
-keep class com.mirfatif.privtasks.MyPackageOps* { *; }

# Default preferences are accessed through Reflection
-keep class com.mirfatif.permissionmanagerx.R$integer {
  int pref_*_default;
}
-keep class com.mirfatif.permissionmanagerx.R$bool {
  int pref_*_default;
}
-keep class com.mirfatif.permissionmanagerx.R$string {
  int pref_*_key;
}

# Permission icons are accessed through Reflection
-keep class com.mirfatif.permissionmanagerx.R$drawable {
  int g_*;
}

# PreferenceFragments are instantiated from XML files
-keep class com.mirfatif.permissionmanagerx.prefs.settings.SettingsFrag* {
   void onCreatePreferences(android.os.Bundle, java.lang.String);
}
