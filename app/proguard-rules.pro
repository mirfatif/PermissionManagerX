# Preserve the line number information for debugging stack traces.
-keepattributes SourceFile,LineNumberTable

# Keep the line number information but hide the original source file name.
-renamesourcefileattribute SourceFile

# Serializable classes must be identical. With R8 it's ignored
# if using -repackageclasses or -flattenpackagehierarchy.
#-applymapping ../priv_daemon/build/outputs/mapping/release/mapping.txt

# Move all obfuscated classes into the root package.
-repackageclasses
-allowaccessmodification

# Default preferences are accessed through Reflection
-keepclassmembers class com.mirfatif.permissionmanagerx.R$integer {
  int pref_*_default;
}
-keepclassmembers class com.mirfatif.permissionmanagerx.R$bool {
  int pref_*_default;
}
-keepclassmembers class com.mirfatif.permissionmanagerx.R$string {
  int pref_*_default;
}
# Used in BackupRestore to check valid permissions and in MySettings#resetToDefaults()
-keepclassmembers class com.mirfatif.permissionmanagerx.R$string {
  int pref_*_key;
}

# Permission icons are accessed through Reflection
-keepclassmembers class com.mirfatif.permissionmanagerx.R$drawable {
  int g_*;
}

# PreferenceFragments are instantiated from XML files
-keep class com.mirfatif.permissionmanagerx.prefs.settings.SettingsFrag* {
  void onCreatePreferences(android.os.Bundle, java.lang.String);
}

# Throwable names must not be obfuscated to correctly print e.toString()
-keepnames class com.mirfatif.** extends java.lang.Exception
-keepnames class com.mirfatif.** extends java.lang.Error
