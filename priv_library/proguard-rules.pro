# All Serializable classes must be retained.
#-keep, allowobfuscation class com.mirfatif.privtasks.* implements java.io.Serializable { *; }

# Required for de-Serialization, must not be obfuscated.
#-keepclassmembers class com.mirfatif.privtasks.* implements java.io.Serializable {
#    static final long serialVersionUID;
#}

# With R8, -applymapping is ignored if used with -repackageclasses.
# Proguard produces bigger APK.
# So either do not use -repackageclasses, or do not use -applymapping
# i.e. do not obfuscate Serializable and its fields / methods.
-keep class com.mirfatif.privtasks.* implements java.io.Serializable { *; }

-dontwarn android.app.ActivityManagerNative
-dontwarn android.app.AppOpsManager$OpEntry
-dontwarn android.app.AppOpsManager$PackageOps
-dontwarn android.app.IActivityManager$Stub
-dontwarn android.app.IActivityManager
-dontwarn android.app.IApplicationThread
-dontwarn android.app.ProfilerInfo
-dontwarn android.content.pm.IPackageManager$Stub
-dontwarn android.content.pm.IPackageManager
-dontwarn android.content.pm.ParceledListSlice
-dontwarn android.os.ServiceManager
-dontwarn android.permission.IPermissionManager$Stub
-dontwarn android.permission.IPermissionManager
-dontwarn com.android.internal.app.IAppOpsService$Stub
-dontwarn com.android.internal.app.IAppOpsService
