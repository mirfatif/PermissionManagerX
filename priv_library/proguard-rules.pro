# Required for de-Serialization, must not be obfuscated.
-keepclassmembers class com.mirfatif.err.HiddenAPIsException {
    static final long serialVersionUID;
}

-dontwarn android.app.ActivityManagerNative
-dontwarn android.app.AppOpsManager$OpEntry
-dontwarn android.app.AppOpsManager$PackageOps
-dontwarn android.app.IActivityManager
-dontwarn android.app.IActivityManager$Stub
-dontwarn android.app.IApplicationThread
-dontwarn android.app.ProfilerInfo
-dontwarn android.content.pm.IPackageManager
-dontwarn android.content.pm.IPackageManager$Stub
-dontwarn android.content.pm.ParceledListSlice
-dontwarn android.os.IDeviceIdleController
-dontwarn android.os.IDeviceIdleController$Stub
-dontwarn android.os.ServiceManager
-dontwarn android.permission.IPermissionManager
-dontwarn android.permission.IPermissionManager$Stub
-dontwarn com.android.internal.app.IAppOpsService
-dontwarn com.android.internal.app.IAppOpsService$Stub
