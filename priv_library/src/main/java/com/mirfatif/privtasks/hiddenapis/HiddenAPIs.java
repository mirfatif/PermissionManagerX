package com.mirfatif.privtasks.hiddenapis;

import android.app.ActivityManager;
import android.app.AppOpsManager;
import android.app.AppOpsManager.OpEntry;
import android.app.AppOpsManager.PackageOps;
import android.app.IActivityManager;
import android.content.pm.IPackageManager;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ParceledListSlice;
import android.content.pm.UserInfo;
import android.os.Build;
import android.os.IUserManager;
import android.os.Process;
import android.os.ServiceManager;
import android.permission.IPermissionManager;
import com.android.internal.app.IAppOpsService;
import com.mirfatif.privtasks.MyPackageOps;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenClass.CType;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenClass.HiddenClasses;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenField.FType;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenField.HiddenFields;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenMethod.HiddenMethods;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenMethod.MType;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.List;

public abstract class HiddenAPIs {

  final HiddenAPIsCallback mCallback;
  Integer OP_FLAGS_ALL = null;

  @HiddenClass(cls = ServiceManager.class)
  @HiddenClass(cls = IAppOpsService.class)
  @HiddenClass(cls = IPackageManager.class)
  @HiddenClass(cls = IPermissionManager.class)
  @HiddenClass(cls = IActivityManager.class)
  @HiddenClass(cls = IUserManager.class)
  @HiddenMethod(name = "getService", type = MType.STATIC_METHOD, cls = ServiceManager.class)
  @HiddenMethod(
      name = "asInterface",
      type = MType.STATIC_METHOD,
      cls = {
        IAppOpsService.Stub.class,
        IPackageManager.Stub.class,
        IPermissionManager.Stub.class,
        IActivityManager.Stub.class,
        IUserManager.Stub.class
      })
  // IPackageManager and IPermissionManager don't have a constant in Context class
  HiddenAPIs(HiddenAPIsCallback callback) {
    mCallback = callback;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
      try {
        OP_FLAGS_ALL = getOpFlagAll();
      } catch (HiddenAPIsError e) {
        e.printStackTrace();
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// APP OPS /////////////////////////////
  //////////////////////////////////////////////////////////////////

  @HiddenField(name = "_NUM_OP", type = FType.STATIC_FIELD, cls = AppOpsManager.class)
  public int getNumOps() throws HiddenAPIsError {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
      return _getNumOps();
    }
    // Using directly the value in compile-time SDK gets hard-coded
    return getStaticIntField("_NUM_OP", AppOpsManager.class);
  }

  @HiddenMethod(name = "getNumOps", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  abstract int _getNumOps() throws HiddenAPIsError;

  @HiddenField(name = "OP_NONE", type = FType.STATIC_FIELD, cls = AppOpsManager.class)
  public int getOpNone() throws HiddenAPIsError {
    // Using directly the value in compile-time SDK gets hard-coded
    return getStaticIntField("OP_NONE", AppOpsManager.class);
  }

  @HiddenField(
      name = "OP_FLAGS_ALL",
      type = FType.STATIC_FIELD,
      cls = AppOpsManager.class,
      minSDK = 29)
  public int getOpFlagAll() throws HiddenAPIsError {
    // Using directly the value in compile-time SDK gets hard-coded
    return getStaticIntField("OP_FLAGS_ALL", AppOpsManager.class);
  }

  @HiddenField(name = "MODE_NAMES", type = FType.STATIC_FIELD, cls = AppOpsManager.class)
  public abstract int getOpModeNamesSize() throws HiddenAPIsError;

  @HiddenMethod(name = "opToDefaultMode", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int opToDefaultMode(int opCode) throws HiddenAPIsError, HiddenAPIsException;

  @HiddenMethod(name = "opToSwitch", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int opToSwitch(int opCode) throws HiddenAPIsError, HiddenAPIsException;

  @HiddenMethod(name = "opToName", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract String opToName(int opCode) throws HiddenAPIsError, HiddenAPIsException;

  @HiddenMethod(name = "modeToName", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract String modeToName(int opMode) throws HiddenAPIsError;

  @HiddenMethod(name = "permissionToOpCode", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int permissionToOpCode(String permName);

  @DaemonOnly
  @HiddenMethod(name = "strDebugOpToOp", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int strDebugOpToOp(String opName) throws HiddenAPIsError;

  @HiddenMethod(name = "setMode", cls = IAppOpsService.class)
  @DaemonOnly
  @Privileged(requires = "android.permission.MANAGE_APP_OPS_MODES")
  @Throws(name = "SecurityException")
  // Profile owners are allowed to change modes but only for apps within their user.
  public abstract void setMode(int op, int uid, String pkgName, int mode)
      throws HiddenAPIsException;

  @HiddenMethod(name = "setUidMode", cls = IAppOpsService.class)
  @DaemonOnly
  @Privileged(requires = "android.permission.MANAGE_APP_OPS_MODES")
  @Throws(name = "SecurityException")
  // Profile owners are allowed to change modes but only for apps within their user.
  public abstract void setUidMode(int op, int uid, int mode) throws HiddenAPIsException;

  @HiddenMethod(name = "resetAllModes", cls = IAppOpsService.class)
  @DaemonOnly
  @Privileged(requires = "android.permission.MANAGE_APP_OPS_MODES")
  @Throws(name = "SecurityException")
  // Profile owners are allowed to change modes but only for apps within their user.
  public abstract void resetAllModes(int userId, String pkgName) throws HiddenAPIsException;

  @HiddenClass(cls = PackageOps.class, type = CType.INNER_CLASS)
  @HiddenClass(cls = OpEntry.class, type = CType.INNER_CLASS)
  @HiddenMethod(name = "getOpsForPackage", cls = IAppOpsService.class)
  @HiddenMethod(name = "getUidOps", cls = IAppOpsService.class)
  @HiddenMethod(name = "getPackageName", cls = PackageOps.class)
  @HiddenMethod(name = "getOps", cls = PackageOps.class)
  @HiddenMethod(name = "getOp", cls = OpEntry.class)
  @HiddenMethod(name = "getMode", cls = OpEntry.class)
  @HiddenMethod(name = "getLastAccessTime", cls = OpEntry.class)
  @HiddenMethod(name = "getTime", cls = OpEntry.class)
  @Privileged(requires = "android.permission.GET_APP_OPS_STATS")
  @Throws(name = "SecurityException")
  /*
   getUidOps() (on O and P) is buggy, throws NullPointerException. Check was added in Q:
     android-10.0.0_r1: frameworks/base/services/core/java/com/android/server/appop/AppOpsService.java#1016
   But don't consider it an error, it just means there are no UID AppOps for the package.
   MIUI has bug and returns bad opCode like 10005, so compare with valid range.
   N and O (P too?) don't have getLastAccessTime(), so use deprecated getTime().
   Returning null is considered an error, so return empty List if no error.
  */
  public abstract List<MyPackageOps> getMyPackageOpsList(
      int uid, String packageName, String op, int opNum)
      throws HiddenAPIsException, HiddenAPIsError;

  //////////////////////////////////////////////////////////////////
  ////////////////////// MANIFEST PERMISSIONS //////////////////////
  //////////////////////////////////////////////////////////////////

  @HiddenField(
      name = "FLAG_PERMISSION_SYSTEM_FIXED",
      type = FType.STATIC_FIELD,
      cls = PackageManager.class)
  public static int getSystemFixedFlag() throws HiddenAPIsError {
    return getStaticIntField("FLAG_PERMISSION_SYSTEM_FIXED", PackageManager.class);
  }

  @HiddenField(
      name = "FLAG_PERMISSION_POLICY_FIXED",
      type = FType.STATIC_FIELD,
      cls = PackageManager.class)
  public static int getPolicyFixedFlag() throws HiddenAPIsError {
    return getStaticIntField("FLAG_PERMISSION_POLICY_FIXED", PackageManager.class);
  }

  @HiddenClass(cls = ParceledListSlice.class)
  @HiddenMethod(
      name = "getAllPermissionGroups",
      cls = {IPackageManager.class, IPermissionManager.class})
  @HiddenMethod(name = "getList", cls = ParceledListSlice.class)
  // getAllPermissionGroups() moved from IPackageManager to IPermissionManager in SDK 30.
  public abstract List<?> getPermGroupInfoList() throws HiddenAPIsException, HiddenAPIsError;

  @HiddenClass(cls = ParceledListSlice.class)
  @HiddenMethod(
      name = "queryPermissionsByGroup",
      cls = {IPackageManager.class, IPermissionManager.class})
  @HiddenMethod(name = "getList", cls = ParceledListSlice.class)
  // queryPermissionsByGroup() moved from IPackageManager to IPermissionManager in SDK 30.
  public abstract List<?> getPermInfoList(String permGroup)
      throws HiddenAPIsException, HiddenAPIsError;

  @HiddenMethod(
      name = "getPermissionFlags",
      cls = {IPackageManager.class, IPermissionManager.class})
  @DaemonOnly
  @Privileged(
      requires = {"android.permission.GRANT_RUNTIME_PERMISSIONS", "REVOKE_RUNTIME_PERMISSIONS"})
  @Throws(name = "SecurityException")
  // getPermissionFlags() moved from IPackageManager to IPermissionManager in SDK 30.
  public abstract int getPermissionFlags(String permName, String pkgName, int userId)
      throws HiddenAPIsException;

  @HiddenMethod(name = "grantRuntimePermission", cls = IPackageManager.class)
  @DaemonOnly
  @Privileged(requires = "android.permission.GRANT_RUNTIME_PERMISSIONS")
  @Throws(name = "SecurityException")
  public abstract void grantRuntimePermission(String pkgName, String permName, int userId)
      throws HiddenAPIsException;

  @HiddenMethod(
      name = "revokeRuntimePermission",
      cls = {IPackageManager.class, IPermissionManager.class})
  @DaemonOnly
  @Privileged(requires = "android.permission.REVOKE_RUNTIME_PERMISSIONS")
  @Throws(name = "SecurityException")
  // revokeRuntimePermission() moved from IPackageManager to IPermissionManager in SDK 30.
  public abstract void revokeRuntimePermission(String pkgName, String permName, int userId)
      throws HiddenAPIsException;

  @HiddenMethod(
      name = "updatePermissionFlags(String, String, int, int, boolean, int)",
      cls = IPermissionManager.class,
      minSDK = 30)
  @HiddenMethod(
      name = "updatePermissionFlags(String, String, int, int, boolean, int)",
      cls = IPackageManager.class,
      minSDK = 29,
      maxSDK = 29)
  @HiddenMethod(
      name = "updatePermissionFlags(String, String, int, int, int)",
      cls = IPackageManager.class,
      maxSDK = 28)
  @DaemonOnly
  @Privileged(
      requires = {
        "android.permission.REVOKE_RUNTIME_PERMISSIONS",
        "android.permission.GRANT_RUNTIME_PERMISSIONS"
      })
  @Throws(name = "SecurityException")
  public abstract void updatePermFlags(
      String pkg, String perm, int flags, int flagValues, int userId) throws HiddenAPIsException;

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PACKAGES ////////////////////////////
  //////////////////////////////////////////////////////////////////

  @HiddenMethod(name = "setApplicationEnabledSetting", cls = IPackageManager.class)
  @DaemonOnly
  @Privileged(
      requires = {
        "android.permission.CHANGE_COMPONENT_ENABLED_STATE",
        "android.permission.INTERACT_ACROSS_USERS"
      })
  @Throws(name = "SecurityException")
  public abstract void setApplicationEnabledSetting(
      String pkg, int state, int flags, int userId, String callingPkg) throws HiddenAPIsException;

  @HiddenMethod(name = "getInstalledPackages", cls = IPackageManager.class)
  @DaemonOnly
  @Privileged(requires = "android.permission.INTERACT_ACROSS_USERS")
  @Throws(name = "SecurityException")
  public abstract List<?> getInstalledPackages(int flags, int userId) throws HiddenAPIsException;

  @HiddenMethod(name = "getPackageInfo", cls = IPackageManager.class)
  @DaemonOnly
  @Privileged(requires = "android.permission.INTERACT_ACROSS_USERS")
  @Throws(name = "SecurityException")
  public abstract PackageInfo getPkgInfo(String pkgName, int flags, int userId)
      throws HiddenAPIsException;

  //////////////////////////////////////////////////////////////////
  ////////////////////////////// OTHERS ////////////////////////////
  //////////////////////////////////////////////////////////////////

  @HiddenMethod(name = "getPidsForCommands", type = MType.STATIC_METHOD, cls = Process.class)
  @DaemonOnly
  public abstract int[] getPidsForCommands(String[] commands);

  @HiddenMethod(name = "startActivityAsUser", cls = IActivityManager.class)
  @DaemonOnly
  @Privileged
  @Throws(name = "SecurityException")
  public abstract int openAppInfo(String pkgName, int userId) throws HiddenAPIsException;

  @HiddenField(name = "START_SUCCESS", type = FType.STATIC_FIELD, cls = ActivityManager.class)
  @DaemonOnly
  public int getAmSuccessCode() throws HiddenAPIsError {
    return getStaticIntField("START_SUCCESS", ActivityManager.class);
  }

  @HiddenMethod(
      name =
          "ComponentName startService(IApplicationThread, Intent, String, boolean, String, String, int)",
      cls = IActivityManager.class,
      minSDK = 30)
  @HiddenMethod(
      name = "ComponentName startService(IApplicationThread, Intent, String, boolean, String, int)",
      cls = IActivityManager.class,
      minSDK = 26,
      maxSDK = 29)
  @HiddenMethod(
      name = "ComponentName startService(IApplicationThread, Intent, String, String, int)",
      cls = IActivityManager.class,
      maxSDK = 25)
  @DaemonOnly
  @Privileged
  @Throws(name = "SecurityException")
  public abstract void sendRequest(String command, int userId, String codeWord)
      throws HiddenAPIsException;

  @HiddenClass(cls = UserInfo.class)
  @HiddenField(name = "id", type = FType.STATIC_FIELD, cls = UserInfo.class)
  @HiddenField(name = "name", type = FType.STATIC_FIELD, cls = UserInfo.class)
  @HiddenMethod(name = "getUsers(boolean)", cls = IUserManager.class, maxSDK = 29)
  @HiddenMethod(name = "getUsers(boolean, boolean, boolean)", cls = IUserManager.class, minSDK = 29)
  @DaemonOnly
  @Privileged(requires = {"android.permission.MANAGE_USERS", "android.permission.CREATE_USERS"})
  @Throws(name = "SecurityException")
  // getUsers(boolean, boolean, boolean) was added in Android-10.0.0_r30, so cannot rely on SDK_INT
  public abstract List<String> getUsers() throws HiddenAPIsException;

  //////////////////////////////////////////////////////////////////
  ///////////////////////// COMMON METHODS /////////////////////////
  //////////////////////////////////////////////////////////////////

  public static int getStaticIntField(String name, Class<?> cls) throws HiddenAPIsError {
    try {
      return cls.getDeclaredField(name).getInt(null);
    } catch (IllegalAccessException | NoSuchFieldException e) {
      throw new HiddenAPIsError(e);
    }
  }

  @NonDaemonOnly
  public abstract boolean canUseIAppOpsService();

  @NonDaemonOnly
  public abstract boolean canUseIPm();

  public interface HiddenAPIsCallback {

    void onGetUidOpsNpException(Exception e);

    void onInvalidOpCode(int opCode, String pkgName);

    void logError(String msg);
  }

  //////////////////////////////////////////////////////////////////
  /////////////////////////// ANNOTATIONS //////////////////////////
  //////////////////////////////////////////////////////////////////

  @Retention(RetentionPolicy.SOURCE)
  @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
  @Repeatable(HiddenClasses.class)
  @interface HiddenClass {

    Class<?> cls();

    CType type() default CType.CLASS;

    enum CType {
      CLASS,
      INNER_CLASS
    }

    @Retention(RetentionPolicy.SOURCE)
    @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
    @interface HiddenClasses {

      HiddenClass[] value();
    }
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
  @Repeatable(HiddenMethods.class)
  @interface HiddenMethod {

    String name();

    MType type() default MType.METHOD;

    Class<?>[] cls();

    int minSDK() default 1;

    int maxSDK() default 1;

    enum MType {
      METHOD,
      STATIC_METHOD
    }

    @Retention(RetentionPolicy.SOURCE)
    @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
    @interface HiddenMethods {

      HiddenMethod[] value();
    }
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @Repeatable(HiddenFields.class)
  @interface HiddenField {

    String[] name();

    FType type() default FType.FIELD;

    Class<?> cls();

    int minSDK() default 1;

    enum FType {
      FIELD,
      STATIC_FIELD
    }

    @Retention(RetentionPolicy.SOURCE)
    @Target(ElementType.METHOD)
    @interface HiddenFields {

      HiddenField[] value();
    }
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @interface Throws {

    String name();
  }

  // Mostly permission checks regard UIDs: 0 and 1000, some 2000 too. On failed check usually a
  // SecurityException is thrown. E.g. ADB lacks permissions on MIUI.
  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @interface Privileged {

    String name() default "";

    String[] requires() default "";
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @interface DaemonOnly {}

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @interface NonDaemonOnly {}
}
