package com.mirfatif.privtasks.hiddenapis;

import android.app.AppOpsManager;
import android.app.AppOpsManager.OpEntry;
import android.app.AppOpsManager.PackageOps;
import android.content.pm.IPackageManager;
import android.content.pm.PackageManager;
import android.content.pm.ParceledListSlice;
import android.os.Build;
import android.os.Process;
import android.os.ServiceManager;
import android.permission.IPermissionManager;
import com.android.internal.app.IAppOpsService;
import com.mirfatif.privtasks.MyPackageOps;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenClass.CType;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenClass.HiddenClasses;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenField.FType;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenMethod.HiddenMethods;
import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.HiddenMethod.MType;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.List;

public abstract class HiddenAPIs {

  Integer OP_FLAGS_ALL = null;

  @HiddenClass(cls = ServiceManager.class)
  @HiddenClass(cls = IAppOpsService.class)
  @HiddenClass(cls = IPackageManager.class)
  @HiddenClass(cls = IPermissionManager.class)
  @HiddenMethod(name = "getService", type = MType.STATIC_METHOD, cls = ServiceManager.class)
  @HiddenMethod(
      name = "asInterface",
      type = MType.STATIC_METHOD,
      cls = {IAppOpsService.Stub.class, IPackageManager.Stub.class, IPermissionManager.Stub.class})
  // IPackageManager and IPermissionManager don't have a constant in Context class
  HiddenAPIs() {
    try {
      OP_FLAGS_ALL = getOpFlagAll();
    } catch (HiddenAPIsError e) {
      e.printStackTrace();
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

  @HiddenField(name = "OP_FLAGS_ALL", type = FType.STATIC_FIELD, cls = AppOpsManager.class)
  public int getOpFlagAll() throws HiddenAPIsError {
    // Using directly the value in compile-time SDK gets hard-coded
    return getStaticIntField("OP_FLAGS_ALL", AppOpsManager.class);
  }

  @HiddenField(name = "MODE_NAMES", type = FType.STATIC_FIELD, cls = AppOpsManager.class)
  public abstract int getOpModeNamesSize() throws HiddenAPIsError;

  @HiddenMethod(name = "opToDefaultMode", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int opToDefaultMode(int opCode) throws HiddenAPIsError;

  @HiddenMethod(name = "opToSwitch", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int opToSwitch(int opCode) throws HiddenAPIsError;

  @HiddenMethod(name = "opToName", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract String opToName(int opCode) throws NoSuchMethodError;

  @HiddenMethod(name = "modeToName", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract String modeToName(int opMode) throws HiddenAPIsError;

  @HiddenMethod(name = "permissionToOpCode", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int permissionToOpCode(String permName);

  @Privileged
  @HiddenMethod(name = "strDebugOpToOp", type = MType.STATIC_METHOD, cls = AppOpsManager.class)
  public abstract int strDebugOpToOp(String opName) throws NoSuchMethodError;

  @HiddenMethod(name = "setMode", cls = IAppOpsService.class)
  @Privileged(requires = "android.permission.UPDATE_APP_OPS_STATS")
  @ThrowsSecurityException
  public abstract void setMode(int op, int uid, String pkgName, int mode)
      throws HiddenAPIsException;

  @HiddenMethod(name = "setUidMode", cls = IAppOpsService.class)
  @Privileged
  @ThrowsSecurityException
  public abstract void setUidMode(int op, int uid, int mode) throws HiddenAPIsException;

  @HiddenMethod(name = "resetAllModes", cls = IAppOpsService.class)
  @Privileged(requires = "android.permission.UPDATE_APP_OPS_STATS")
  @ThrowsSecurityException
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
  /*
   getUidOps() (on Android 8?) is buggy, throws NullPointerException
   MIUI has bug and returns bad opCode like 10005, so compare with valid range
   N and O (P too?) don't have getLastAccessTime(), so use deprecated getTime()
   Returning null is considered an error, so return empty List if no error
  */
  public abstract List<MyPackageOps> getMyPackageOpsList(
      int uid, String packageName, String op, int opNum, Callback callback)
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
  @Privileged(
      requires = {"android.permission.GRANT_RUNTIME_PERMISSIONS", "REVOKE_RUNTIME_PERMISSIONS"})
  @ThrowsSecurityException
  // getPermissionFlags() moved from IPackageManager to IPermissionManager in SDK 30.
  public abstract int getPermissionFlags(String permName, String pkgName, int userId)
      throws HiddenAPIsException;

  @HiddenMethod(name = "grantRuntimePermission", cls = IPackageManager.class)
  @Privileged(requires = "android.permission.GRANT_RUNTIME_PERMISSIONS")
  @ThrowsSecurityException
  public abstract void grantRuntimePermission(String pkgName, String permName, int userId)
      throws HiddenAPIsException;

  @HiddenMethod(
      name = "revokeRuntimePermission",
      cls = {IPackageManager.class, IPermissionManager.class})
  @Privileged(requires = "android.permission.REVOKE_RUNTIME_PERMISSIONS")
  @ThrowsSecurityException
  // revokeRuntimePermission() moved from IPackageManager to IPermissionManager in SDK 30.
  public abstract void revokeRuntimePermission(String pkgName, String permName, int userId)
      throws HiddenAPIsException;

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PACKAGES ////////////////////////////
  //////////////////////////////////////////////////////////////////

  @HiddenMethod(name = "setApplicationEnabledSetting", cls = IPackageManager.class)
  @Privileged
  public abstract void setApplicationEnabledSetting(
      String pkg, int state, int flags, int userId, String callingPkg) throws HiddenAPIsException;

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PROCESSES ///////////////////////////
  //////////////////////////////////////////////////////////////////

  @HiddenMethod(name = "getPidsForCommands", type = MType.STATIC_METHOD, cls = Process.class)
  @Privileged
  public abstract int[] getPidsForCommands(String[] commands);

  //////////////////////////////////////////////////////////////////
  ///////////////////////// COMMON METHODS /////////////////////////
  //////////////////////////////////////////////////////////////////

  static int getStaticIntField(String name, Class<?> cls) throws HiddenAPIsError {
    try {
      return cls.getDeclaredField(name).getInt(null);
    } catch (IllegalAccessException | NoSuchFieldException e) {
      throw new HiddenAPIsError(e);
    }
  }

  // non-Daemon-only
  public abstract boolean canUseIAppOpsService();

  // non-Daemon-only
  public abstract boolean canUseIPm();

  public interface Callback {

    void onGetUidOpsNpException(Exception e);

    void onInvalidOpCode(int opCode, String pkgName);
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
  @interface HiddenField {

    String name();

    FType type() default FType.FIELD;

    Class<?> cls();

    enum FType {
      FIELD,
      STATIC_FIELD
    }
  }

  // Some methods throw SecurityException if the calling UID/PID doesn't have required permissions
  //  e.g. ADB lacks these permissions on MIUI.
  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @interface ThrowsSecurityException {

    String name() default "";
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @interface Privileged {

    String name() default "";

    String[] requires() default "";
  }

  // https://developer.android.com/distribute/best-practices/develop/restrictions-non-sdk-interfaces#results-of-keeping-non-sdk
  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.TYPE)
  public @interface NonSDK {}
}
