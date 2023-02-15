package com.mirfatif.privtasks.hiddenapis;

import android.app.ActivityManagerNative;
import android.app.IActivityManager;
import android.content.Context;
import android.content.pm.IPackageManager;
import android.os.Build;
import android.os.IDeviceIdleController;
import android.os.ServiceManager;
import android.permission.IPermissionManager;
import com.android.internal.app.IAppOpsService;
import com.mirfatif.err.HiddenAPIsException;

enum SysSvcFactory {
  INS;

  private IAppOpsService mIAppOpsSvc;

  IAppOpsService getIAppOpsSvc() throws HiddenAPIsException {
    try {
      if (mIAppOpsSvc == null) {
        mIAppOpsSvc =
            IAppOpsService.Stub.asInterface(ServiceManager.getService(Context.APP_OPS_SERVICE));
      }
    } catch (NoSuchMethodError e) {
      throw new HiddenAPIsException(e);
    }

    return mIAppOpsSvc;
  }

  private IPackageManager mPkgMgr;

  IPackageManager getIPkgMgr() throws HiddenAPIsException {
    try {
      if (mPkgMgr == null) {

        mPkgMgr = IPackageManager.Stub.asInterface(ServiceManager.getService("package"));
      }
    } catch (NoSuchMethodError e) {
      throw new HiddenAPIsException(e);
    }

    return mPkgMgr;
  }

  private IPermissionManager mIPermMgr;

  IPermissionManager getIPermMgr() throws HiddenAPIsException {
    try {
      if (mIPermMgr == null && Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
        mIPermMgr = IPermissionManager.Stub.asInterface(ServiceManager.getService("permissionmgr"));
      }
    } catch (NoSuchMethodError e) {
      throw new HiddenAPIsException(e);
    }

    return mIPermMgr;
  }

  private IActivityManager mIActMgr;

  IActivityManager getIActMgr() throws HiddenAPIsException {
    try {
      if (mIActMgr == null) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {

          mIActMgr =
              IActivityManager.Stub.asInterface(
                  ServiceManager.getService(Context.ACTIVITY_SERVICE));
        } else {
          mIActMgr = ActivityManagerNative.getDefault();
        }
      }
    } catch (NoSuchMethodError e) {
      throw new HiddenAPIsException(e);
    }

    return mIActMgr;
  }

  private IDeviceIdleController mIDeviceIdleController;

  IDeviceIdleController getIDevIdleController() throws HiddenAPIsException {
    try {
      if (mIDeviceIdleController == null) {
        mIDeviceIdleController =
            IDeviceIdleController.Stub.asInterface(
                ServiceManager.getService(Context.DEVICE_IDLE_CONTROLLER));
      }
    } catch (NoSuchMethodError e) {
      throw new HiddenAPIsException(e);
    }

    return mIDeviceIdleController;
  }
}
