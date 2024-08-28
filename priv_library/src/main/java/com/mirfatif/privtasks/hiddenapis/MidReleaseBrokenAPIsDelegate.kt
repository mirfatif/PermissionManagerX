package com.mirfatif.privtasks.hiddenapis

import android.content.Context
import android.os.Build
import com.mirfatif.privtasks.AppPrivTasks
import com.mirfatif.privtasks.HiddenSdkStringConstants

private const val TAG = "MidA14A15BrokenAPIsDelegate"

object MidReleaseBrokenAPIsDelegate {

  fun <T> delegateMidA14A15(
      cb: AppPrivTasks.AppPrivTasksCallback,
      method: String,
      preU: () -> T,
      preU29: () -> T,
      postU29: () -> T,
      postU38: () -> T
  ): T {
    while (true) {
      try {
        return delegateMidA14A15(preU, preU29, postU29, postU38)
      } catch (e: NoSuchMethodErrorCont) {
        cb.logErr(TAG, method, e.msg)
      }
    }
  }

  fun getPermFlags(
      permName: String,
      pkgName: String,
      userId: Int,
      cb: AppPrivTasks.AppPrivTasksCallback
  ): Int =
      delegateMidA14A15(
          cb,
          "getPermFlags",
          {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
              SysSvcFactory.INS.iPermMgr.getPermissionFlags(pkgName, permName, userId)
            } else if (Build.VERSION.SDK_INT == Build.VERSION_CODES.R) {
              SysSvcFactory.INS.iPermMgr.getPermissionFlags(permName, pkgName, userId)
            } else {
              SysSvcFactory.INS.iPkgMgr.getPermissionFlags(permName, pkgName, userId)
            }
          },
          { SysSvcFactory.INS.iPermMgr.getPermissionFlags(pkgName, permName, userId) },
          {
            SysSvcFactory.INS.iPermMgr.getPermissionFlags(
                pkgName, permName, Context.DEVICE_ID_DEFAULT, userId)
          },
          {
            SysSvcFactory.INS.iPermMgr.getPermissionFlags(
                pkgName,
                permName,
                HiddenSdkStringConstants.PERSISTENT_DEVICE_ID_DEFAULT.get(),
                userId)
          })

  fun grantRuntimePermission(
      pkgName: String,
      permName: String,
      userId: Int,
      cb: AppPrivTasks.AppPrivTasksCallback
  ) =
      delegateMidA14A15(
          cb,
          "grantRuntimePermission",
          { SysSvcFactory.INS.iPkgMgr.grantRuntimePermission(pkgName, permName, userId) },
          { SysSvcFactory.INS.iPermMgr.grantRuntimePermission(pkgName, permName, userId) },
          {
            SysSvcFactory.INS.iPermMgr.grantRuntimePermission(
                pkgName, permName, Context.DEVICE_ID_DEFAULT, userId)
          },
          {
            SysSvcFactory.INS.iPermMgr.grantRuntimePermission(
                pkgName,
                permName,
                HiddenSdkStringConstants.PERSISTENT_DEVICE_ID_DEFAULT.get(),
                userId)
          })

  fun revokeRuntimePermission(
      pkgName: String,
      permName: String,
      userId: Int,
      cb: AppPrivTasks.AppPrivTasksCallback
  ) =
      delegateMidA14A15(
          cb,
          "revokeRuntimePermission",
          {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
              SysSvcFactory.INS.iPermMgr.revokeRuntimePermission(pkgName, permName, userId, null)
            } else {
              SysSvcFactory.INS.iPkgMgr.revokeRuntimePermission(pkgName, permName, userId)
            }
          },
          { SysSvcFactory.INS.iPermMgr.revokeRuntimePermission(pkgName, permName, userId, null) },
          {
            SysSvcFactory.INS.iPermMgr.revokeRuntimePermission(
                pkgName, permName, Context.DEVICE_ID_DEFAULT, userId, null)
          },
          {
            SysSvcFactory.INS.iPermMgr.revokeRuntimePermission(
                pkgName,
                permName,
                HiddenSdkStringConstants.PERSISTENT_DEVICE_ID_DEFAULT.get(),
                userId,
                null)
          })
}

private var isU29Plus: Boolean? = null
private var isU38Plus: Boolean? = null

class NoSuchMethodErrorCont(val msg: String) : NoSuchMethodError()

private fun <T> delegateMidA14A15(
    preU: () -> T,
    preU29: () -> T,
    postU29: () -> T,
    postU38: () -> T
): T {
  return if (Build.VERSION.SDK_INT > Build.VERSION_CODES.UPSIDE_DOWN_CAKE || isU38Plus == true) {
    postU38()
  } else if (Build.VERSION.SDK_INT == Build.VERSION_CODES.UPSIDE_DOWN_CAKE) {
    if (isU29Plus == true) {
      if (isU38Plus == false) {
        postU29()
      } else {
        try {
          postU29().also { isU38Plus = false }
        } catch (e: NoSuchMethodError) {
          isU38Plus = true
          throw NoSuchMethodErrorCont(e.toString())
        }
      }
    } else if (isU29Plus == false) {
      preU29()
    } else {
      try {
        preU29().also { isU29Plus = false }
      } catch (e: NoSuchMethodError) {
        isU29Plus = true
        throw NoSuchMethodErrorCont(e.toString())
      }
    }
  } else {
    preU()
  }
}
