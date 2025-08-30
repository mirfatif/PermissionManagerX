package com.mirfatif.permissionmanagerx.util;

import android.Manifest;
import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import androidx.browser.customtabs.CustomTabColorSchemeParams;
import androidx.browser.customtabs.CustomTabsIntent;
import androidx.browser.customtabs.CustomTabsService;
import androidx.fragment.app.Fragment;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.privtasks.Constants;
import java.util.List;

public class ApiUtils {

  private ApiUtils() {}

  public static String getString(int resId, Object... args) {
    return App.getCxt().getString(resId, args);
  }

  public static String getQtyString(int resId, int qty, Object... args) {
    return App.getCxt().getResources().getQuantityString(resId, qty, args);
  }

  public static int getInt(int resId) {
    return App.getCxt().getResources().getInteger(resId);
  }

  public static void sendMail(Activity activity, String body) {
    sendMail(activity, getString(R.string.app_name), body);
  }

  public static boolean sendMail(Activity activity, String subject, String body) {
    Intent emailIntent = new Intent(Intent.ACTION_SENDTO).setData(Uri.parse("mailto:"));
    emailIntent.putExtra(Intent.EXTRA_EMAIL, new String[] {getString(R.string.email_address)});
    emailIntent.putExtra(Intent.EXTRA_SUBJECT, subject);
    if (body != null) {
      emailIntent.putExtra(Intent.EXTRA_TEXT, body);
    }
    try {
      activity.startActivity(emailIntent);
      return true;
    } catch (ActivityNotFoundException e) {
      UiUtils.showToast(R.string.no_email_app_installed);
      return false;
    }
  }

  public static boolean openWebUrl(Activity activity, String url) {
    Intent intent = new Intent(CustomTabsService.ACTION_CUSTOM_TABS_CONNECTION);
    ResolveInfo customTabSvc = resolveService(intent, PackageManager.MATCH_ALL);

    if (customTabSvc == null) {
      try {
        activity.startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(url)));
        return true;
      } catch (ActivityNotFoundException e) {
        UiUtils.showToast(R.string.no_browser_installed);
        return false;
      }
    }

    CustomTabColorSchemeParams colorSchemeParams =
        new CustomTabColorSchemeParams.Builder()
            .setToolbarColor(UiUtils.getColor(activity, R.attr.accentTrans10Color))
            .build();

    CustomTabsIntent customTabsIntent =
        new CustomTabsIntent.Builder()
            .setShareState(CustomTabsIntent.SHARE_STATE_ON)
            .setDefaultColorSchemeParams(colorSchemeParams)
            .build();

    customTabsIntent.launchUrl(activity, Uri.parse(url));

    return true;
  }

  public static ApplicationInfo getMyAppInfo() {
    try {
      return getAppInfo(App.getCxt().getPackageName(), 0);
    } catch (PackageManager.NameNotFoundException e) {
      throw new RuntimeException(e);
    }
  }

  public static ApplicationInfo getAppInfo(String pkg, int flags)
      throws PackageManager.NameNotFoundException {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
      return App.getPm().getApplicationInfo(pkg, flags);
    } else {
      return App.getPm().getApplicationInfo(pkg, PackageManager.ApplicationInfoFlags.of(flags));
    }
  }

  public static PackageInfo getMyPkgInfo() {
    return getMyPkgInfo(0);
  }

  public static PackageInfo getMyPkgInfo(int flags) {
    try {
      return getPkgInfo(App.getCxt().getPackageName(), flags);
    } catch (PackageManager.NameNotFoundException e) {
      throw new RuntimeException(e);
    }
  }

  public static PackageInfo getPkgInfo(String pkg, int flags)
      throws PackageManager.NameNotFoundException {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
      return App.getPm().getPackageInfo(pkg, flags);
    } else {
      return App.getPm().getPackageInfo(pkg, PackageManager.PackageInfoFlags.of(flags));
    }
  }

  public static List<PackageInfo> getInstalledPackages(int flags) {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
      return App.getPm().getInstalledPackages(flags);
    } else {
      return App.getPm().getInstalledPackages(PackageManager.PackageInfoFlags.of(flags));
    }
  }

  public static ResolveInfo resolveActivity(Intent intent, int flags) {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
      return App.getPm().resolveActivity(intent, flags);
    } else {
      return App.getPm().resolveActivity(intent, PackageManager.ResolveInfoFlags.of(flags));
    }
  }

  public static ResolveInfo resolveService(Intent intent, int flags) {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
      return App.getPm().resolveService(intent, flags);
    } else {
      return App.getPm().resolveService(intent, PackageManager.ResolveInfoFlags.of(flags));
    }
  }

  public static void setTargetFragment(Fragment source, Fragment target) {
    source.setTargetFragment(target, 0);
  }

  public static boolean hasAppOpsPerm() {
    return hasPerm(Constants.PERM_GET_APP_OPS_STATS);
  }

  public static boolean hasNotifPerm() {
    return Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU
        || hasPerm(Manifest.permission.POST_NOTIFICATIONS);
  }

  public static boolean hasPerm(String perm) {
    return App.getCxt().checkSelfPermission(perm) == PackageManager.PERMISSION_GRANTED;
  }
}
