package com.mirfatif.permissionmanagerx.prefs;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.text.SpannableString;
import android.text.Spanned;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.SmallTextSpan;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public enum ExcFiltersData {
  INS;

  private static final String TAG = "ExcFiltersData";

  private final List<String> mCriticalApps =
      Arrays.asList(App.getRes().getStringArray(R.array.critical_apps));

  public boolean isCriticalApp(String packageName) {
    return mCriticalApps.contains(packageName);
  }

  private Set<String> mExcludedApps;
  private CharSequence[] mExcludedAppsLabels;

  public CharSequence[] getExcludedAppsLabels() {
    populateExcludedAppsList(false, false);
    return mExcludedAppsLabels;
  }

  public Set<String> getExcludedApps() {
    populateExcludedAppsList(false, false);
    return mExcludedApps;
  }

  public int getExcludedAppsCount() {
    return getExcludedApps().size();
  }

  public boolean isPkgExcluded(String packageName) {
    return MySettings.INS.manuallyExcludeApps() && getExcludedApps().contains(packageName);
  }

  public boolean canBeExcluded(Package pkg) {
    return MySettings.INS.getExcFiltersEnabled() && !getExcludedApps().contains(pkg.getName());
  }

  private final Object EXCLUDED_APPS_LOCK = new Object();

  private void populateExcludedAppsList(boolean update, boolean loadDefaults) {
    synchronized (EXCLUDED_APPS_LOCK) {
      if (!update && mExcludedAppsLabels != null) {
        return;
      }

      Set<String> savedExcApps = MySettings.INS.getSetPref(R.string.pref_filter_excluded_apps_key);
      Set<String> excApps = savedExcApps;
      if (savedExcApps == null || loadDefaults) {
        String[] defExcApps = App.getRes().getStringArray(R.array.excluded_apps);
        excApps = new HashSet<>(Arrays.asList(defExcApps));
      }

      List<Pkg> excAppsTmpList = new ArrayList<>();

      for (String pkgName : excApps) {
        String pkgLabel;
        try {
          int flags = PackageManager.MATCH_UNINSTALLED_PACKAGES;
          pkgLabel = ApiUtils.getAppInfo(pkgName, flags).loadLabel(App.getPm()).toString();
        } catch (PackageManager.NameNotFoundException e) {

          continue;
        }

        if (pkgLabel.equals(pkgName)) {
          excAppsTmpList.add(new Pkg(new SpannableString(pkgLabel), pkgName));
        } else {
          SpannableString str = new SpannableString(pkgLabel + "\n" + pkgName);
          str.setSpan(
              new SmallTextSpan(),
              pkgLabel.length(),
              str.length(),
              Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
          excAppsTmpList.add(new Pkg(str, pkgName));
        }
      }

      excAppsTmpList.sort(Comparator.comparing(o -> o.label.toString().toUpperCase()));

      CharSequence[] excAppsLabels = new CharSequence[excAppsTmpList.size()];

      mExcludedApps = Collections.synchronizedSet(new LinkedHashSet<>());

      for (int i = 0; i < excAppsTmpList.size(); i++) {
        excAppsLabels[i] = excAppsTmpList.get(i).label;
        mExcludedApps.add(excAppsTmpList.get(i).name);
      }

      mExcludedAppsLabels = excAppsLabels;

      if (savedExcApps == null || !savedExcApps.equals(mExcludedApps)) {
        MySettings.INS.saveExcludedList(R.string.pref_filter_excluded_apps_key, mExcludedApps);
      }
    }
  }

  private static class Pkg {

    private final SpannableString label;
    private final String name;

    Pkg(SpannableString label, String name) {
      this.label = label;
      this.name = name;
    }
  }

  private Set<String> mExcludedPerms;

  public Set<String> getExcludedPerms() {
    populateExcludedPermsList(false);
    return mExcludedPerms;
  }

  public int getExcludedPermsCount() {
    return getExcludedPerms().size();
  }

  public boolean isPermExcluded(String permissionName) {
    return MySettings.INS.manuallyExcludePerms() && getExcludedPerms().contains(permissionName);
  }

  public boolean canBeExcluded(Permission perm) {
    return MySettings.INS.getExcFiltersEnabled()
        && !perm.isExtraAppOp()
        && !getExcludedPerms().contains(perm.getName());
  }

  private final Object EXCLUDED_PERMS_LOCK = new Object();

  private void populateExcludedPermsList(boolean update) {
    synchronized (EXCLUDED_PERMS_LOCK) {
      if (!update && mExcludedPerms != null) {
        return;
      }

      Set<String> excludedPerms =
          MySettings.INS.getSetPref(R.string.pref_filter_excluded_perms_key);
      if (excludedPerms == null) {
        excludedPerms = new HashSet<>();
      }

      List<String> excludedPermsList = new ArrayList<>(excludedPerms);
      excludedPermsList.sort(Comparator.comparing(String::toUpperCase));

      mExcludedPerms = Collections.synchronizedSet(new LinkedHashSet<>(excludedPermsList));
    }
  }

  private Set<String> mExtraAppOps;

  public Set<String> getExtraAppOps() {
    populateExtraAppOpsList(false, false);
    return mExtraAppOps;
  }

  public int getExtraAppOpsCount() {
    return getExtraAppOps().size();
  }

  public boolean isExtraAppOp(String opName) {
    return MySettings.INS.showExtraAppOps() && getExtraAppOps().contains(opName);
  }

  private final Object EXTRA_APP_OPS_LOCK = new Object();

  public void populateExtraAppOpsList(boolean update, boolean loadDefaults) {
    synchronized (EXTRA_APP_OPS_LOCK) {
      if (!update && mExtraAppOps != null) {
        return;
      }

      Set<String> savedExtraAppOps =
          MySettings.INS.getSetPref(R.string.pref_filter_extra_appops_key);

      Set<String> extraAppOps;

      if (savedExtraAppOps == null || loadDefaults) {

        String[] defaultExtraAppOps =
            App.getCxt().getResources().getStringArray(R.array.extra_app_ops);
        extraAppOps = new HashSet<>(Arrays.asList(defaultExtraAppOps));
      } else {
        extraAppOps = savedExtraAppOps;
      }

      List<String> appOpsList = AppOpsParser.INS.getAppOpsNames();

      if (!appOpsList.isEmpty()) {

        extraAppOps.removeIf(appOp -> !appOpsList.contains(appOp));
      }

      mExtraAppOps = Collections.synchronizedSet(extraAppOps);

      if (savedExtraAppOps == null || !savedExtraAppOps.equals(mExtraAppOps)) {
        MySettings.INS.saveExcludedList(
            R.string.pref_filter_extra_appops_key, new HashSet<>(mExtraAppOps));
      }
    }
  }

  public void updateList(String key) {
    if (key.equals(getString(R.string.pref_filter_excluded_apps_key))) {
      populateExcludedAppsList(true, false);
    } else if (key.equals(getString(R.string.pref_filter_excluded_perms_key))) {
      populateExcludedPermsList(true);
    } else if (key.equals(getString(R.string.pref_filter_extra_appops_key))) {
      populateExtraAppOpsList(true, false);
    }
  }

  public void populateLists() {
    BgRunner.execute(() -> populateExcludedAppsList(false, false));
    BgRunner.execute(() -> populateExcludedPermsList(false));
    BgRunner.execute(() -> populateExtraAppOpsList(false, false));
  }

  public void resetExcFilters() {

    SharedPreferences.Editor prefEditor = MySettings.getDefPrefs().edit();
    int count = 0;

    int resId;
    String name;

    for (Field field : R.string.class.getDeclaredFields()) {
      name = field.getName();

      try {
        resId = R.string.class.getDeclaredField(name).getInt(null);
      } catch (NoSuchFieldException | IllegalAccessException e) {
        continue;
      }

      String str = App.getRes().getString(resId);

      if (str.startsWith("pref_filter_") && name.equals(str + "_key")) {
        prefEditor.remove(str);
        count++;
      }
    }

    prefEditor.apply();
    MyLog.i(TAG, "resetExcFilters", count + " preferences removed");

    BgRunner.execute(() -> populateExcludedAppsList(true, true));
    BgRunner.execute(() -> populateExtraAppOpsList(true, true));
  }
}
