package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.privtasks.util.Util.PM_GET_SIGNATURES;

import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import java.util.Comparator;
import java.util.List;

public enum PkgParserFlavor {
  INS;

  private final PackageManager mPm = App.getPm();

  public void buildRequiredData() {}

  void onPkgCreated(Package pkg) {}

  void onPkgListCompleted() {}

  public List<Package> getAllUsersPkgList() {
    return PackageParser.INS.updatePkgListWithResult(false);
  }

  void sortPkgList(List<PackageInfo> packageInfoList) {
    packageInfoList.sort(
        Comparator.comparing(
            pkgInfo -> pkgInfo.applicationInfo.loadLabel(mPm).toString().toUpperCase()));
  }

  void sortPkgListAgain(List<Package> packageList) {}

  List<PackageInfo> getPackageList() {
    return ApiUtils.getInstalledPackages(PackageManager.GET_PERMISSIONS | PM_GET_SIGNATURES);
  }

  PackageInfo getPackageInfo(Package pkg) {
    return PackageParser.getPkgInfo(pkg.getName(), PackageManager.GET_PERMISSIONS);
  }

  public void setProgress(boolean isMax, int value) {}

  public Boolean isFilteredOut(PackageInfo packageInfo, Package pkg) {
    return null;
  }

  public boolean isFilteredOut(Package pkg) {
    return false;
  }

  public CharSequence getPermName(Permission perm) {
    return perm.getName();
  }

  public CharSequence getPermName(String permName, int groupId) {
    return permName;
  }

  public CharSequence getPermDesc(Permission perm) {
    return null;
  }

  public void dumpPerms(Uri file) {}
}
