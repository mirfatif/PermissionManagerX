package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.parser.PackageParser.PKG_PARSER;
import static com.mirfatif.permissionmanagerx.parser.PackageParser.PM_GET_SIGNATURES;

import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import com.mirfatif.permissionmanagerx.app.App;
import java.util.Comparator;
import java.util.List;

public enum PkgParserFlavor {
  PKG_PARSER_FLAVOR;

  private final PackageManager mPackageManager = App.getContext().getPackageManager();

  @SuppressWarnings("UnusedDeclaration")
  void onPkgCreated(Package pkg) {}

  void onPkgListCompleted() {}

  void sortPkgList(List<PackageInfo> packageInfoList) {
    packageInfoList.sort(
        Comparator.comparing(
            pkgInfo ->
                pkgInfo.applicationInfo.loadLabel(mPackageManager).toString().toUpperCase()));
  }

  @SuppressWarnings("UnusedDeclaration")
  void sortPkgListAgain(List<Package> packageList) {}

  List<PackageInfo> getPackageList() {
    return mPackageManager.getInstalledPackages(PackageManager.GET_PERMISSIONS | PM_GET_SIGNATURES);
  }

  PackageInfo getPackageInfo(Package pkg) {
    return PKG_PARSER.getPackageInfo(pkg.getName(), PackageManager.GET_PERMISSIONS);
  }

  @SuppressWarnings("UnusedDeclaration")
  public void setProgress(boolean isMax, int value) {}

  @SuppressWarnings("UnusedDeclaration")
  public boolean isFilteredOut(PackageInfo packageInfo, Package pkg) {
    return false;
  }

  @SuppressWarnings("UnusedDeclaration")
  public boolean isFilteredOut(Package pkg) {
    return false;
  }

  public boolean allowQuickScan() {
    return true;
  }
}
