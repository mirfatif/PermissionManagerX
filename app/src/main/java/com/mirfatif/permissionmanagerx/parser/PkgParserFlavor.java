package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.parser.PackageParser.PM_GET_SIGNATURES;

import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import com.mirfatif.permissionmanagerx.app.App;
import java.util.Comparator;
import java.util.List;

public class PkgParserFlavor {

  private static PkgParserFlavor mPkgParserFlavor;

  public static synchronized PkgParserFlavor getInstance() {
    if (mPkgParserFlavor == null) {
      mPkgParserFlavor = new PkgParserFlavor();
    }
    return mPkgParserFlavor;
  }

  private final PackageManager mPackageManager = App.getContext().getPackageManager();

  private PkgParserFlavor() {}

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
    return PackageParser.getInstance()
        .getPackageInfo(pkg.getName(), PackageManager.GET_PERMISSIONS);
  }

  @SuppressWarnings("UnusedDeclaration")
  public Boolean handleSearchQuery(String queryText, Package pkg, Permission perm) {
    return null;
  }
}
