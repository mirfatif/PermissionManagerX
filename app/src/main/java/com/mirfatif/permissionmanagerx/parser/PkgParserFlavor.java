package com.mirfatif.permissionmanagerx.parser;

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

  public void sortPkgList(List<PackageInfo> packageInfoList) {
    packageInfoList.sort(
        Comparator.comparing(
            pkgInfo ->
                pkgInfo.applicationInfo.loadLabel(mPackageManager).toString().toUpperCase()));
  }
}
