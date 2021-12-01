package com.mirfatif.permissionmanagerx.pkg;

import android.view.Menu;
import android.view.MenuItem;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.pkg.fwk.PackageActivity;
import java.util.Comparator;
import java.util.List;

public class PkgActivityFlavor {

  private final PackageActivity mA;

  public PkgActivityFlavor(PackageActivity activity) {
    mA = activity;
  }

  public void sortPermsList(List<Permission> permissionsList) {
    permissionsList.sort(Comparator.comparingInt(Permission::getOrder));
  }

  @SuppressWarnings("UnusedDeclaration")
  public void onCreateOptionsMenu(Menu menu) {}

  @SuppressWarnings("UnusedDeclaration")
  public void onPrepareOptionsMenu(Menu menu, boolean havePerms) {}

  @SuppressWarnings("UnusedDeclaration")
  public boolean onOptionsItemSelected(MenuItem item) {
    return false;
  }

  @SuppressWarnings("UnusedDeclaration")
  public boolean beforePermChange(Package mPackage, Permission permission, boolean isSystemFixed) {
    return true;
  }

  @SuppressWarnings("UnusedDeclaration")
  public void afterPermChange(Package mPackage, Permission permission, boolean isSystemFixed) {}

  public void onPermClick(Permission perm) {
    mA.onPermSwitchToggle(perm);
  }

  public void onStart() {}

  public void onStop() {}

  @SuppressWarnings("UnusedDeclaration")
  public void pkgRefChanged(Package mPackage) {}
}
