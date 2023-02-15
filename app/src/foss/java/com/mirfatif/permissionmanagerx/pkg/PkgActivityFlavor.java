package com.mirfatif.permissionmanagerx.pkg;

import android.view.Menu;
import android.view.MenuItem;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.Permission;
import java.util.Comparator;
import java.util.List;

public class PkgActivityFlavor {

  public void sortPermsList(List<Permission> permissionsList) {
    permissionsList.sort(Comparator.comparingInt(Permission::getGroupId));
  }

  public PkgActivityFlavor(PackageActivity activity) {}

  public void onCreateOptionsMenu(Menu menu) {}

  public void onPrepareOptionsMenu(Menu menu, boolean havePerms) {}

  public boolean onOptionsItemSelected(MenuItem item) {
    return false;
  }

  public Boolean beforePermChange(Package mPackage, Permission permission) {
    return true;
  }

  public void afterPermChange(Package mPackage, Permission permission, boolean isSystemFixed) {}

  void beforeAppOpChange(Package pkg, Permission appOp, int mode) {}

  public boolean onPermClick(Permission perm) {
    return true;
  }

  public void onStart() {}

  public void onStop() {}

  public void pkgRefChanged(Package mPackage) {}
}
