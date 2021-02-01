package com.mirfatif.permissionmanagerx.ui;

import android.view.Menu;
import android.view.MenuItem;
import com.mirfatif.permissionmanagerx.parser.Permission;
import java.util.Comparator;
import java.util.List;

public class PkgActivityFlavor {

  @SuppressWarnings("UnusedDeclaration")
  public PkgActivityFlavor(PackageActivity activity) {}

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
}
