package com.mirfatif.permissionmanagerx;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.SystemClock;
import android.text.TextUtils;
import android.util.Log;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.view.WindowManager.LayoutParams;
import android.widget.TextView;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.SearchView;
import androidx.fragment.app.FragmentManager;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.permsdb.PermissionEntity;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class PackageActivity extends AppCompatActivity {

  private List<Permission> mPermissionsList = new ArrayList<>();
  private MySettings mMySettings;
  private PrivDaemonHandler mPrivDaemonHandler;
  private PackageParser mPackageParser;
  private Package mPackage;
  private SwipeRefreshLayout mRefreshLayout;
  private PermissionAdapter mPermissionAdapter;

  private final FragmentManager mFM = getSupportFragmentManager();

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_package);

    int position = getIntent().getIntExtra(MainActivity.EXTRA_PKG_POSITION, -1);
    if (position == -1) {
      Toast.makeText(App.getContext(), R.string.something_bad_happened, Toast.LENGTH_LONG).show();
      finish();
      return;
    }

    mPackageParser = PackageParser.getInstance();
    mPackage = mPackageParser.getPackage(position);

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) actionBar.setTitle(mPackage.getLabel());

    mMySettings = MySettings.getInstance();
    mPrivDaemonHandler = PrivDaemonHandler.getInstance();

    RecyclerView recyclerView = findViewById(R.id.package_recycler_view);
    mPermissionAdapter =
        new PermissionAdapter(
            getSwitchToggleListener(),
            getSpinnerSelectListener(),
            getPermClickListener(),
            getPermLongClickListener());

    // Set Adapter on RecyclerView
    recyclerView.setAdapter(mPermissionAdapter);

    // Create and set a vertically scrolling list
    LinearLayoutManager layoutManager =
        new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
    recyclerView.setLayoutManager(layoutManager);

    // Create and add divider between rows
    recyclerView.addItemDecoration(new DividerItemDecoration(this, LinearLayoutManager.VERTICAL));

    Utils.runInBg(this::updatePermissionsList);

    mRefreshLayout = findViewById(R.id.package_refresh_layout);
    mRefreshLayout.setOnRefreshListener(() -> Utils.runInBg(this::updatePackage));
  }

  private PermClickListener getSwitchToggleListener() {
    return this::setPermission;
  }

  private PermSpinnerSelectListener getSpinnerSelectListener() {
    return (permission, selectedValue) -> {
      if (!checkPrivileges()) {
        updateSpinnerSelection(false);
        return;
      }

      if (permission.isPerUid()) {
        int affectedPackagesCount = getPackageManager().getPackagesForUid(mPackage.getUid()).length;

        if (affectedPackagesCount > 1) {
          AlertDialog dialog =
              new Builder(this)
                  .setPositiveButton(
                      R.string.yes, (d, which) -> setAppOpsMode(permission, selectedValue, true))
                  .setNegativeButton(R.string.no, (dialog1, which) -> updateSpinnerSelection(false))
                  .setMessage(
                      getString(R.string.uid_mode_app_ops_warning, affectedPackagesCount - 1))
                  .create();
          new AlertDialogFragment(dialog).show(mFM, "CHANGE_UID_MODE", false);
        } else {
          setAppOpsMode(permission, selectedValue, true);
        }
      } else {
        setAppOpsMode(permission, selectedValue, false);
      }
    };
  }

  private void setAppOpsMode(Permission permission, int mode, boolean uidMode) {
    Utils.runInBg(
        () -> {
          String pkgName;
          if (uidMode) {
            pkgName = "null";
          } else {
            pkgName = mPackage.getName();
          }
          String command =
              Commands.SET_APP_OPS_MODE
                  + " "
                  + permission.getName()
                  + " "
                  + mPackage.getUid()
                  + " "
                  + pkgName
                  + " "
                  + mode;
          if (mMySettings.DEBUG) Util.debugLog("setAppOpsMode", "Sending command: " + command);
          Object res = mPrivDaemonHandler.sendRequest(command);

          // new Permission objects are created, so cannot check previous one for new state
          if (res != null) {
            Utils.runInFg(
                () ->
                    Toast.makeText(
                            App.getContext(), R.string.something_bad_happened, Toast.LENGTH_LONG)
                        .show());
            Log.e("setAppOpsMode", "Response is " + res);
          }

          updateSpinnerSelection(true);
        });
  }

  // to force revert spinner selection in case of no privileges, user denial, or failure
  private void updateSpinnerSelection(boolean delay) {
    Utils.runInBg(
        () -> {
          updatePackage();
          if (delay) SystemClock.sleep(500); // To avoid Spinner value flash
          Utils.runInFg(() -> mPermissionAdapter.notifyDataSetChanged());

          // Some AppOps may take a little while to update e.g.
          // LEGACY_STORAGE always reverts back to Allow.
          if (!delay) return;
          SystemClock.sleep(500);
          updatePackage();
          Utils.runInFg(() -> mPermissionAdapter.notifyDataSetChanged());
        });
  }

  private PermClickListenerWithLoc getPermClickListener() {
    return (permission, yLocation) -> {
      String permName = permission.createPermNameString();
      String protectionLevel =
          getString(R.string.protection_level, permission.createProtectLevelString());

      View layout = getLayoutInflater().inflate(R.layout.permission_details_alert_dialog, null);
      ((TextView) layout.findViewById(R.id.permission_name_view)).setText(permName);
      ((TextView) layout.findViewById(R.id.protection_level_view)).setText(protectionLevel);
      if (permission.getDescription() != null) {
        TextView descView = layout.findViewById(R.id.perm_desc_view);
        descView.setText(permission.getDescription());
        descView.setVisibility(View.VISIBLE);
      }

      AlertDialog dialog = new Builder(this).setView(layout).create();

      Window dialogWindow = dialog.getWindow();
      if (dialogWindow != null) {
        LayoutParams layoutParams = dialogWindow.getAttributes();
        layoutParams.gravity = Gravity.TOP;
        layoutParams.y = yLocation;
      }

      new AlertDialogFragment(dialog).show(mFM, "PERM_DETAILS", false);
    };
  }

  private PermLongClickListener getPermLongClickListener() {
    return permission -> {
      String permState = getPermState(permission);
      Builder builder = new Builder(this);
      builder.setPositiveButton(
          R.string.exclude,
          (dialogInterface, i) ->
              Utils.runInBg(
                  () -> {
                    mMySettings.addPermToExcludedPerms(permission.getName());
                    updatePackage();

                    // other packages are also affected
                    mPackageParser.updatePackagesList(false);
                  }));
      builder.setNegativeButton(android.R.string.cancel, null);

      boolean isReferenced = permission.isReferenced() != null && permission.isReferenced();
      builder.setNeutralButton(
          (isReferenced ? R.string.clear_reference : R.string.set_reference),
          (dialog, which) -> {
            if (isReferenced) {
              Utils.runInBg(
                  () -> {
                    mMySettings
                        .getPermDb()
                        .deletePermission(mPackage.getName(), permission.getName());
                    mPackageParser.updatePermReferences(
                        mPackage.getName(), permission.getName(), null);
                    updatePackage();
                  });
              return;
            }
            PermissionEntity entity = new PermissionEntity();
            entity.isAppOps = permission.isAppOps();
            entity.permName = permission.getName();
            entity.pkgName = mPackage.getName();
            entity.state = permState;
            Utils.runInBg(
                () -> {
                  int id = mMySettings.getPermDb().getId(entity.pkgName, entity.permName);
                  if (id > 0) entity.id = id;
                  mMySettings.getPermDb().insertAll(entity);
                  mPackageParser.updatePermReferences(
                      mPackage.getName(), permission.getName(), permState);
                  updatePackage();
                });
          });

      // Set message, create and show the AlertDialog
      @SuppressLint("InflateParams")
      View layout = getLayoutInflater().inflate(R.layout.activity_package_perm_title, null);
      TextView titleView = layout.findViewById(R.id.permission_title_view);
      titleView.setText(permission.getName());
      builder.setCustomTitle(titleView);

      String message;
      if (permission.isExtraAppOp()) message = getString(R.string.extra_ops_cant_be_excluded);
      else message = getString(R.string.exclude_perm_from_list);
      if (permission.isChangeable()) {
        if (isReferenced) {
          message += " " + getString(R.string.clear_perm_state_reference, permState);
        } else {
          message += " " + getString(R.string.set_perm_state_reference, permState);
        }
      }
      builder.setMessage(message);

      AlertDialog dialog = builder.create();
      dialog.setOnShowListener(
          d -> {
            dialog.getButton(AlertDialog.BUTTON_NEUTRAL).setEnabled(permission.isChangeable());
            dialog.getButton(AlertDialog.BUTTON_POSITIVE).setEnabled(!permission.isExtraAppOp());
          });
      new AlertDialogFragment(dialog).show(mFM, "PERM_OPTIONS", false);
    };
  }

  private String getPermState(Permission permission) {
    return permission.isAppOps()
        ? mMySettings.getAppOpsModes().get(permission.getAppOpsMode())
        : (permission.isGranted() ? Permission.GRANTED : Permission.REVOKED);
  }

  private void checkEmptyPermissionsList() {
    if (mPermissionsList.size() == 0) {
      TextView noPermissionsView = findViewById(R.id.no_permissions_view);
      String message;
      if (mPackage.getTotalPermCount() != 0) {
        if (mPackage.getTotalPermCount() == 1) {
          message = "1 " + getString(R.string.permission_filtered_out);
        } else {
          message =
              getString(R.string.count_permissions_filtered_out, mPackage.getTotalPermCount());
        }
        findViewById(R.id.open_filter_settings)
            .setOnClickListener(
                v ->
                    startActivity(
                        new Intent().setClass(App.getContext(), FilterSettingsActivity.class)));
        findViewById(R.id.no_permissions_view).setVisibility(View.VISIBLE);
        findViewById(R.id.open_filter_settings).setVisibility(View.VISIBLE);
      } else {
        message = getString(R.string.requested_no_permissions);
        findViewById(R.id.open_filter_settings).setVisibility(View.GONE);
      }
      noPermissionsView.setText(message);
      findViewById(R.id.package_recycler_view).setVisibility(View.GONE);
    } else {
      findViewById(R.id.package_recycler_view).setVisibility(View.VISIBLE);
      findViewById(R.id.no_permissions_view).setVisibility(View.GONE);
      findViewById(R.id.open_filter_settings).setVisibility(View.GONE);
    }
  }

  private void updatePackage() {
    if (mMySettings.DEBUG) Util.debugLog("PackageActivity", "updatePackage() called");
    mPackageParser.updatePackage(mPackage);
    updatePermissionsList(); // the same Package object is updated
  }

  private void updatePermissionsList() {
    mPermissionsList = mPackage.getPermissionsList();
    mPermissionsList.sort(Comparator.comparingInt(Permission::getOrder));
    handleSearchQuery();
    Utils.runInFg(
        () -> {
          checkEmptyPermissionsList();
          mRefreshLayout.setRefreshing(false);
        });
  }

  private SearchView mSearchView;

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.package_menu, menu);
    menu.findItem(R.id.action_reset_app_ops).setVisible(!mMySettings.excludeAppOpsPerms());

    MenuItem searchMenuItem = menu.findItem(R.id.action_search);
    mSearchView = searchMenuItem.getActionView().findViewById(R.id.action_search);
    mSearchView.setMaxWidth(Integer.MAX_VALUE);

    mSearchView.setOnQueryTextListener(
        new SearchView.OnQueryTextListener() {
          @Override
          public boolean onQueryTextSubmit(String query) {
            Utils.runInBg(() -> handleSearchQuery());
            return true;
          }

          @Override
          public boolean onQueryTextChange(String newText) {
            Utils.runInBg(() -> handleSearchQuery());
            return true;
          }
        });

    // clear search query when no text is entered
    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          if (!hasFocus && TextUtils.isEmpty(mSearchView.getQuery())) {
            collapseSearchView();
          }
        });

    // Show a search hint
    mSearchView.setQueryHint(getString(R.string.search_menu_item));

    return super.onCreateOptionsMenu(menu);
  }

  private void handleSearchQuery() {
    CharSequence queryText = mSearchView == null ? null : mSearchView.getQuery();
    if (queryText == null || TextUtils.isEmpty(queryText)) {
      Utils.runInFg(() -> mPermissionAdapter.submitList(new ArrayList<>(mPermissionsList)));
      return;
    }

    List<Permission> permList = new ArrayList<>();
    for (Permission permission : mPermissionsList) {
      if (permission.contains(queryText.toString())) permList.add(permission);
      Utils.runInFg(() -> mPermissionAdapter.submitList(new ArrayList<>(permList)));
    }
  }

  private void collapseSearchView() {
    mSearchView.onActionViewCollapsed();
    mSearchView.setQuery(null, false);
    Utils.runInBg(this::handleSearchQuery);
  }

  @Override
  public boolean onOptionsItemSelected(@NonNull MenuItem item) {
    if (mMySettings.DEBUG)
      Util.debugLog("PackageActivity", "onOptionsItemSelected(): " + item.getTitle());
    if (item.getItemId() == R.id.action_information) {
      startActivity(
          new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
              .setData(Uri.parse("package:" + mPackage.getName())));
    }

    if (item.getItemId() == R.id.action_reset_app_ops && checkPrivileges()) {
      AlertDialog dialog =
          new Builder(this)
              .setPositiveButton(
                  R.string.yes,
                  (d, which) -> {
                    String cmd =
                        Commands.RESET_APP_OPS + " " + Utils.getUserId() + " " + mPackage.getName();
                    Utils.runInBg(
                        () -> {
                          Object res = mPrivDaemonHandler.sendRequest(cmd);
                          updatePackage();
                          if (res != null) {
                            Utils.runInFg(
                                () ->
                                    Toast.makeText(
                                            App.getContext(),
                                            R.string.something_bad_happened,
                                            Toast.LENGTH_LONG)
                                        .show());
                            Log.e("resetAppOps", "Response is " + res);
                          }
                        });
                  })
              .setNegativeButton(R.string.no, null)
              .setTitle(mPackage.getLabel())
              .setMessage(R.string.reset_app_ops_confirmation)
              .create();
      new AlertDialogFragment(dialog).show(mFM, "RESET_APP_OPS_CONFIRM", false);
    }

    if (item.getItemId() == R.id.action_set_all_references) {
      AlertDialog dialog =
          new Builder(this)
              .setPositiveButton(
                  R.string.yes,
                  (d, which) ->
                      Utils.runInBg(
                          () -> {
                            List<BackupEntry> permEntries = new ArrayList<>();
                            for (Permission permission : mPermissionsList) {
                              if (!permission.isChangeable()) continue;

                              String permState = getPermState(permission);

                              BackupEntry backupEntry = new BackupEntry();
                              backupEntry.key = mPackage.getName();
                              backupEntry.value = permState;
                              backupEntry.type = permission.getName();
                              permEntries.add(backupEntry);

                              mPackageParser.updatePermReferences(
                                  mPackage.getName(), permission.getName(), permState);
                            }
                            BackupRestore.updatePermissionEntities(permEntries);
                            updatePackage();
                          }))
              .setNegativeButton(R.string.no, null)
              .setTitle(mPackage.getLabel())
              .setMessage(R.string.set_references_confirmation)
              .create();
      new AlertDialogFragment(dialog).show(mFM, "SET_REF_CONFIRM", false);
    }
    if (item.getItemId() == R.id.action_clear_references) {
      AlertDialog dialog =
          new Builder(this)
              .setPositiveButton(
                  R.string.yes,
                  (d, which) ->
                      Utils.runInBg(
                          () -> {
                            mMySettings.getPermDb().deletePackage(mPackage.getName());
                            for (Permission permission : mPermissionsList) {
                              mPackageParser.updatePermReferences(
                                  mPackage.getName(), permission.getName(), null);
                            }
                            updatePackage();
                          }))
              .setNegativeButton(R.string.no, null)
              .setTitle(mPackage.getLabel())
              .setMessage(R.string.clear_references_confirmation)
              .create();
      new AlertDialogFragment(dialog).show(mFM, "CLEAR_REF_CONFIRM", false);
    }

    // do not recreate parent (Main) activity
    if (item.getItemId() == android.R.id.home) {
      onBackPressed();
      return true;
    }

    return super.onOptionsItemSelected(item);
  }

  private void setPermission(Permission permission) {
    if (!checkPrivileges()) return;

    Utils.runInBg(
        () -> {
          String command =
              mPackage.getName() + " " + permission.getName() + " " + Utils.getUserId();
          if (permission.isGranted()) {
            command = Commands.REVOKE_PERMISSION + " " + command;
          } else {
            command = Commands.GRANT_PERMISSION + " " + command;
          }

          if (mMySettings.DEBUG) Util.debugLog("setPermission", "Sending command: " + command);
          Object res = mPrivDaemonHandler.sendRequest(command);
          updatePackage();

          // new Permission objects are created, so cannot check previous one for new state
          if (res != null) {
            Utils.runInFg(
                () ->
                    Toast.makeText(
                            App.getContext(), R.string.something_bad_happened, Toast.LENGTH_LONG)
                        .show());
            Log.e("setPermission", "Response is " + res);
          }
        });
  }

  private boolean checkPrivileges() {
    if (mMySettings.mPrivDaemonAlive) {
      return true;
    }

    AlertDialog dialog =
        new Builder(this)
            .setPositiveButton(
                android.R.string.ok,
                (d, which) -> {
                  startActivity(
                      new Intent(App.getContext(), MainActivity.class)
                          .setAction(MainActivity.ACTION_SHOW_DRAWER)
                          .setFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT));
                  finish();
                })
            .setNegativeButton(android.R.string.cancel, null)
            .setTitle(R.string.privileges)
            .setMessage(R.string.grant_root_or_adb)
            .create();
    new AlertDialogFragment(dialog).show(mFM, MainActivity.TAG_GRANT_ROOT_OR_ADB, false);
    return false;
  }

  // update package when coming back after changing FilterSettings
  // also called on first time activity launch to populate view
  // don't update list, it's already updated from FilterSettingsActivity (if made any changes)
  @Override
  protected void onResume() {
    super.onResume();
    Utils.runInBg(this::updatePackage);
  }

  @Override
  public void onBackPressed() {
    if (!TextUtils.isEmpty(mSearchView.getQuery())) {
      collapseSearchView();
      return;
    }
    super.onBackPressed();
  }
}
