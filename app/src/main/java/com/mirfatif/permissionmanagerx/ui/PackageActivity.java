package com.mirfatif.permissionmanagerx.ui;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.net.Uri;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.Bundle;
import android.os.SystemClock;
import android.text.Spanned;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.view.WindowManager.LayoutParams;
import android.widget.Button;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.widget.SearchView;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.main.BackupRestore;
import com.mirfatif.permissionmanagerx.main.BackupRestore.BackupEntry;
import com.mirfatif.permissionmanagerx.main.MainActivity;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.prefs.FilterSettingsActivity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.ui.PermissionAdapter.PermClickListener;
import com.mirfatif.permissionmanagerx.ui.PermissionAdapter.PermClickListenerWithLoc;
import com.mirfatif.permissionmanagerx.ui.PermissionAdapter.PermLongClickListener;
import com.mirfatif.permissionmanagerx.ui.PermissionAdapter.PermSpinnerSelectListener;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.locks.ReentrantLock;

public class PackageActivity extends BaseActivity {

  private static final String TAG = "PackageActivity";

  private final MySettings mMySettings = MySettings.getInstance();
  private final PackageParser mPackageParser = PackageParser.getInstance();
  private final PrivDaemonHandler mPrivDaemonHandler = PrivDaemonHandler.getInstance();

  private PkgActivityFlavor mPkgActivityFlavor;

  private Package mPackage;
  private List<Permission> mPermissionsList = new ArrayList<>();
  private PermissionAdapter mPermissionAdapter;
  private SwipeRefreshLayout mRefreshLayout;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_package);

    mPkgActivityFlavor = new PkgActivityFlavor(this);

    int position = getIntent().getIntExtra(MainActivity.EXTRA_PKG_POSITION, -1);
    if (position == -1 || (mPackage = mPackageParser.getPackage(position)) == null) {
      Utils.showToast(R.string.something_bad_happened);
      finishAfterTransition();
      return;
    }

    mPackage.setIsRemoved(false);

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null && mPackage != null) {
      actionBar.setTitle(mPackage.getLabel());
    }

    RecyclerView recyclerView = findViewById(R.id.package_recycler_view);
    mPermissionAdapter =
        new PermissionAdapter(
            this,
            new SwitchToggleListener(),
            new SpinnerSelectListener(),
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

  private boolean isPackageNull() {
    if (mPackage == null) {
      Utils.runInFg(this::finishAfterTransition);
      return true;
    }
    return false;
  }

  private void setAppOpsMode(Permission permission, Integer pos, int mode, boolean uidMode) {
    holdRefreshLock();
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
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "setAppOpsMode: sending command: " + command);
    }
    mPrivDaemonHandler.sendRequest(command);
    updateSpinnerSelection(true, pos);
  }

  private void updateSpinnerSelectionInBg(Integer position) {
    Utils.runInBg(() -> updateSpinnerSelection(false, position));
  }

  // To force revert spinner selection in case of no privileges, user denial, or failure
  private void updateSpinnerSelection(boolean delay, Integer position) {
    updatePackage();
    if (delay) {
      SystemClock.sleep(500); // To avoid Spinner value flash
    }
    updateSpinnerSelection(position);

    // Some AppOps may take a little while to update e.g.
    // LEGACY_STORAGE always reverts back to Allow.
    if (delay) {
      SystemClock.sleep(500);
      updatePackage();
      updateSpinnerSelection(position);
      if (STOP_REFRESH_LOCK.isHeldByCurrentThread()) {
        STOP_REFRESH_LOCK.unlock();
      }
      stopRefreshing();
    }
  }

  private void updateSpinnerSelection(Integer position) {
    if (position != null) {
      Utils.runInFg(() -> mPermissionAdapter.notifyItemChanged(position));
    } else {
      Utils.runInFg(() -> mPermissionAdapter.notifyDataSetChanged());
    }
  }

  private final ReentrantLock STOP_REFRESH_LOCK = new ReentrantLock();

  private void stopRefreshing() {
    if (STOP_REFRESH_LOCK.isLocked()) {
      return;
    }
    Utils.runInFg(() -> mRefreshLayout.setRefreshing(false));
  }

  private void holdRefreshLock() {
    STOP_REFRESH_LOCK.lock();
    Utils.runInFg(() -> mRefreshLayout.setRefreshing(true));
  }

  private PermClickListenerWithLoc getPermClickListener() {
    return (permission, yLocation) -> {
      String permName = permission.createPermNameString();
      Spanned protectionLevel =
          Utils.htmlToString(
              getString(R.string.protection_level, permission.createProtectLevelString()));

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

      new AlertDialogFragment(dialog).show(this, "PERM_DETAILS", false);
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
                    mPackageParser.updatePackagesList();
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
                  if (id > 0) {
                    entity.id = id;
                  }
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
      if (permission.isExtraAppOp()) {
        message = getString(R.string.extra_ops_cant_be_excluded);
      } else {
        message = getString(R.string.exclude_perm_from_list);
      }
      if (permission.isChangeable()) {
        if (isReferenced) {
          message += getString(R.string.clear_perm_state_reference, permState);
        } else {
          message += getString(R.string.set_perm_state_reference, permState);
        }
      }
      builder.setMessage(Utils.htmlToString(message));

      AlertDialog dialog = builder.create();
      dialog.setOnShowListener(
          d -> {
            dialog.getButton(AlertDialog.BUTTON_NEUTRAL).setEnabled(permission.isChangeable());
            dialog.getButton(AlertDialog.BUTTON_POSITIVE).setEnabled(!permission.isExtraAppOp());
          });
      new AlertDialogFragment(dialog).show(this, "PERM_OPTIONS", false);
    };
  }

  private String getPermState(Permission permission) {
    return permission.isAppOps()
        ? mMySettings.getAppOpsModes().get(permission.getAppOpsMode())
        : (permission.isGranted() ? Permission.GRANTED : Permission.REVOKED);
  }

  private void checkEmptyPermissionsList() {
    TextView noPermissionsView = findViewById(R.id.no_permissions_view);
    Button filterSettingsButton = findViewById(R.id.open_filter_settings);

    if (mPermissionsList.size() == 0) {
      String message;
      if (mPackage.getTotalPermCount() != 0) {
        if (mPackage.getTotalPermCount() == 1) {
          message = getString(R.string.permission_filtered_out);
        } else {
          message =
              getString(R.string.count_permissions_filtered_out, mPackage.getTotalPermCount());
        }
        filterSettingsButton.setOnClickListener(
            v -> startActivity(new Intent(App.getContext(), FilterSettingsActivity.class)));
        filterSettingsButton.setVisibility(View.VISIBLE);
      } else {
        message = getString(R.string.requested_no_permissions);
        filterSettingsButton.setVisibility(View.GONE);
      }
      noPermissionsView.setText(message);
      noPermissionsView.setVisibility(View.VISIBLE);
      mRefreshLayout.setVisibility(View.GONE);
    } else {
      mRefreshLayout.setVisibility(View.VISIBLE);
      noPermissionsView.setVisibility(View.GONE);
      filterSettingsButton.setVisibility(View.GONE);
    }
    invalidateOptionsMenu();
  }

  void updatePackage() {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "updatePackage() called");
    }
    mPackageParser.updatePackage(mPackage);
    if (mPackage == null || mPackage.isRemoved()) {
      // Package is excluded due to changes made by user e.g. uninstalled or ref states changed
      Utils.runInFg(this::finishAfterTransition);
    } else {
      // The same Package Object is updated, but with new Permission Objects
      updatePermissionsList();
    }
  }

  private void updatePermissionsList() {
    mPermissionsList = mPackage.getPermissionsList();
    if (mPkgActivityFlavor != null) {
      mPkgActivityFlavor.sortPermsList(mPermissionsList);
    }
    submitPermsList();
    Utils.runInFg(this::checkEmptyPermissionsList);
    stopRefreshing();
  }

  private SearchView mSearchView;

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.package_menu, menu);
    if (VERSION.SDK_INT >= VERSION_CODES.P) {
      menu.setGroupDividerEnabled(true);
    }

    menu.findItem(R.id.action_reset_app_ops).setVisible(!mMySettings.excludeAppOpsPerms());

    MenuItem searchMenuItem = menu.findItem(R.id.action_search);
    mSearchView = searchMenuItem.getActionView().findViewById(R.id.action_search);
    mSearchView.setMaxWidth(Integer.MAX_VALUE);

    mSearchView.setOnQueryTextListener(
        new SearchView.OnQueryTextListener() {
          @Override
          public boolean onQueryTextSubmit(String query) {
            Utils.runInBg(() -> submitPermsList());
            return true;
          }

          @Override
          public boolean onQueryTextChange(String newText) {
            Utils.runInBg(() -> submitPermsList());
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

    if (mPkgActivityFlavor != null) {
      mPkgActivityFlavor.onCreateOptionsMenu(menu);
    }
    return super.onCreateOptionsMenu(menu);
  }

  @Override
  public boolean onPrepareOptionsMenu(Menu menu) {
    boolean havePerms = mPermissionsList.size() > 0;
    menu.findItem(R.id.action_search).setVisible(havePerms);
    menu.findItem(R.id.action_reset_app_ops).setVisible(havePerms);
    menu.findItem(R.id.action_set_all_references).setVisible(havePerms);
    menu.findItem(R.id.action_clear_references).setVisible(havePerms);
    if (mPkgActivityFlavor != null) {
      mPkgActivityFlavor.onPrepareOptionsMenu(menu, havePerms);
    }
    return super.onPrepareOptionsMenu(menu);
  }

  private void submitPermsList() {
    CharSequence queryText = mSearchView == null ? null : mSearchView.getQuery();
    if (queryText == null || TextUtils.isEmpty(queryText)) {
      submitList(mPermissionsList);
      return;
    }

    synchronized (mSearchExecutor) {
      if (mSearchFuture != null && !mSearchFuture.isDone()) {
        mSearchFuture.cancel(true);
      }
      mSearchFuture = mSearchExecutor.submit(() -> handleSearchQuery(queryText.toString()));
    }
  }

  private final ExecutorService mSearchExecutor = Executors.newSingleThreadExecutor();
  private Future<?> mSearchFuture;

  private void handleSearchQuery(String queryText) {
    long ts = System.currentTimeMillis();
    List<Permission> permList = new ArrayList<>();
    for (Permission permission : mPermissionsList) {
      if (permission.contains(queryText)) {
        permList.add(permission);
      }
      if (Thread.interrupted()) {
        return;
      }
      if (System.currentTimeMillis() - ts > 500) {
        submitList(permList);
      }
    }
    submitList(permList);
  }

  private void submitList(List<Permission> permList) {
    if (mPermissionAdapter != null) {
      Utils.runInFg(() -> mPermissionAdapter.submitList(new ArrayList<>(permList)));
    }
  }

  private void collapseSearchView() {
    if (mSearchView != null) {
      mSearchView.onActionViewCollapsed();
      mSearchView.setQuery(null, false);
    }
    Utils.runInBg(this::submitPermsList);
  }

  @Override
  public boolean onOptionsItemSelected(@NonNull MenuItem item) {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "onOptionsItemSelected: " + item.getTitle());
    }

    if (item.getItemId() == R.id.action_information) {
      openAppInfo();
      return true;
    }

    if (item.getItemId() == R.id.action_reset_app_ops && checkPrivileges()) {
      if (!isPackageNull()) {
        AlertDialog dialog =
            new Builder(this)
                .setPositiveButton(R.string.yes, (d, which) -> Utils.runInBg(this::resetAppOps))
                .setNegativeButton(R.string.no, null)
                .setTitle(mPackage.getLabel())
                .setMessage(R.string.reset_app_ops_confirmation)
                .create();
        new AlertDialogFragment(dialog).show(this, "RESET_APP_OPS_CONFIRM", false);
      }
      return true;
    }

    if (item.getItemId() == R.id.action_set_all_references) {
      if (!isPackageNull()) {
        AlertDialog dialog =
            new Builder(this)
                .setPositiveButton(
                    R.string.yes, (d, which) -> Utils.runInBg(this::setAllReferences))
                .setNegativeButton(R.string.no, null)
                .setTitle(mPackage.getLabel())
                .setMessage(R.string.set_references_confirmation)
                .create();
        new AlertDialogFragment(dialog).show(this, "SET_REF_CONFIRM", false);
      }
      return true;
    }

    if (item.getItemId() == R.id.action_clear_references) {
      if (!isPackageNull()) {
        AlertDialog dialog =
            new Builder(this)
                .setPositiveButton(R.string.yes, (d, which) -> Utils.runInBg(this::clearReferences))
                .setNegativeButton(R.string.no, null)
                .setTitle(mPackage.getLabel())
                .setMessage(R.string.clear_references_confirmation)
                .create();
        new AlertDialogFragment(dialog).show(this, "CLEAR_REF_CONFIRM", false);
      }
      return true;
    }

    boolean res = false;
    if (mPkgActivityFlavor != null) {
      res = mPkgActivityFlavor.onOptionsItemSelected(item);
    }
    return res || super.onOptionsItemSelected(item);
  }

  private void openAppInfo() {
    if (isPackageNull()) {
      return;
    }
    int pkgUserId = Utils.getUserId(mPackage.getUid());
    if (Utils.getUserId() == pkgUserId) {
      startActivity(
          new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
              .setData(Uri.parse("package:" + mPackage.getName())));
    } else if (mMySettings.isPrivDaemonAlive()) {
      String cmd = Commands.OPEN_APP_INFO + " " + mPackage.getName() + " " + pkgUserId;

      if (mMySettings.isDebug()) {
        Util.debugLog(TAG, "openAppInfo: sending command: " + cmd);
      }

      Utils.runInBg(() -> mPrivDaemonHandler.sendRequest(cmd));
    }
  }

  private void resetAppOps() {
    if (isPackageNull()) {
      return;
    }
    holdRefreshLock();
    String cmd =
        Commands.RESET_APP_OPS
            + " "
            + Utils.getUserId(mPackage.getUid())
            + " "
            + mPackage.getName();
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "resetAppOps: sending command: " + cmd);
    }
    mPrivDaemonHandler.sendRequest(cmd);
    updatePackage();
    updateSpinnerSelection(true, null);
  }

  private void setAllReferences() {
    if (isPackageNull()) {
      return;
    }
    List<BackupEntry> permEntries = new ArrayList<>();
    for (Permission permission : mPermissionsList) {
      if (!permission.isChangeable()) {
        continue;
      }

      String permState = getPermState(permission);

      BackupEntry backupEntry = new BackupEntry();
      backupEntry.key = mPackage.getName();
      backupEntry.value = permState;
      backupEntry.type = permission.getName();
      permEntries.add(backupEntry);

      mPackageParser.updatePermReferences(mPackage.getName(), permission.getName(), permState);
    }
    BackupRestore.updatePermissionEntities(permEntries);
    updatePackage();
  }

  private void clearReferences() {
    if (isPackageNull()) {
      return;
    }
    mMySettings.getPermDb().deletePackage(mPackage.getName());
    for (Permission permission : mPermissionsList) {
      mPackageParser.updatePermReferences(mPackage.getName(), permission.getName(), null);
    }
    updatePackage();
  }

  private void setPermission(Permission permission) {
    if (isPackageNull()) {
      return;
    }
    String command =
        mPackage.getName() + " " + permission.getName() + " " + Utils.getUserId(mPackage.getUid());
    if (permission.isGranted()) {
      command = Commands.REVOKE_PERMISSION + " " + command;
    } else {
      command = Commands.GRANT_PERMISSION + " " + command;
    }

    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "setPermission: sending command: " + command);
    }
    mPrivDaemonHandler.sendRequest(command);
    updatePackage();
  }

  private boolean checkPrivileges() {
    if (mMySettings.isPrivDaemonAlive()) {
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
                  finishAfterTransition();
                })
            .setNegativeButton(android.R.string.cancel, null)
            .setTitle(R.string.privileges)
            .setMessage(R.string.grant_root_or_adb)
            .create();
    new AlertDialogFragment(dialog).show(this, MainActivity.TAG_GRANT_ROOT_OR_ADB, false);
    return false;
  }

  private void doNotRemindDangAction() {
    mMySettings.savePref(R.string.pref_package_warn_dang_change_enc_key, false);
  }

  @Override
  protected void onSaveInstanceState(@NonNull Bundle outState) {
    AlertDialogFragment.removeAll(this);
    super.onSaveInstanceState(outState);
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
    if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
      collapseSearchView();
      return;
    }
    super.onBackPressed();
  }

  private class SwitchToggleListener implements PermClickListener {

    @Override
    public void onClick(Permission permission) {
      if (isPackageNull()) {
        return;
      }

      if (!checkPrivileges()) {
        return;
      }

      String warn = null;
      if (mMySettings.getBoolPref(R.string.pref_package_warn_dang_change_enc_key)) {
        if (mPackage.isFrameworkApp()) {
          warn = getString(R.string.change_perms_warning, getString(R.string.framework));
        } else if (mPackage.isSystemApp()) {
          warn = getString(R.string.change_perms_warning, getString(R.string.system));
        }
      }

      if (warn == null) {
        Utils.runInBg(() -> setPermission(permission));
        return;
      }

      AlertDialog dialog =
          new Builder(PackageActivity.this)
              .setPositiveButton(
                  R.string.yes, (d, which) -> Utils.runInBg(() -> setPermission(permission)))
              .setNegativeButton(R.string.no, null)
              .setNeutralButton(
                  R.string.do_not_remind,
                  (d, which) -> {
                    doNotRemindDangAction();
                    Utils.runInBg(() -> setPermission(permission));
                  })
              .setTitle(R.string.warning)
              .setMessage(Utils.breakParas(warn))
              .create();
      new AlertDialogFragment(dialog).show(PackageActivity.this, "PERM_CHANGE_WARNING", false);
    }
  }

  private class SpinnerSelectListener implements PermSpinnerSelectListener {

    @Override
    public void onSelect(Permission perm, int selectedValue) {
      if (mPackage == null || mPermissionAdapter == null) {
        finishAfterTransition();
        return;
      }

      int position = mPermissionAdapter.getCurrentList().indexOf(perm); // May come -1
      Integer pos = position == mPermissionsList.indexOf(perm) && position >= 0 ? position : null;
      if (!checkPrivileges()) {
        updateSpinnerSelectionInBg(pos);
        return;
      }

      String warn = null;
      if (mMySettings.getBoolPref(R.string.pref_package_warn_dang_change_enc_key)) {
        if (mPackage.isFrameworkApp()) {
          warn = getString(R.string.change_perms_warning, getString(R.string.framework));
        } else if (mPackage.isSystemApp()) {
          warn = getString(R.string.change_perms_warning, getString(R.string.system));
        }
      }

      boolean uidMode = perm.isPerUid();

      int affectedPkgCount = 0;
      if (uidMode) {
        affectedPkgCount = getPackageManager().getPackagesForUid(mPackage.getUid()).length;
      }

      if (warn == null && (!uidMode || affectedPkgCount <= 1)) {
        Utils.runInBg(() -> setAppOpsMode(perm, pos, selectedValue, uidMode));
        return;
      }

      String msg = "";
      if (affectedPkgCount > 1) {
        msg = getString(R.string.uid_mode_app_ops_warning, affectedPkgCount - 1);
        if (warn == null) {
          msg += "\n" + getString(R.string._continue);
        }
      }

      if (warn != null) {
        if (!msg.isEmpty()) {
          msg += "\n";
        }
        msg += warn;
      }

      final boolean[] isYes = {false};
      Builder builder =
          new Builder(PackageActivity.this)
              .setPositiveButton(
                  R.string.yes,
                  (dialog, which) -> {
                    isYes[0] = true;
                    Utils.runInBg(() -> setAppOpsMode(perm, pos, selectedValue, uidMode));
                  })
              .setNegativeButton(R.string.no, null)
              .setTitle(R.string.warning)
              .setMessage(Utils.breakParas(msg));

      if (affectedPkgCount <= 1) {
        builder.setNeutralButton(
            R.string.do_not_remind,
            (dialog, which) -> {
              isYes[0] = true;
              Utils.runInBg(() -> setAppOpsMode(perm, pos, selectedValue, uidMode));
              doNotRemindDangAction();
            });
      }

      // UpdateSpinner on Dialog dismiss also suffices for Negative and Neutral buttons
      new AlertDialogFragment(builder.create())
          .setOnDismissListener(
              dialog -> {
                if (!isYes[0]) {
                  updateSpinnerSelectionInBg(pos);
                }
              })
          .show(PackageActivity.this, "PERM_CHANGE_WARNING", false);
    }
  }
}
