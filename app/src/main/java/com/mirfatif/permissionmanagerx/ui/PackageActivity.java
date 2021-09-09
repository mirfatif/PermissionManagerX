package com.mirfatif.permissionmanagerx.ui;

import static com.mirfatif.permissionmanagerx.parser.AppOpsParser.APP_OPS_PARSER;
import static com.mirfatif.permissionmanagerx.parser.PackageParser.PKG_PARSER;
import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;
import static com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler.DAEMON_HANDLER;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.net.Uri;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.Bundle;
import android.os.SystemClock;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.view.WindowManager.LayoutParams;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.widget.SearchView;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.ActivityPackageBinding;
import com.mirfatif.permissionmanagerx.databinding.ActivityPkgPermTitleBinding;
import com.mirfatif.permissionmanagerx.databinding.PermDetailsDialogBinding;
import com.mirfatif.permissionmanagerx.main.BackupRestore;
import com.mirfatif.permissionmanagerx.main.BackupRestore.BackupEntry;
import com.mirfatif.permissionmanagerx.main.MainActivity;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.prefs.FilterSettingsActivity;
import com.mirfatif.permissionmanagerx.ui.PermissionAdapter.PermAdapterCallback;
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

  private PkgActivityFlavor mPkgActivityFlavor;

  private Package mPackage;
  private List<Permission> mPermissionsList = new ArrayList<>();
  private PermissionAdapter mPermissionAdapter;
  private ActivityPackageBinding mB;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    if (Utils.setNightTheme(this)) {
      return;
    }
    mB = ActivityPackageBinding.inflate(getLayoutInflater());
    setContentView(mB.getRoot());

    mPkgActivityFlavor = new PkgActivityFlavor(this);

    int position = getIntent().getIntExtra(MainActivity.EXTRA_PKG_POSITION, -1);
    if (position == -1 || (mPackage = PKG_PARSER.getPackage(position)) == null) {
      Utils.showToast(R.string.something_bad_happened);
      finishAfterTransition();
      return;
    }

    mPackage.setIsRemoved(false);

    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null && mPackage != null) {
      actionBar.setTitle(mPackage.getLabel());
    }

    mPermissionAdapter = new PermissionAdapter(this, new PermAdapterCallbackImpl());

    // Set Adapter on RecyclerView
    mB.recyclerV.setAdapter(mPermissionAdapter);

    // Create and set a vertically scrolling list
    LinearLayoutManager layoutManager =
        new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
    mB.recyclerV.setLayoutManager(layoutManager);

    // Create and add divider between rows
    mB.recyclerV.addItemDecoration(new DividerItemDecoration(this, LinearLayoutManager.VERTICAL));

    Utils.runInBg(this::updatePermissionsList);

    mB.refreshLayout.setOnRefreshListener(() -> Utils.runInBg(this::updatePackage));
  }

  private boolean isPackageNull() {
    if (mPackage == null) {
      Utils.runInFg(this, this::finishAfterTransition);
      return true;
    }
    return false;
  }

  private void setAppOpsMode(Permission permission, Integer pos, int mode) {
    holdRefreshLock();
    Permission.setAppOpMode(mPackage, permission, mode);
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

  @SuppressLint("NotifyDataSetChanged")
  private void updateSpinnerSelection(Integer position) {
    if (position != null) {
      Utils.runInFg(this, () -> mPermissionAdapter.notifyItemChanged(position));
    } else {
      Utils.runInFg(this, () -> mPermissionAdapter.notifyDataSetChanged());
    }
  }

  private final ReentrantLock STOP_REFRESH_LOCK = new ReentrantLock();

  private void stopRefreshing() {
    if (STOP_REFRESH_LOCK.isLocked()) {
      return;
    }
    Utils.runInFg(this, () -> mB.refreshLayout.setRefreshing(false));
  }

  private void holdRefreshLock() {
    STOP_REFRESH_LOCK.lock();
    Utils.runInFg(this, () -> mB.refreshLayout.setRefreshing(true));
  }

  private String getPermState(Permission permission) {
    return permission.isAppOps()
        ? APP_OPS_PARSER.getAppOpsModes().get(permission.getAppOpsMode())
        : (permission.isGranted() ? Permission.GRANTED : Permission.REVOKED);
  }

  private void checkEmptyPermissionsList() {
    if (mPermissionsList.size() == 0) {
      String message;
      if (mPackage.getTotalPermCount() != 0) {
        int cnt = mPackage.getTotalPermCount();
        message = Utils.getQtyString(R.plurals.count_permissions_filtered_out, cnt, cnt);
        mB.settingsButton.setOnClickListener(
            v -> startActivity(new Intent(App.getContext(), FilterSettingsActivity.class)));
        mB.settingsButton.setVisibility(View.VISIBLE);
      } else {
        message = getString(R.string.requested_no_permissions);
        mB.settingsButton.setVisibility(View.GONE);
      }
      mB.noPermsView.setText(message);
      mB.noPermsView.setVisibility(View.VISIBLE);
      mB.refreshLayout.setVisibility(View.GONE);
    } else {
      mB.refreshLayout.setVisibility(View.VISIBLE);
      mB.noPermsView.setVisibility(View.GONE);
      mB.settingsButton.setVisibility(View.GONE);
    }
    invalidateOptionsMenu();
  }

  void updatePackage() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "updatePackage() called");
    }
    PKG_PARSER.updatePackage(mPackage);
    if (mPackage == null || mPackage.isRemoved()) {
      // Package is excluded due to changes made by user e.g. uninstalled or ref states changed
      Utils.runInFg(this, this::finishAfterTransition);
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
    Utils.runInFg(this, this::checkEmptyPermissionsList);
    stopRefreshing();
  }

  private SearchView mSearchView;

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.package_menu, menu);
    if (VERSION.SDK_INT >= VERSION_CODES.P) {
      menu.setGroupDividerEnabled(true);
    }

    menu.findItem(R.id.action_reset_app_ops).setVisible(!SETTINGS.excludeAppOpsPerms());

    MenuItem searchMenuItem = menu.findItem(R.id.action_search);
    mSearchView = (SearchView) searchMenuItem.getActionView();
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
      if (permission.contains(mPackage, queryText, false)) {
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
      Utils.runInFg(this, () -> mPermissionAdapter.submitList(new ArrayList<>(permList)));
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
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "onOptionsItemSelected: " + item.getTitle());
    }

    if (item.getItemId() == R.id.action_information) {
      openAppInfo();
      return true;
    }

    if (item.getItemId() == R.id.action_reset_app_ops && checkPrivileges()) {
      if (!isPackageNull()) {
        AlertDialogFragment.show(this, null, TAG_RESET_APP_OPS_CONFIRM);
      }
      return true;
    }

    if (item.getItemId() == R.id.action_set_all_references) {
      if (!isPackageNull()) {
        AlertDialogFragment.show(this, null, TAG_SET_REF_CONFIRM);
      }
      return true;
    }

    if (item.getItemId() == R.id.action_clear_references) {
      if (!isPackageNull()) {
        AlertDialogFragment.show(this, null, TAG_CLEAR_REF_CONFIRM);
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
    } else if (SETTINGS.isPrivDaemonAlive()) {
      String cmd = Commands.OPEN_APP_INFO + " " + mPackage.getName() + " " + pkgUserId;

      if (SETTINGS.isDebug()) {
        Util.debugLog(TAG, "openAppInfo: sending command: " + cmd);
      }

      Utils.runInBg(() -> DAEMON_HANDLER.sendRequest(cmd));
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
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "resetAppOps: sending command: " + cmd);
    }
    DAEMON_HANDLER.sendRequest(cmd);
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

      PKG_PARSER.updatePermReferences(mPackage.getName(), permission.getName(), permState);
    }
    BackupRestore.updatePermissionEntities(permEntries);
    updatePackage();
    mPkgActivityFlavor.pkgRefChanged(mPackage);
  }

  private void clearReferences() {
    if (isPackageNull()) {
      return;
    }
    SETTINGS.getPermDb().deletePackage(mPackage.getName());
    for (Permission permission : mPermissionsList) {
      PKG_PARSER.updatePermReferences(mPackage.getName(), permission.getName(), null);
    }
    updatePackage();
    mPkgActivityFlavor.pkgRefChanged(mPackage);
  }

  void onPermSwitchToggle(Permission perm) {
    String warn = null;
    if (SETTINGS.getBoolPref(R.string.pref_package_warn_dang_change_enc_key)) {
      if (mPackage.isFrameworkApp()) {
        warn = getString(R.string.change_perms_warning, getString(R.string.framework));
      } else if (mPackage.isSystemApp()) {
        warn = getString(R.string.change_perms_warning, getString(R.string.system));
      }
    }

    if (warn == null) {
      Utils.runInBg(() -> setPermission(perm));
      return;
    }

    AlertDialog dialog =
        new Builder(PackageActivity.this)
            .setPositiveButton(R.string.yes, (d, which) -> Utils.runInBg(() -> setPermission(perm)))
            .setNegativeButton(R.string.no, null)
            .setNeutralButton(
                R.string.do_not_remind,
                (d, which) -> {
                  doNotRemindDangAction();
                  Utils.runInBg(() -> setPermission(perm));
                })
            .setTitle(R.string.warning)
            .setMessage(Utils.breakParas(warn))
            .create();
    AlertDialogFragment.show(PackageActivity.this, dialog, "PERM_CHANGE_WARNING");
  }

  private void setPermission(Permission permission) {
    if (isPackageNull()) {
      return;
    }

    boolean isSystemFixed = permission.isSystemFixed();
    if (!mPkgActivityFlavor.beforePermChange(mPackage, permission, isSystemFixed)) {
      return;
    }

    Permission.setPermission(mPackage, permission);

    mPkgActivityFlavor.afterPermChange(mPackage, permission, isSystemFixed);
    updatePackage();
  }

  private boolean checkPrivileges() {
    if (SETTINGS.isPrivDaemonAlive()) {
      return true;
    }
    AlertDialogFragment.show(this, null, TAG_GRANT_ROOT_OR_ADB);
    return false;
  }

  private void doNotRemindDangAction() {
    SETTINGS.savePref(R.string.pref_package_warn_dang_change_enc_key, false);
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
  protected void onStart() {
    if (mPkgActivityFlavor != null) {
      mPkgActivityFlavor.onStart();
    }
    super.onStart();
  }

  @Override
  protected void onStop() {
    if (mPkgActivityFlavor != null) {
      mPkgActivityFlavor.onStop();
    }
    super.onStop();
  }

  @Override
  public void onBackPressed() {
    if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
      collapseSearchView();
      return;
    }
    super.onBackPressed();
  }

  private static final String CLASS = PackageActivity.class.getName();
  private static final String TAG_GRANT_ROOT_OR_ADB = CLASS + ".GRANT_ROOT_OR_ADB";
  private static final String TAG_RESET_APP_OPS_CONFIRM = CLASS + ".RESET_APP_OPS_CONFIRM";
  private static final String TAG_SET_REF_CONFIRM = CLASS + ".SET_REF_CONFIRM";
  private static final String TAG_CLEAR_REF_CONFIRM = CLASS + ".CLEAR_REF_CONFIRM";

  @Override
  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    if (TAG_GRANT_ROOT_OR_ADB.equals(tag)) {
      return new Builder(this)
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
    }

    if (TAG_RESET_APP_OPS_CONFIRM.equals(tag)) {
      if (isPackageNull()) {
        return null;
      }
      return new Builder(this)
          .setPositiveButton(R.string.yes, (d, which) -> Utils.runInBg(this::resetAppOps))
          .setNegativeButton(R.string.no, null)
          .setTitle(mPackage.getLabel())
          .setMessage(R.string.reset_app_ops_confirmation)
          .create();
    }

    if (TAG_SET_REF_CONFIRM.equals(tag)) {
      if (isPackageNull()) {
        return null;
      }
      return new Builder(this)
          .setPositiveButton(R.string.yes, (d, which) -> Utils.runInBg(this::setAllReferences))
          .setNegativeButton(R.string.no, null)
          .setTitle(mPackage.getLabel())
          .setMessage(R.string.set_references_confirmation)
          .create();
    }

    if (TAG_CLEAR_REF_CONFIRM.equals(tag)) {
      if (isPackageNull()) {
        return null;
      }
      return new Builder(this)
          .setPositiveButton(R.string.yes, (d, which) -> Utils.runInBg(this::clearReferences))
          .setNegativeButton(R.string.no, null)
          .setTitle(mPackage.getLabel())
          .setMessage(R.string.clear_references_confirmation)
          .create();
    }

    return super.createDialog(tag, dialogFragment);
  }

  private class PermAdapterCallbackImpl implements PermAdapterCallback {

    @Override
    public void onPermClick(Permission perm, Integer yLocation) {
      String permName = perm.getPermNameString();
      String protLevel = perm.getProtLevelString();

      PermDetailsDialogBinding b = PermDetailsDialogBinding.inflate(getLayoutInflater());

      b.permNameV.setText(permName);
      b.protLevelV.setText(protLevel);
      if (perm.getDescription() != null) {
        b.permDescV.setText(perm.getDescription());
        b.permDescV.setVisibility(View.VISIBLE);
      }

      AlertDialog dialog = new Builder(PackageActivity.this).setView(b.getRoot()).create();

      Window dialogWindow = dialog.getWindow();
      if (dialogWindow != null) {
        LayoutParams layoutParams = dialogWindow.getAttributes();
        layoutParams.gravity = Gravity.TOP;
        layoutParams.y = yLocation;
      }

      AlertDialogFragment.show(PackageActivity.this, dialog, "PERM_DETAILS");
    }

    @Override
    public void onPermLongClick(Permission perm) {
      String permState = getPermState(perm);
      Builder builder = new Builder(PackageActivity.this);
      builder.setPositiveButton(
          R.string.exclude,
          (dialogInterface, i) ->
              Utils.runInBg(
                  () -> {
                    SETTINGS.addPermToExcludedPerms(perm.getName());
                    updatePackage();

                    // other packages are also affected
                    PKG_PARSER.updatePackagesList();
                  }));
      builder.setNegativeButton(android.R.string.cancel, null);

      boolean isReferenced = perm.isReferenced() != null && perm.isReferenced();
      builder.setNeutralButton(
          (isReferenced ? R.string.clear_reference : R.string.set_reference),
          (dialog, which) -> {
            if (isReferenced) {
              Utils.runInBg(
                  () -> {
                    SETTINGS.getPermDb().deletePermission(mPackage.getName(), perm.getName());
                    PKG_PARSER.updatePermReferences(mPackage.getName(), perm.getName(), null);
                    updatePackage();
                    mPkgActivityFlavor.pkgRefChanged(mPackage);
                  });
              return;
            }
            PermissionEntity entity = new PermissionEntity();
            entity.isAppOps = perm.isAppOps();
            entity.permName = perm.getName();
            entity.pkgName = mPackage.getName();
            entity.state = permState;
            Utils.runInBg(
                () -> {
                  int id = SETTINGS.getPermDb().getId(entity.pkgName, entity.permName);
                  if (id > 0) {
                    entity.id = id;
                  }
                  SETTINGS.getPermDb().insertAll(entity);
                  PKG_PARSER.updatePermReferences(mPackage.getName(), perm.getName(), permState);
                  updatePackage();
                  mPkgActivityFlavor.pkgRefChanged(mPackage);
                });
          });

      // Set message, create and show the AlertDialog
      ActivityPkgPermTitleBinding b = ActivityPkgPermTitleBinding.inflate(getLayoutInflater());
      b.getRoot().setText(perm.getName());
      builder.setCustomTitle(b.getRoot());

      String message;
      if (perm.isExtraAppOp()) {
        message = getString(R.string.extra_ops_cant_be_excluded);
      } else {
        message = getString(R.string.exclude_perm_from_list);
      }
      if (perm.isChangeable()) {
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
            dialog.getButton(AlertDialog.BUTTON_NEUTRAL).setEnabled(perm.isChangeable());
            boolean canBeExcluded = SETTINGS.canBeExcluded(perm);
            dialog.getButton(AlertDialog.BUTTON_POSITIVE).setEnabled(canBeExcluded);
          });
      AlertDialogFragment.show(PackageActivity.this, dialog, "PERM_OPTIONS");
    }

    @Override
    public void onPermSwitchClick(Permission perm) {
      if (!isPackageNull() && checkPrivileges() && mPkgActivityFlavor != null) {
        mPkgActivityFlavor.onPermClick(perm);
      }
    }

    @Override
    public void onSpinnerItemSelect(Permission perm, int selectedValue) {
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
      if (SETTINGS.getBoolPref(R.string.pref_package_warn_dang_change_enc_key)) {
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
        Utils.runInBg(() -> setAppOpsMode(perm, pos, selectedValue));
        return;
      }

      String msg = "";
      if (affectedPkgCount > 1) {
        int count = affectedPkgCount - 1;
        msg = Utils.getQtyString(R.plurals.uid_mode_app_ops_warning, count, count);
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
                    Utils.runInBg(() -> setAppOpsMode(perm, pos, selectedValue));
                  })
              .setNegativeButton(R.string.no, null)
              .setTitle(R.string.warning)
              .setMessage(Utils.breakParas(msg));

      if (affectedPkgCount <= 1) {
        builder.setNeutralButton(
            R.string.do_not_remind,
            (dialog, which) -> {
              isYes[0] = true;
              Utils.runInBg(() -> setAppOpsMode(perm, pos, selectedValue));
              doNotRemindDangAction();
            });
      }

      // UpdateSpinner on Dialog dismiss also suffices for Negative and Neutral buttons
      AlertDialogFragment.show(PackageActivity.this, builder.create(), "PERM_CHANGE_WARNING")
          .setOnDismissListener(
              dialog -> {
                if (!isYes[0]) {
                  updateSpinnerSelectionInBg(pos);
                }
              });
    }

    @Override
    public void runInFg(Runnable task) {
      Utils.runInFg(PackageActivity.this, task);
    }
  }
}
