package com.mirfatif.permissionmanagerx.pkg;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.Activity;
import android.content.Intent;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.SystemClock;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import androidx.activity.OnBackPressedCallback;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.widget.SearchView;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.ActivityPackageBinding;
import com.mirfatif.permissionmanagerx.fwk.FilterSettingsActivityM;
import com.mirfatif.permissionmanagerx.fwk.MainActivityM;
import com.mirfatif.permissionmanagerx.fwk.PackageActivityM;
import com.mirfatif.permissionmanagerx.main.MainActivity;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermsDb;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermsDbFlavor;
import com.mirfatif.permissionmanagerx.pkg.PermissionAdapter.PermAdapterCallback;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.StringUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.UserUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveBgTask;
import com.mirfatif.permissionmanagerx.util.bg.LiveSchedTask;
import com.mirfatif.permissionmanagerx.util.bg.LiveSingleParamTask;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.permissionmanagerx.util.bg.LiveUiTask;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class PackageActivity extends OnBackPressedCallback implements PermAdapterCallback {

  private static final String TAG = "PackageActivity";

  private static final String CLASS = PackageActivity.class.getName();
  private static final String EXTRA_PKG_POSITION = CLASS + ".extra.PKG_POSITION";
  private static final String EXTRA_PERM_FILTER = CLASS + ".extra.PERM_FILTER";

  public final PackageActivityM mA;

  private final PkgActivityFlavor mActFlavor = new PkgActivityFlavor(this);
  private final LiveSchedTask mRefreshStopper;
  private final LiveUiTask mPostPkgUpdateTask;
  private final LiveUiTask mPostPermListUpdateTask;
  private final LiveBgTask mPermListSortTask;

  private String mPermFilter;
  private boolean mFilterPerms = true;

  public PackageActivity(PackageActivityM activity) {
    super(true);
    mA = activity;
    mRefreshStopper =
        new LiveSchedTask(
            mA,
            () -> mB.refreshLayout.setRefreshing(false),
            500,
            TimeUnit.MILLISECONDS,
            true,
            TAG + "-RefreshStopper");
    mPostPkgUpdateTask = new LiveUiTask(mA, this::postPkgUpdate);
    mPostPermListUpdateTask = new LiveUiTask(mA, this::postPermListUpdate);
    mSearchExecutor =
        new LiveSingleParamTask<>(mA, this::handleSearchQuery, TAG + "-SearchExecutor");
    mPermListSortTask = new LiveBgTask(mA, this::sortPermList);
  }

  private Package mPkg;
  private final List<Permission> mSortedPermList = Collections.synchronizedList(new ArrayList<>());
  private PermissionAdapter mAdapter;
  private ActivityPackageBinding mB;

  public void onCreated() {
    int pos = mA.getIntent().getIntExtra(EXTRA_PKG_POSITION, -1);
    if (pos == -1 || (mPkg = PackageParser.INS.getPkg(pos)) == null) {
      UiUtils.showToast(R.string.something_went_wrong);
      mA.finishAfterTransition();
      return;
    }

    mPkg.setPkgRemoved(false);

    mB = ActivityPackageBinding.inflate(mA.getLayoutInflater());
    mA.setContentView(mB);

    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(mPkg.getLabel());
    }

    mAdapter = new PermissionAdapter(mA, this, mA);

    mB.recyclerV.setAdapter(mAdapter);

    LinearLayoutManager layoutManager =
        new LinearLayoutManager(mA, LinearLayoutManager.VERTICAL, false);
    mB.recyclerV.setLayoutManager(layoutManager);

    mB.recyclerV.addItemDecoration(new DividerItemDecoration(mA, LinearLayoutManager.VERTICAL));

    mPermFilter = mA.getIntent().getStringExtra(EXTRA_PERM_FILTER);

    mPermListSortTask.execute();

    mB.refreshLayout.setOnRefreshListener(() -> BgRunner.execute(this::updatePkg));

    mA.getOnBackPressedDispatcher().addCallback(mA, this);
  }

  public boolean onCreateOptionsMenu(Menu menu) {
    mA.getMenuInflater().inflate(R.menu.package_menu, menu);
    if (VERSION.SDK_INT >= VERSION_CODES.P) {
      menu.setGroupDividerEnabled(true);
    }

    menu.findItem(R.id.action_reset_app_ops).setVisible(AppOpsParser.INS.hasAppOps());
    menu.findItem(R.id.action_show_all_perms).setChecked(!mFilterPerms);

    MenuItem searchMenuItem = menu.findItem(R.id.action_search);
    mSearchView = (SearchView) searchMenuItem.getActionView();
    Objects.requireNonNull(mSearchView).setMaxWidth(Integer.MAX_VALUE);

    mSearchView.setOnQueryTextListener(
        new SearchView.OnQueryTextListener() {
          public boolean onQueryTextSubmit(String query) {
            submitPermList();
            return true;
          }

          public boolean onQueryTextChange(String newText) {
            submitPermList();
            return true;
          }
        });

    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          if (!hasFocus && TextUtils.isEmpty(mSearchView.getQuery())) {
            collapseSearchView();
          }
        });

    mSearchView.setQueryHint(getString(R.string.search_menu_item));

    menu.findItem(R.id.action_show_all_perms)
        .setVisible(!MySettings.INS.isDeepSearching() && mPkg.getPermFilter() == null);

    mActFlavor.onCreateOptionsMenu(menu);
    return true;
  }

  public boolean onPrepareOptionsMenu(Menu menu) {
    boolean havePerms = !mSortedPermList.isEmpty();
    menu.findItem(R.id.action_search).setVisible(havePerms);
    menu.findItem(R.id.action_reset_app_ops).setVisible(havePerms);
    menu.findItem(R.id.action_set_all_references).setVisible(havePerms);
    menu.findItem(R.id.action_clear_references).setVisible(havePerms);
    mActFlavor.onPrepareOptionsMenu(menu, havePerms);
    return true;
  }

  public boolean onOptionsItemSelected(MenuItem item) {
    int itemId = item.getItemId();

    if (itemId == R.id.action_reset_app_ops && isDaemonAlive()) {
      AlertDialogFragment.show(mA, null, TAG_RESET_APP_OPS_CONFIRM);
      return true;
    }

    if (itemId == R.id.action_set_all_references) {
      AlertDialogFragment.show(mA, null, TAG_SET_REF_CONFIRM);
      return true;
    }

    if (itemId == R.id.action_clear_references) {
      AlertDialogFragment.show(mA, null, TAG_CLEAR_REF_CONFIRM);
      return true;
    }

    if (itemId == R.id.action_show_all_perms) {
      item.setChecked(!item.isChecked());
      mFilterPerms = !item.isChecked();
      BgRunner.execute(this::updatePkg);
      return true;
    }

    return mActFlavor.onOptionsItemSelected(item);
  }

  public void onResume() {
    BgRunner.execute(this::updatePkg);
  }

  public void onStart() {
    mActFlavor.onStart();
  }

  public void onStop() {
    mActFlavor.onStop();
  }

  public void handleOnBackPressed() {
    if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
      collapseSearchView();
    } else {
      mA.finishAfterTransition();
    }
  }

  private static final String TAG_GRANT_ROOT_OR_ADB = CLASS + ".GRANT_ROOT_OR_ADB";
  private static final String TAG_RESET_APP_OPS_CONFIRM = CLASS + ".RESET_APP_OPS_CONFIRM";
  private static final String TAG_SET_REF_CONFIRM = CLASS + ".SET_REF_CONFIRM";
  private static final String TAG_CLEAR_REF_CONFIRM = CLASS + ".CLEAR_REF_CONFIRM";

  public AlertDialog createDialog(String tag) {
    if (TAG_GRANT_ROOT_OR_ADB.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(
              android.R.string.ok,
              (d, which) -> {
                mA.startActivity(
                    new Intent(App.getCxt(), MainActivityM.class)
                        .setAction(MainActivity.ACTION_SHOW_DRAWER)
                        .setFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT));
                mA.finishAfterTransition();
              })
          .setNegativeButton(android.R.string.cancel, null)
          .setTitle(R.string.privileges_title)
          .setMessage(R.string.grant_root_or_adb_to_change_perms)
          .create();
    }

    if (TAG_RESET_APP_OPS_CONFIRM.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(R.string.yes, (d, w) -> BgRunner.execute(this::resetAppOps))
          .setNegativeButton(R.string.no, null)
          .setTitle(mPkg.getLabel())
          .setMessage(R.string.reset_app_ops_confirmation)
          .create();
    }

    if (TAG_SET_REF_CONFIRM.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(R.string.yes, (d, w) -> BgRunner.execute(this::setAllReferences))
          .setNegativeButton(R.string.no, null)
          .setTitle(mPkg.getLabel())
          .setMessage(R.string.set_references_confirmation)
          .create();
    }

    if (TAG_CLEAR_REF_CONFIRM.equals(tag)) {
      return new Builder(mA)
          .setPositiveButton(R.string.yes, (d, w) -> BgRunner.execute(this::clearReferences))
          .setNegativeButton(R.string.no, null)
          .setTitle(mPkg.getLabel())
          .setMessage(R.string.clear_references_confirmation)
          .create();
    }

    return null;
  }

  public void onPermClick(Permission perm, int yLocation) {
    new PermDetailDialog(this).show(perm, yLocation);
  }

  public void onPermLongClick(Permission perm) {
    PermLongPressDialogFrag.show(perm, mPkg, mActFlavor, mA.getSupportFragmentManager());
  }

  public void onPermSwitchToggle(Permission perm) {
    if (perm.isAppOp()) {
      onAppOpModeSelect(perm, Permission.getAppOpMode(!perm.isGranted()));
    } else if (mActFlavor.onPermClick(perm)) {
      onManifestPermStateChanged(perm);
    }
  }

  void updatePkg() {
    if (!mPkg.isRemoved()) {
      PackageParser.INS.updatePackage(mPkg, mFilterPerms);
    }

    if (!mPkg.isRemoved()) {
      sortPermList();
    }

    mPostPkgUpdateTask.post(true);
  }

  private void postPkgUpdate() {
    if (mPkg.isRemoved()) {
      mA.finishAfterTransition();
    }
  }

  private void sortPermList() {
    mSortedPermList.clear();
    mSortedPermList.addAll(mPkg.getPermList());
    if (mFilterPerms && mPermFilter != null) {
      synchronized (mSortedPermList) {
        mSortedPermList.removeIf(perm -> !perm.getName().equals(mPermFilter));
      }
    }
    mActFlavor.sortPermsList(mSortedPermList);
    mPostPermListUpdateTask.post(true);
  }

  private void postPermListUpdate() {
    submitPermList();
    checkEmptyPermissionsList();
  }

  private void checkEmptyPermissionsList() {
    if (mSortedPermList.isEmpty()) {
      String message;
      if (mPkg.getTotalPermCount() != 0) {
        int cnt = mPkg.getTotalPermCount();
        message = ApiUtils.getQtyString(R.plurals.count_permissions_filtered_out, cnt, cnt);
        mB.settingsButton.setOnClickListener(
            v -> mA.startActivity(new Intent(App.getCxt(), FilterSettingsActivityM.class)));
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
    mA.invalidateOptionsMenu();
  }

  private SearchView mSearchView;

  private void collapseSearchView() {
    if (mSearchView != null) {
      mSearchView.onActionViewCollapsed();
      mSearchView.setQuery(null, false);
    }
    submitPermList();
  }

  private final LiveSingleParamTask<String> mSearchExecutor;

  private void submitPermList() {
    CharSequence queryText = mSearchView == null ? null : mSearchView.getQuery();
    if (queryText == null || TextUtils.isEmpty(queryText)) {
      submitList(mSortedPermList);
    } else {
      mB.refreshLayout.setRefreshing(true);
      mSearchExecutor.cancelAndSubmit(queryText.toString(), true);
    }
  }

  private void handleSearchQuery(String queryText) {
    synchronized (mSortedPermList) {
      List<Permission> permList =
          mSortedPermList.parallelStream()
              .filter(perm -> perm.contains(mPkg, queryText, false))
              .collect(Collectors.toList());

      if (!Thread.interrupted()) {
        submitList(permList);
      }
    }
  }

  private void submitList(List<Permission> permList) {
    mAdapter.submitList(new ArrayList<>(permList));
    mRefreshStopper.cancelAndSchedule();
  }

  private boolean isDaemonAlive() {
    if (DaemonHandler.INS.isDaemonAlive()) {
      return true;
    }
    AlertDialogFragment.show(mA, null, TAG_GRANT_ROOT_OR_ADB);
    return false;
  }

  private String createDangPermChangeWarn() {
    if (MySettings.INS.warnDangerousPermChanges()) {
      if (mPkg.isFrameworkApp()) {
        return getString(R.string.change_perms_warning, getString(R.string.framework));
      } else if (mPkg.isSystemApp()) {
        return getString(R.string.change_perms_warning, getString(R.string.system));
      }
    }
    return null;
  }

  private void setAllReferences() {
    List<PermissionEntity> entities = new ArrayList<>();
    buildRefsFromCurrentPermStates(mPkg, mSortedPermList, entities);

    PermsDb.INS.updateRefsDb(entities.toArray(new PermissionEntity[0]));
    for (PermissionEntity e : entities) {
      PermsDb.INS.updateRefs(e.pkgName, e.permName, e.state, e.isAppOps, e.isPerUid, e.userId);
    }

    mActFlavor.pkgRefChanged(mPkg);
    updatePkg();
  }

  public static void buildRefsFromCurrentPermStates(
      Package pkg, List<Permission> permList, List<PermissionEntity> entities) {
    int userId = PermsDbFlavor.getUserIdForPermRefs(pkg.getUid());

    for (Permission perm : permList) {
      if (!Boolean.TRUE.equals(perm.isReferenced()) && perm.isChangeable()) {
        entities.add(
            new PermissionEntity(
                pkg.getName(),
                perm.getName(),
                perm.createRefStringForDb(),
                perm.isAppOp(),
                MySettings.INS.useUniqueRefForAppOpUidMode() && perm.isPerUid(),
                userId));
      }
    }
  }

  private void clearReferences() {
    int userId = PermsDbFlavor.getUserIdForPermRefs(mPkg.getUid());

    PermsDb.INS.getDb().deletePkg(mPkg.getName(), userId);

    synchronized (mSortedPermList) {
      mSortedPermList.forEach(
          perm ->
              PermsDb.INS.updateRefs(
                  mPkg.getName(),
                  perm.getName(),
                  null,
                  perm.isAppOp(),
                  MySettings.INS.useUniqueRefForAppOpUidMode() && perm.isPerUid(),
                  userId));
    }

    mActFlavor.pkgRefChanged(mPkg);
    updatePkg();
  }

  void onAppOpModeSelect(Permission appOp, int mode) {
    if (isDaemonAlive()) {
      new LiveTasksQueueTyped<>(mA, () -> getAffectedPkgCount(appOp))
          .onUiWith(count -> onAppOpModeSelect(appOp, mode, count))
          .start();
    }
  }

  private Integer getAffectedPkgCount(Permission appOp) {
    if (!appOp.isPerUid()) {
      return 0;
    }

    String[] pkgs = UserUtils.getPackagesForUid(mPkg.getUid());
    return pkgs == null ? null : pkgs.length;
  }

  private void onAppOpModeSelect(Permission appOp, int mode, Integer affectedPkgCount) {
    if (affectedPkgCount == null) {
      return;
    }

    String warn = createDangPermChangeWarn();

    if (warn == null && (!appOp.isPerUid() || affectedPkgCount <= 1)) {
      BgRunner.execute(() -> setAppOpMode(appOp, mode));
      return;
    }

    StringBuilder msg = new StringBuilder();
    if (affectedPkgCount > 1) {
      int count = affectedPkgCount - 1;
      msg.append(ApiUtils.getQtyString(R.plurals.uid_mode_app_ops_warning, count, count));
      if (warn == null) {
        msg.append("\n").append(getString(R.string._continue));
      }
    }

    if (warn != null) {
      if (msg.length() != 0) {
        msg.append("\n");
      }
      msg.append(warn);
    }

    Builder builder =
        new Builder(mA)
            .setTitle(R.string.warning)
            .setMessage(StringUtils.breakParas(msg.toString()))
            .setPositiveButton(
                R.string.yes, (d, w) -> BgRunner.execute(() -> setAppOpMode(appOp, mode)))
            .setNegativeButton(R.string.no, null);

    if (affectedPkgCount <= 1) {
      builder.setNeutralButton(
          R.string.do_not_remind,
          (d, w) -> {
            BgRunner.execute(() -> setAppOpMode(appOp, mode));
            MySettings.INS.disableWarnDangerousPermChanges();
          });
    }

    AlertDialogFragment.show(mA, builder.create(), "PERM_CHANGE_WARNING");
  }

  private void setAppOpMode(Permission appOp, int mode) {
    if (!AppOpsParser.INS.isValidAppOpMode(mode)) {
      UiUtils.showToast(R.string.something_went_wrong);
      return;
    }

    mActFlavor.beforeAppOpChange(mPkg, appOp, mode);

    appOp.setAppOpMode(mPkg, mode);

    updatePkg();
    if (mPkg.isRemoved()) {
      return;
    }

    SystemClock.sleep(1000);
    updatePkg();

    synchronized (mSortedPermList) {
      for (Permission perm : mSortedPermList) {
        if (perm.isSamePerm(appOp)) {
          if (perm.getAppOpMode() == appOp.getAppOpMode()) {
            UiUtils.showToast(R.string.app_op_mode_not_chaged_toast);
          }
          break;
        }
      }
    }
  }

  private void resetAppOps() {
    DaemonIface.INS.resetAppOps(UserUtils.getUserId(mPkg.getUid()), mPkg.getName());
    updatePkg();
  }

  void onManifestPermStateChanged(Permission perm) {
    if (!isDaemonAlive()) {
      return;
    }

    String warn = createDangPermChangeWarn();

    if (warn == null) {
      BgRunner.execute(() -> setPermission(perm));
      return;
    }

    AlertDialog dialog =
        new Builder(mA)
            .setPositiveButton(
                R.string.yes, (d, which) -> BgRunner.execute(() -> setPermission(perm)))
            .setNegativeButton(R.string.no, null)
            .setNeutralButton(
                R.string.do_not_remind,
                (d, which) -> {
                  MySettings.INS.disableWarnDangerousPermChanges();
                  BgRunner.execute(() -> setPermission(perm));
                })
            .setTitle(R.string.warning)
            .setMessage(StringUtils.breakParas(warn))
            .create();

    AlertDialogFragment.show(mA, dialog, "PERM_CHANGE_WARNING");
  }

  private void setPermission(Permission perm) {
    Boolean before = mActFlavor.beforePermChange(mPkg, perm);
    if (before == null) {
      return;
    }

    perm.toggleState(mPkg);

    mActFlavor.afterPermChange(mPkg, perm, before);
    updatePkg();
  }

  public static void start(Activity activity, Package pkg, String permFilter) {
    Intent intent = new Intent(App.getCxt(), PackageActivityM.class);
    intent.putExtra(EXTRA_PKG_POSITION, PackageParser.INS.getPkgPosition(pkg));
    if (permFilter != null) {
      intent.putExtra(EXTRA_PERM_FILTER, permFilter);
    }
    activity.startActivity(intent);
  }
}
