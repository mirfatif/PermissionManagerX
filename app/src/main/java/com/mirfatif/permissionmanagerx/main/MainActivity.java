package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getQtyString;
import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;
import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.appcompat.widget.SearchView;
import androidx.appcompat.widget.SearchView.OnQueryTextListener;
import androidx.coordinatorlayout.widget.CoordinatorLayout;
import androidx.core.view.GravityCompat;
import androidx.core.view.MenuCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.about.AboutActivity;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.ActivityMainBinding;
import com.mirfatif.permissionmanagerx.databinding.DilogTitleWithHelpBinding;
import com.mirfatif.permissionmanagerx.fwk.AdvSettingsActivityM;
import com.mirfatif.permissionmanagerx.fwk.FilterSettingsActivityM;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.permissionmanagerx.fwk.MainActivityM;
import com.mirfatif.permissionmanagerx.fwk.MoveUpBehavior;
import com.mirfatif.permissionmanagerx.fwk.SettingsActivityM;
import com.mirfatif.permissionmanagerx.help.HelpActivity;
import com.mirfatif.permissionmanagerx.main.PackageAdapter.PkgAdapterCallback;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.pkg.PackageActivity;
import com.mirfatif.permissionmanagerx.prefs.AppUpdate;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.settings.SearchSettingsFrag;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonStarter;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.LocaleUtils;
import com.mirfatif.permissionmanagerx.util.NotifUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.UserUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveMinDelayParamTask;
import com.mirfatif.permissionmanagerx.util.bg.LiveSchedParamTask;
import com.mirfatif.permissionmanagerx.util.bg.LiveSchedTask;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueue;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.privtasks.util.bg.BgRunner;
import com.mirfatif.privtasks.util.bg.NotifyWaiter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class MainActivity extends OnBackPressedCallback {

  private static final String TAG = "MainActivity";

  public final MainActivityM mA;

  public MainActivity(MainActivityM activity) {
    super(true);
    mA = activity;

    mPkgCountNotifier =
        new LiveSchedParamTask<>(
            mA, this::showPkgCount, 500, TimeUnit.MILLISECONDS, true, TAG + "-PkgCountNotifier");

    mBigProgHider =
        new LiveMinDelayParamTask<>(
            mA,
            visibility -> mB.bigProgCont.setVisibility(visibility),
            1,
            TimeUnit.SECONDS,
            true,
            TAG + "-BigProgHider");

    mWindowWaiter = new NotifyWaiter(() -> mA.getWindow() == null);
  }

  private static final String CLASS = MainActivity.class.getName();
  public static final String ACTION_SHOW_DRAWER = CLASS + ".action.SHOW_DRAWER";

  public final DaemonStartProg mDaemonStartProg = new DaemonStartProg(this);
  private final Feedback mFeedback = new Feedback(this);
  private final BackupRestoreDialog mBackupRestoreDialog = new BackupRestoreDialog(this);
  private final PrivsCheckBoxFocus mCheckBoxFocus = new PrivsCheckBoxFocus(this);

  private ActivityResultLauncher<String> mNotifPermReqLauncher;

  ActivityMainBinding mB;

  private LinearLayoutManager mLayoutManager;
  public SearchView mSearchView;
  private PackageAdapter mPkgAdapter;

  ActionBarDrawerToggle mDrawerToggle;

  private final Class<?> mSnackBarLayoutCls = Snackbar.SnackbarLayout.class;

  public void onCreated() {
    if (isSecondaryUser()) {
      return;
    }

    mB = ActivityMainBinding.inflate(mA.getLayoutInflater());
    mB.movCont.setData(new Data());
    mA.setContentView(mB);

    ((CoordinatorLayout.LayoutParams) mB.moveUpCont.getLayoutParams())
        .setBehavior(new MoveUpBehavior(mSnackBarLayoutCls, mB.movCont.getRoot()));

    mB.bigProgCont.setOnClickListener(null);

    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      actionBar.setDisplayHomeAsUpEnabled(true);
    }

    mDrawerToggle =
        new ActionBarDrawerToggle(mA, mB.getRoot(), R.string.open_drawer, R.string.close_drawer);
    mB.getRoot().addDrawerListener(mDrawerToggle);
    mDrawerToggle.syncState();

    handleIntentActions(mA.getIntent());

    mB.navV.setNavigationItemSelectedListener(this::handleNavigationItemSelected);
    setNavigationMenu();

    MySettings.INS.mPrefsWatcher.observe(mA, this::onPrefChanged);

    mB.refreshLayout.setOnRefreshListener(
        () -> {
          if (MySettings.INS.isSearching()) {
            handleSearchQuery();
          } else {
            PackageParser.INS.updatePkgList();
          }
        });

    mPkgAdapter = new PackageAdapter(mA, new PkgAdapterCallbackImpl());

    mB.recyclerView.setAdapter(mPkgAdapter);

    mLayoutManager = new LinearLayoutManager(mA, LinearLayoutManager.VERTICAL, false);
    mB.recyclerView.setLayoutManager(mLayoutManager);

    mB.recyclerView.addItemDecoration(new DividerItemDecoration(mA, LinearLayoutManager.VERTICAL));

    mB.recyclerView.setOnScrollChangeListener(
        (v, scrollX, scrollY, oldScrollX, oldScrollY) -> setRepeatUpdates());

    if (mSearchView != null) {
      collapseSearchView();
    } else {
      MySettings.INS.setQueryText(null);
    }

    mB.searchSettingsContainer.setOnClickListener(v -> hideSearchSettings());

    String action = mA.getIntent().getAction();

    if (Intent.ACTION_MAIN.equals(action)) {
      BgRunner.execute(MySettings.INS::plusAppLaunchCount);
    }

    mNotifPermReqLauncher =
        mA.registerForActivityResult(
            new ActivityResultContracts.RequestPermission(), granted -> {});

    mDaemonStartProg.onCreated(action);
    mBackupRestoreDialog.onCreated();

    if (ApiUtils.hasNotifPerm()) {
      BgRunner.execute(() -> AppUpdate.check(true));
    }

    mA.getOnBackPressedDispatcher().addCallback(mA, this);
  }

  public void onNewIntent(Intent intent) {
    handleIntentActions(intent);
  }

  public boolean onCreateOptionsMenu(Menu menu) {
    if (UserUtils.getUserId() != 0) {
      return false;
    }

    mA.getMenuInflater().inflate(R.menu.main_search, menu);
    MenuCompat.setGroupDividerEnabled(menu, true);

    MenuItem searchMenuItem = menu.findItem(R.id.action_search);
    mSearchView = (SearchView) searchMenuItem.getActionView();
    setUpSearchView();

    return true;
  }

  public boolean onOptionsItemSelected(MenuItem item) {
    if (item.getItemId() == android.R.id.home) {
      if (!mSearchView.isIconified()) {
        if (mB.searchSettingsContainer.getVisibility() == View.VISIBLE) {
          hideSearchSettings();
        } else {
          mB.searchSettingsContainer.setVisibility(View.VISIBLE);
          mA.getSupportFragmentManager()
              .beginTransaction()
              .replace(R.id.search_settings_frag, new SearchSettingsFrag())
              .commit();
        }
        return true;
      }
    }

    return mDrawerToggle.onOptionsItemSelected(item);
  }

  public void handleOnBackPressed() {
    mCheckBoxFocus.endFocus();

    if (mB != null && mB.getRoot().isDrawerOpen(GravityCompat.START)) {
      mB.getRoot().closeDrawer(GravityCompat.START, true);
      return;
    }

    if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
      collapseSearchView();
      return;
    }

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      mExited = true;
    }

    mA.finishAfterTransition();
  }

  private boolean mExited = false;

  public void onResume() {
    mFeedback.askForFeedback();
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S && mExited) {
      PackageParser.INS.updatePkgList();
    }
    mExited = false;
  }

  public void onPause() {
    mCheckBoxFocus.endFocus();
  }

  public void onAttachedToWindow() {
    mWindowWaiter.notify(true);
  }

  public static final String TAG_PRIVS_REQ_FOR_DAEMON = CLASS + ".PRIVS_REQ_FOR_DAEMON";
  public static final String TAG_GRANT_ROOT_OR_ADB = CLASS + ".GRANT_ROOT_OR_ADB";
  public static final String TAG_ADB_CONNECTION = CLASS + ".TAG_ADB_CONNECTION";
  private static final String TAG_BACKUP_RESTORE = CLASS + ".TAG_BACKUP_RESTORE";

  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    if (TAG_PRIVS_REQ_FOR_DAEMON.equals(tag) || TAG_GRANT_ROOT_OR_ADB.equals(tag)) {
      DilogTitleWithHelpBinding b = DilogTitleWithHelpBinding.inflate(mA.getLayoutInflater());
      b.titleV.setText(R.string.privileges_title);
      b.helpV.setOnClickListener(v -> HelpActivity.start(mA, "faq1"));

      Builder builder =
          new Builder(mA)
              .setPositiveButton(android.R.string.ok, (d, w) -> openDrawerForPrivileges())
              .setCustomTitle(b.getRoot())
              .setMessage(getString(R.string.grant_root_or_adb));
      if (TAG_GRANT_ROOT_OR_ADB.equals(tag)) {
        builder.setNegativeButton(android.R.string.cancel, null);
      } else {
        builder.setNeutralButton(
            R.string.do_not_remind, (d, which) -> MySettings.INS.setPrivReminderOff());
      }
      return UiUtils.removeButtonPadding(builder.create());
    }

    if (TAG_ADB_CONNECTION.equals(tag)) {
      return new AdbConnectDialog(this, dialogFragment).createDialog();
    }

    if (TAG_BACKUP_RESTORE.equals(tag)) {
      return mBackupRestoreDialog.createDialog();
    }

    return null;
  }

  private boolean isSecondaryUser() {
    if (UserUtils.getUserId() == 0) {
      return false;
    }

    Builder builder =
        new Builder(mA)
            .setPositiveButton(android.R.string.ok, null)
            .setTitle(R.string.primary_account)
            .setMessage(R.string.primary_profile_only);
    AlertDialogFragment.show(mA, builder.create(), "PRIMARY_PROFILE")
        .setOnDismissListener(d -> mA.finishAfterTransition());

    return true;
  }

  private class PkgAdapterCallbackImpl implements PkgAdapterCallback {

    public void onClick(Package pkg) {
      PackageActivity.start(mA, pkg, null);
    }

    public void onLongClick(Package pkg) {
      new PkgLongPressDialogFrag(pkg).show(mA.getSupportFragmentManager(), "PKG_OPTIONS");
    }
  }

  private boolean mObserversSet = false;

  public synchronized void setLiveDataObservers() {
    if (mObserversSet) {
      return;
    }

    PackageParser.INS.getProgMax().observe(mA, this::setMaxProg);
    PackageParser.INS.getProgNow().observe(mA, this::setNowProg);
    PackageParser.INS.getListCompleted().observe(mA, this::onListCompleted);
    PackageParser.INS.getPkgListLive().observe(mA, this::pkgListReceived);
    PackageParser.INS.getChangedPkg().observe(mA, this::pkgChanged);

    mObserversSet = true;

    PackageParser.INS.updatePkgListIfChanged();
  }

  private void setMaxProg(int progMax) {
    if (progMax < 0) {
      TextView progTv;

      if (mB.bigProgCont.getVisibility() == View.VISIBLE) {
        progTv = mB.bigProgText;
      } else {
        progTv = mB.movCont.progNowV;
        mB.movCont.progBar.setIndeterminate(true);
        mB.movCont.progMaxV.setText("");
        setProgVisible(true);
      }

      progTv.setText(PackageParser.INS.getProgMsg(progMax));
      return;
    }

    mB.movCont.progBar.setIndeterminate(false);
    mB.movCont.progBar.setProgress(0);
    mB.movCont.progNowV.setText(LocaleUtils.toLocalizedNum(0));
    mB.movCont.progBar.setMax(progMax);
    mB.movCont.progMaxV.setText(LocaleUtils.toLocalizedNum(progMax));

    setBigProgVisible(false);
    setProgVisible(true);
  }

  private void setNowProg(int progNow) {
    mB.movCont.progBar.setProgress(progNow, true);
    mB.movCont.progNowV.setText(LocaleUtils.toLocalizedNum(progNow));
  }

  private boolean mFirstListCompleted = true;
  private final LiveSchedParamTask<Integer> mPkgCountNotifier;

  private void onListCompleted(int pkgCount) {
    setProgVisible(false);

    if (((mB.refreshLayout.isRefreshing() || MySettings.INS.isSearching()) && pkgCount >= 0)
        || pkgCount == 0) {
      mPkgCountNotifier.cancelAndSchedule(pkgCount);
    }

    mB.refreshLayout.setRefreshing(false);

    if (mFirstListCompleted) {
      mFirstListCompleted = false;

      LiveSchedTask.schedule(
          mA, this::askForFeedback, 5, TimeUnit.SECONDS, true, TAG + "-FeedbackPrompt");

      showAppOpsPrivsToast();

      if (DaemonHandler.INS.isDaemonAlive()
          && !ApiUtils.hasNotifPerm()
          && MySettings.INS.shouldAskForNotifPerm()) {
        NotifUtils.askForNotifPerm(mA, mNotifPermReqLauncher);
      }
    }
  }

  private void pkgListReceived(List<Package> packages) {
    mPkgAdapter.submitList(new ArrayList<>(packages));
    setRepeatUpdates();
  }

  private void pkgChanged(Package pkg) {
    int position = mPkgAdapter.getCurrentList().indexOf(pkg);
    if (position != -1) {
      mPkgAdapter.notifyItemChanged(position);
    }
  }

  private void setRepeatUpdates() {
    boolean rep = mPkgAdapter.getItemCount() < mLayoutManager.findLastVisibleItemPosition() + 5;
    PackageParser.INS.setRepeatUpdates(rep);
  }

  private void onPrefChanged(Integer pref) {
    switch (pref) {
      case MySettings.PREF_DRAWER_CHANGED -> setBoxesChecked();
      case MySettings.PREF_UI_CHANGED -> mA.recreate();
    }
  }

  private void showPkgCount(int pkgCount) {
    showSnackBar(getQtyString(R.plurals.apps_count, pkgCount, pkgCount), 5);
  }

  public void showSnackBar(String text, int sec) {
    createSnackBar(text, sec).show();
  }

  public Snackbar createSnackBar(String text, int sec) {
    return UiUtils.createSnackBar(mA, mB.movCont.progBarCont, null, text, sec);
  }

  private void handleIntentActions(Intent intent) {
    String action = intent.getAction();
    if (action != null) {
      if (action.equals(ACTION_SHOW_DRAWER)) {
        openDrawerForPrivileges();
      }
    }
  }

  private final LiveMinDelayParamTask<Integer> mBigProgHider;

  public void setBigProgVisible(boolean visible) {
    if (visible) {
      mBigProgHider.cancelAndRunNow(View.VISIBLE);
    } else {
      mBigProgHider.cancelAndRunOrSchedule(View.GONE);
    }
  }

  public void setProgVisible(boolean visible) {
    mB.movCont.progBarCont.setVisibility(visible ? View.VISIBLE : View.GONE);
  }

  private boolean mNoAppOpsToasted = false;

  private void showAppOpsPrivsToast() {
    FragmentManager fm = mA.getSupportFragmentManager();
    if (!mNoAppOpsToasted
        && !MySettings.INS.excludeAppOpsPerms()
        && !AppOpsParser.INS.canReadAppOps()
        && !MySettings.INS.isRootEnabled()
        && !MySettings.INS.isAdbEnabled()
        && fm.findFragmentByTag(TAG_GRANT_ROOT_OR_ADB) == null
        && fm.findFragmentByTag(TAG_PRIVS_REQ_FOR_DAEMON) == null) {
      UiUtils.showToast(R.string.no_app_ops_without_daemon_toast);
      mNoAppOpsToasted = true;
    }
  }

  private void askForFeedback() {
    MySettings.INS.setMayAskForFeedback(true);
    LifecycleWatcher.addOnDestroyed(mA, () -> MySettings.INS.setMayAskForFeedback(false));
    mFeedback.askForFeedback();
  }

  private void setUpSearchView() {
    mSearchView.setOnQueryTextListener(
        new OnQueryTextListener() {
          public boolean onQueryTextSubmit(String query) {
            handleSearchQuery();
            return true;
          }

          public boolean onQueryTextChange(String newText) {
            handleSearchQuery();
            return true;
          }
        });

    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          ActionBar actionBar = mA.getSupportActionBar();
          if (actionBar != null) {
            Drawable icon = AppCompatResources.getDrawable(mA, R.drawable.search_settings);
            if (icon != null) {
              actionBar.setHomeAsUpIndicator(icon);
            }
          }

          mB.getRoot().closeDrawer(GravityCompat.START, true);
          if (!hasFocus && TextUtils.isEmpty(mSearchView.getQuery())) {
            collapseSearchView();
          }
        });

    mSearchView.setQueryHint(getString(R.string.search_menu_item));
    mSearchView.setMaxWidth(Integer.MAX_VALUE);
  }

  private void collapseSearchView() {
    mSearchView.onActionViewCollapsed();
    mSearchView.setQuery(null, false);
    handleSearchQuery();

    ActionBar actionBar = mA.getSupportActionBar();
    ActionBarDrawerToggle drawerToggle;
    if (actionBar != null && (drawerToggle = mDrawerToggle) != null) {
      actionBar.setHomeAsUpIndicator(drawerToggle.getDrawerArrowDrawable());
    }
  }

  private void hideSearchSettings() {
    int delay = 0;
    FragmentManager fm = mA.getSupportFragmentManager();
    Fragment frag = fm.findFragmentById(R.id.search_settings_frag);
    if (frag != null) {
      fm.beginTransaction().remove(frag).commit();
      delay = 50;
    }
    mB.searchSettingsContainer.postDelayed(
        () -> mB.searchSettingsContainer.setVisibility(View.GONE), delay);
  }

  private void handleSearchQuery() {
    handleSearchQuery(true);
  }

  public void handleSearchQuery(boolean hideSettings) {
    if (hideSettings) {
      hideSearchSettings();
    }

    CharSequence queryText = mSearchView.getQuery();
    boolean wasSearching = MySettings.INS.isSearching();

    MySettings.INS.setQueryText(queryText == null ? null : queryText.toString());

    if (!MySettings.INS.isSearching() && !wasSearching) {
      return;
    }

    mPkgCountNotifier.cancel();

    mB.refreshLayout.setRefreshing(
        !MySettings.INS.isDeepSearchEnabled() || !MySettings.INS.isSearching());

    PackageParser.INS.setRepeatUpdates(true);

    PackageParser.INS.handleSearchQuery();
  }

  private void setNavigationMenu() {
    mB.navV.invalidate();
    setBoxesChecked();
    setCheckBoxListeners();
    mB.navV.getMenu().findItem(R.id.action_donate).setVisible(true);
  }

  private void setBoxesChecked() {
    if (mB != null) {
      setRootCheckBox(MySettings.INS.isRootEnabled(), true);
      setAdbCheckBox(MySettings.INS.isAdbEnabled(), true);
    }
  }

  private void setCheckBoxListeners() {
    if (mB == null) {
      return;
    }
    Menu menu = mB.navV.getMenu();
    for (int id : new int[] {R.id.action_root, R.id.action_adb}) {
      MenuItem menuItem = menu.findItem(id);
      Objects.requireNonNull(menuItem.getActionView())
          .setOnClickListener(v -> handleNavigationItemChecked(menuItem));
    }
  }

  private boolean handleNavigationItemSelected(MenuItem item) {
    View view = item.getActionView();
    if (view instanceof CheckBox checkBox) {
      checkBox.setChecked(!checkBox.isChecked());
    }
    return handleNavigationItemChecked(item);
  }

  private boolean handleNavigationItemChecked(MenuItem item) {
    mB.getRoot().closeDrawer(GravityCompat.START, true);

    if (item.getItemId() == R.id.action_settings) {
      mA.startActivity(new Intent(App.getCxt(), SettingsActivityM.class));
      return true;
    }

    if (item.getItemId() == R.id.action_advanced_settings) {
      mA.startActivity(new Intent(App.getCxt(), AdvSettingsActivityM.class));
      return true;
    }

    if (item.getItemId() == R.id.action_filter) {
      mA.startActivity(new Intent(App.getCxt(), FilterSettingsActivityM.class));
      return true;
    }

    if (item.getItemId() == R.id.action_root) {
      handleRootCheckBox(((CheckBox) Objects.requireNonNull(item.getActionView())).isChecked());
      return true;
    }

    if (item.getItemId() == R.id.action_adb) {
      handleAdbCheckBox(((CheckBox) Objects.requireNonNull(item.getActionView())).isChecked());
      return true;
    }

    if (item.getItemId() == R.id.action_backup_restore) {
      AlertDialogFragment.show(mA, null, TAG_BACKUP_RESTORE);
      return true;
    }

    if (item.getItemId() == R.id.action_help) {
      HelpActivity.start(mA, null);
      return true;
    }

    if (item.getItemId() == R.id.action_donate) {
      ApiUtils.openWebUrl(
          mA, getString(R.string.payment_methods_href, getString(R.string.purchase_pro_url)));
      return true;
    }

    if (item.getItemId() == R.id.action_about) {
      AboutActivity.start(mA);
      return true;
    }

    return false;
  }

  private void handleRootCheckBox(boolean enable) {
    if (!enable) {
      BgRunner.execute(
          () -> {
            MySettings.INS.setRootEnabled(false);
            NativeDaemon.INS_R.stopDaemon();
            DaemonStarter.INS.stopDaemon(false);
          });
      return;
    }

    setRootCheckBox(false, false);
    new LiveTasksQueueTyped<>(mA, () -> NativeDaemon.getRoot(false))
        .onUiWith(this::handleRootResult)
        .start();
  }

  private void setRootCheckBox(boolean checked, boolean enabled) {
    CheckBox rootCheckBox = (CheckBox) mB.navV.getMenu().findItem(R.id.action_root).getActionView();
    Objects.requireNonNull(rootCheckBox).setChecked(checked);
    rootCheckBox.setEnabled(enabled);
  }

  private void handleRootResult(boolean granted) {
    if (granted) {
      showSnackBar(getString(R.string.root_granted_toast), 5);
      DaemonStarter.INS.switchToRootOrAdbDaemon(true);
    } else {
      showSnackBar(getString(R.string.getting_root_failed), 10);
    }
  }

  private void handleAdbCheckBox(boolean enable) {
    if (!enable) {
      BgRunner.execute(
          () -> {
            MySettings.INS.setAdbEnabled(false);
            NativeDaemon.INS_A.stopDaemon();
            DaemonStarter.INS.stopDaemon(true);
          });
      return;
    }

    setAdbCheckBox(false, true);
    AlertDialogFragment.show(mA, null, TAG_ADB_CONNECTION);
  }

  public void setAdbCheckBox(boolean checked, boolean enabled) {
    CheckBox adbCheckBox = (CheckBox) mB.navV.getMenu().findItem(R.id.action_adb).getActionView();
    Objects.requireNonNull(adbCheckBox).setChecked(checked);
    adbCheckBox.setEnabled(enabled);
  }

  private final NotifyWaiter mWindowWaiter;

  private void openDrawerForPrivileges() {
    new LiveTasksQueue(mA, mWindowWaiter::waitForNotifyNoThrow)
        .onUi(
            () -> {
              if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
                collapseSearchView();
              }
              mCheckBoxFocus.doFocus();
            })
        .start();
  }

  public class Data {

    public final int progBgColor, progBgSeparatorColor;

    private Data() {
      progBgColor = UiUtils.getSharpBgColor(mA);
      progBgSeparatorColor = UiUtils.getDimBgColor(mA);
    }
  }
}
