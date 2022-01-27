package com.mirfatif.permissionmanagerx.main.fwk;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.PERM_GET_APP_OPS_STATS;
import static com.mirfatif.permissionmanagerx.util.Utils.getQtyString;

import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.os.Bundle;
import android.os.SystemClock;
import android.text.TextUtils;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;
import androidx.annotation.ColorInt;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.appcompat.widget.SearchView;
import androidx.appcompat.widget.SearchView.OnQueryTextListener;
import androidx.core.app.NotificationManagerCompat;
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
import com.mirfatif.permissionmanagerx.databinding.ActivityMainBinding;
import com.mirfatif.permissionmanagerx.main.AdvSettingsDialogFrag;
import com.mirfatif.permissionmanagerx.main.BackupRestore;
import com.mirfatif.permissionmanagerx.main.Donate;
import com.mirfatif.permissionmanagerx.main.MainActivityFlavor;
import com.mirfatif.permissionmanagerx.main.PackageAdapter;
import com.mirfatif.permissionmanagerx.main.PackageAdapter.PkgAdapterCallback;
import com.mirfatif.permissionmanagerx.main.PkgLongPressDialogFrag;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.pkg.fwk.PackageActivity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.fwk.FilterSettingsActivity;
import com.mirfatif.permissionmanagerx.prefs.settings.AppUpdate;
import com.mirfatif.permissionmanagerx.prefs.settings.SearchSettingsFrag;
import com.mirfatif.permissionmanagerx.prefs.settings.SettingsActivity;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.HelpActivity;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;

public class MainActivity extends BaseActivity {

  private static final String TAG = "MainActivity";

  public static final String ACTION_SHOW_DRAWER = "com.mirfatif.pmx.action.SHOW_DRAWER";
  public static final String EXTRA_PKG_POSITION = "com.mirfatif.pmx.extra.PKG_POSITION";

  public static final String EXTRA_CRASH_NOTIF_ID =
      MainActivity.class.getName() + ".extra.CRASH_NOTIF_ID";

  private MainActivityFlavor mMainActivityFlavor;
  private BackupRestore mBackupRestore;

  private ActivityMainBinding mB;

  private LinearLayoutManager mLayoutManager;
  private SearchView mSearchView;
  private PackageAdapter mPackageAdapter;

  private ActionBarDrawerToggle mDrawerToggle;

  // On Android 9- onCreate is called twice after applying night theme, so keep synced.
  @Override
  protected synchronized void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    if (Utils.setNightTheme(this)) {
      return; // Activity is recreated on switching to Dark Theme, so return here
    }

    // ADB cannot access shared storage of secondary profiles on Pie+. On R+ shared storage of
    // secondary profiles is not mounted (and hence not visible) in root mount namespace.
    if (isSecondaryUser()) {
      return;
    }

    sendCrashReport();

    mB = ActivityMainBinding.inflate(getLayoutInflater());
    mB.movCont.setData(new Data());
    setContentView(mB.getRoot());

    mB.roundProgBar.setBackground(new RoundProgBg(this));

    mMainActivityFlavor = new MainActivityFlavor(this);
    mBackupRestore = new BackupRestore(this);

    // to show drawer icon
    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setDisplayHomeAsUpEnabled(true);
    }

    mDrawerToggle =
        new ActionBarDrawerToggle(this, mB.getRoot(), R.string.open_drawer, R.string.close_drawer);
    mB.getRoot().addDrawerListener(mDrawerToggle);
    mDrawerToggle.syncState();

    handleIntentActions(getIntent());

    // Drawer items
    mB.navV.setNavigationItemSelectedListener(
        item -> {
          if (handleNavigationItemSelected(item)) {
            return true;
          }
          return super.onOptionsItemSelected(item);
        });
    setNavigationMenu();

    MySettings.INSTANCE.getPrefsChanged().observe(this, this::onPrefChanged);

    mB.refreshLayout.setOnRefreshListener(
        () -> {
          if (MySettings.INSTANCE.isSearching()) {
            handleSearchQuery();
          } else {
            PackageParser.INSTANCE.updatePackagesList();
          }
        });

    String action = getIntent().getAction();

    // Check if we can read AppOps, even if daemon is alive
    Utils.runInBg(() -> startPrivDaemon(true, true, !ACTION_SHOW_DRAWER.equals(action)));

    mPackageAdapter = new PackageAdapter(new PkgAdapterCallbackImpl());

    // Set Adapter on RecyclerView
    mB.recyclerView.setAdapter(mPackageAdapter);

    // Create and set a vertically scrolling list
    mLayoutManager = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
    mB.recyclerView.setLayoutManager(mLayoutManager);

    // Create and add divider between rows
    mB.recyclerView.addItemDecoration(
        new DividerItemDecoration(this, LinearLayoutManager.VERTICAL));

    // Set whether to receive new items frequent updates from PackageParser
    mB.recyclerView.setOnScrollChangeListener(
        (v, scrollX, scrollY, oldScrollX, oldScrollY) -> setRepeatUpdates());

    // Clear search query on activity refresh
    if (mSearchView != null) {
      collapseSearchView();
    } else {
      MySettings.INSTANCE.setQueryText(null);
    }

    mB.searchSettingsContainer.setOnClickListener(v -> hideSearchSettings());

    // Increment app launch count
    if (Intent.ACTION_MAIN.equals(action)) {
      MySettings.INSTANCE.plusAppLaunchCount();
    }

    mMainActivityFlavor.onCreated();

    Utils.runInBg(() -> new AppUpdate().check(true));
  }

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
    handleIntentActions(intent);
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    if (Utils.getUserId() != 0) {
      /*
       Do not show menu if secondary user. onCreate is not completed,
       so tapping on menu items may crash.
      */
      return false;
    }

    getMenuInflater().inflate(R.menu.main_search, menu);
    MenuCompat.setGroupDividerEnabled(menu, true);

    MenuItem searchMenuItem = menu.findItem(R.id.action_search);
    mSearchView = (SearchView) searchMenuItem.getActionView();
    setUpSearchView();

    if (mMainActivityFlavor != null) {
      mMainActivityFlavor.onCreateOptionsMenu(menu);
    }
    return super.onCreateOptionsMenu(menu);
  }

  @Override
  public boolean onPrepareOptionsMenu(Menu menu) {
    boolean res = false;
    if (mMainActivityFlavor != null) {
      res = mMainActivityFlavor.onPrepareOptionsMenu(menu);
    }
    return res || super.onPrepareOptionsMenu(menu);
  }

  // Required for navigation drawer tap to work
  @Override
  public boolean onOptionsItemSelected(@NonNull MenuItem item) {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "onOptionsItemSelected: " + item.getTitle());
    }

    if (item.getItemId() == android.R.id.home) {
      if (!mSearchView.isIconified()) {
        if (mB.searchSettingsContainer.getVisibility() == View.VISIBLE) {
          hideSearchSettings();
        } else {
          mB.searchSettingsContainer.setVisibility(View.VISIBLE);
          getSupportFragmentManager()
              .beginTransaction()
              .replace(R.id.search_settings_frag, new SearchSettingsFrag())
              .commit();
        }
        return true;
      }
    }

    boolean res = false;
    if (mMainActivityFlavor != null) {
      res = mMainActivityFlavor.onOptionsItemSelected(item);
    }
    return res || mDrawerToggle.onOptionsItemSelected(item) || super.onOptionsItemSelected(item);
  }

  @Override
  public void onBackPressed() {
    if (mB != null && mB.getRoot().isDrawerOpen(GravityCompat.START)) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "onBackPressed: closing drawer");
      }
      mB.getRoot().closeDrawer(GravityCompat.START, true);
      return;
    }
    if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "onBackPressed: collapsing searchView");
      }
      collapseSearchView();
      return;
    }

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      mExited = true;
      if (mMainActivityFlavor != null) {
        mMainActivityFlavor.onBackPressed();
      }
    }

    // https://issuetracker.google.com/issues/139738913
    if (Build.VERSION.SDK_INT == Build.VERSION_CODES.Q) {
      finishAfterTransition();
    } else {
      super.onBackPressed();
    }
  }

  private boolean mExited = false;

  @Override
  protected void onResume() {
    super.onResume();
    if (mMainActivityFlavor != null) {
      mMainActivityFlavor.onResumed();
    }
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S && mExited) {
      PackageParser.INSTANCE.updatePackagesList();
    }
    mExited = false;
  }

  private final Object ON_DESTROY_LOCK = new Object();

  @Override
  protected void onDestroy() {
    synchronized (ON_DESTROY_LOCK) {
      mPackageAdapter = null;
    }
    super.onDestroy();
  }

  @Override
  public void onAttachedToWindow() {
    super.onAttachedToWindow();
    synchronized (WINDOW_WAITER) {
      WINDOW_WAITER.notifyAll();
    }
  }

  private static final String CLASS = MainActivity.class.getName();
  private static final String TAG_PRIVS_REQ_FOR_DAEMON = CLASS + ".PRIVS_REQ_FOR_DAEMON";
  private static final String TAG_GRANT_ROOT_OR_ADB = CLASS + ".GRANT_ROOT_OR_ADB";
  private static final String TAG_ADB_CONNECT_FAILED = CLASS + ".ADB_CONNECT_FAILED";
  private static final String TAG_BACKUP_RESTORE = CLASS + ".TAG_BACKUP_RESTORE";
  public static final String TAG_DONATION = CLASS + ".TAG_DONATION";

  @Override
  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    if (TAG_PRIVS_REQ_FOR_DAEMON.equals(tag) || TAG_GRANT_ROOT_OR_ADB.equals(tag)) {
      Builder builder =
          new Builder(this)
              .setPositiveButton(android.R.string.ok, (d, which) -> openDrawerForPrivileges())
              .setTitle(R.string.privileges)
              .setMessage(getString(R.string.grant_root_or_adb));
      if (TAG_GRANT_ROOT_OR_ADB.equals(tag)) {
        builder.setNegativeButton(android.R.string.cancel, null);
      } else {
        builder
            .setNeutralButton(
                R.string.do_not_remind, (d, which) -> MySettings.INSTANCE.setPrivReminderOff())
            .setNegativeButton(R.string.get_help, (d, which) -> HelpActivity.start(this));
      }
      return Utils.removeButtonPadding(builder.create());
    }

    if (TAG_ADB_CONNECT_FAILED.equals(tag)) {
      return new Builder(this)
          .setPositiveButton(android.R.string.ok, null)
          .setTitle(R.string.privileges)
          .setMessage(Utils.htmlToString(R.string.adb_connect_fail_long))
          .create();
    }

    if (TAG_BACKUP_RESTORE.equals(tag)) {
      return mBackupRestore.createDialog();
    }

    if (TAG_DONATION.equals(tag)) {
      return new Donate(this).createDialog();
    }

    return super.createDialog(tag, dialogFragment);
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// GENERAL ////////////////////////////
  //////////////////////////////////////////////////////////////////

  private boolean isSecondaryUser() {
    if (Utils.getUserId() == 0) {
      return false;
    }

    Builder builder =
        new Builder(this)
            .setPositiveButton(android.R.string.ok, null)
            .setTitle(R.string.primary_account)
            .setMessage(R.string.primary_profile_only);
    AlertDialogFragment.show(this, builder.create(), "PRIMARY_PROFILE")
        .setOnDismissListener(d -> finishAfterTransition());

    Utils.getDefPrefs().edit().putBoolean("PRIMARY_USER", false).apply(); // Trigger auto-backup
    return true;
  }

  private void sendCrashReport() {
    Intent intent = getIntent();
    int id = intent.getIntExtra(EXTRA_CRASH_NOTIF_ID, 0);
    if (id != 0) {
      NotificationManagerCompat.from(App.getContext()).cancel(id);
      intent.removeExtra(EXTRA_CRASH_NOTIF_ID);
      intent = new Intent(intent);
      intent.setComponent(null);
      startActivity(intent);
    }
  }

  private class PkgAdapterCallbackImpl implements PkgAdapterCallback {

    @Override
    public void onClick(Package pkg) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "PkgClickListener: Package received: " + pkg.getLabel());
      }
      Intent intent = new Intent(App.getContext(), PackageActivity.class);
      intent.putExtra(EXTRA_PKG_POSITION, PackageParser.INSTANCE.getPackagePosition(pkg));
      startActivity(intent);
    }

    @Override
    public void onLongClick(Package pkg) {
      new PkgLongPressDialogFrag(pkg).show(getSupportFragmentManager(), "PKG_OPTIONS");
    }

    @Override
    public void runInFg(Runnable task) {
      Utils.runInFg(MainActivity.this, task);
    }
  }

  private boolean mObserversSet = false;

  private synchronized void setLiveDataObservers(boolean updateList) {
    if (mObserversSet) {
      if (updateList) {
        // Observers are already set, just update the package list.
        PackageParser.INSTANCE.updatePackagesList();
      }
      return;
    }

    PackageParser.INSTANCE.getProgressMax().observe(this, this::setMaxProgress);
    PackageParser.INSTANCE.getProgressNow().observe(this, this::setNowProgress);
    PackageParser.INSTANCE.getPackagesListLive().observe(this, this::packagesListReceived);
    PackageParser.INSTANCE.getChangedPackage().observe(this, this::packageChanged);

    mObserversSet = true;
  }

  private void setMaxProgress(Integer progressMax) {
    if (progressMax < 0) {
      TextView progressTextView;
      if (mB.rndProgCont.getVisibility() == View.VISIBLE) {
        progressTextView = mB.rndProgTextV;
      } else {
        progressTextView = mB.movCont.progNowV;
        mB.movCont.progBar.setIndeterminate(true);
        mB.movCont.progMaxV.setText("");
        mB.movCont.progBarCont.setVisibility(View.VISIBLE);
      }
      progressTextView.setText(PackageParser.INSTANCE.getProgressTextResId(progressMax));
      return;
    }

    NumberFormat nf = NumberFormat.getIntegerInstance();
    mB.movCont.progBar.setIndeterminate(false);
    mB.movCont.progBar.setProgress(0);
    mB.movCont.progNowV.setText(nf.format(0));
    mB.movCont.progBar.setMax(progressMax);
    mB.movCont.progMaxV.setText(nf.format(progressMax));
    mB.rndProgCont.setVisibility(View.GONE);
    mB.movCont.progBarCont.setVisibility(View.VISIBLE);
  }

  private void setNowProgress(Integer progressNow) {
    if (progressNow == null) {
      return;
    }

    int progress = progressNow;
    if (progress < 0 && mB.movCont.progBar.getMax() > 0) {
      progress = mB.movCont.progBar.getMax();
    }
    if (progress >= 0) {
      mB.movCont.progBar.setProgress(progress, true);
      mB.movCont.progNowV.setText(NumberFormat.getIntegerInstance().format(progress));
    }
    if (progressNow >= 0) {
      return;
    }

    mB.movCont.progBarCont.setVisibility(View.GONE);
    boolean showPkgCount = true;

    if (progressNow == PackageParser.PKG_PROG_ENDS) {
      mMainActivityFlavor.onPackagesUpdated();
      if (MySettings.INSTANCE.isSearching()) {
        // Don't stop refreshing. We'll receive call later when search ends
        return;
      }
      if (!mB.refreshLayout.isRefreshing()) {
        // Show Toast only if refreshed manually or by shallow search.
        // Otherwise on every onResume() Toast is displayed
        showPkgCount = false;
      }
    } else if (progressNow == PackageParser.SEARCH_ENDS) {
      if (!MySettings.INSTANCE.isSearching()) {
        // Do not show pkg count when returning from search
        showPkgCount = false;
      }
    }

    /*
     mPackageAdapter.getItemCount() gives wrong value.
     In PackageParser, Packages LiveList must be updated before updating ProgressBars.
    */
    int size = PackageParser.INSTANCE.getPkgCount();

    mB.refreshLayout.setRefreshing(false);
    if (showPkgCount || size == 0) {
      showSnackBar(getQtyString(R.plurals.apps_count, size, size), 5000);
    }

    /*
     Invalidate progress value so that on next
     Activity#onCreate(), the last value is ignored.
    */
    PackageParser.INSTANCE.unsetProgress();
  }

  private void packagesListReceived(List<Package> packages) {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "pkgListLiveObserver: " + packages.size() + " packages received");
    }
    synchronized (ON_DESTROY_LOCK) {
      if (mPackageAdapter != null) {
        mPackageAdapter.submitList(new ArrayList<>(packages));
      }
    }
    setRepeatUpdates();
  }

  private void packageChanged(Package pkg) {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "pkgChangedLiveObserver: Package updated: " + pkg.getLabel());
    }
    synchronized (ON_DESTROY_LOCK) {
      if (mPackageAdapter != null) {
        int position = mPackageAdapter.getCurrentList().indexOf(pkg);
        if (position != -1) {
          mPackageAdapter.notifyItemChanged(position);
        }
      }
    }
  }

  /*
   Keep on receiving new items from PackageParser unless there are at least 5 invisible items at
   the bottom.
  */
  private void setRepeatUpdates() {
    synchronized (ON_DESTROY_LOCK) {
      if (mPackageAdapter != null) {
        boolean rep =
            mPackageAdapter.getItemCount() < mLayoutManager.findLastVisibleItemPosition() + 5;
        PackageParser.INSTANCE.setRepeatUpdates(rep);
        if (MySettings.INSTANCE.isDebug()) {
          Util.debugLog(TAG, "setRepeatUpdates: " + rep);
        }
      }
    }
  }

  private void onPrefChanged(Integer pref) {
    switch (pref) {
      case MySettings.PREF_DRAWER_CHANGED:
        setBoxesChecked();
        break;
      case MySettings.PREF_UI_CHANGED:
        recreate();
        break;
      default:
        break;
    }
  }

  public void showSnackBar(String text, int duration) {
    Utils.runInFg(
        this,
        () -> {
          Snackbar snackBar = Snackbar.make(mB.movCont.progBarCont, text, duration);
          snackBar.setTextColor(getColor(R.color.sharpText));
          snackBar.getView().setBackgroundColor(Utils.getSharpBgColor(this));
          snackBar.show();
        });
  }

  // If called from PackageActivity or WR
  private void handleIntentActions(Intent intent) {
    String action = intent.getAction();
    if (action != null) {
      if (action.equals(ACTION_SHOW_DRAWER)) {
        openDrawerForPrivileges();
      } else if (mMainActivityFlavor != null) {
        mMainActivityFlavor.handleIntentActions(intent);
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// SEARCH /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private void setUpSearchView() {
    // Start listeners on SearchView
    // https://stackoverflow.com/a/31490543/9165920
    mSearchView.setOnQueryTextListener(
        new OnQueryTextListener() {
          @Override
          public boolean onQueryTextSubmit(String query) {
            if (MySettings.INSTANCE.isDebug()) {
              Util.debugLog(TAG, "searchQueryTextSubmitted: " + query);
            }
            handleSearchQuery();
            return true;
          }

          @Override
          public boolean onQueryTextChange(String newText) {
            if (MySettings.INSTANCE.isDebug()) {
              Util.debugLog(TAG, "searchQueryTextChanged: " + newText);
            }
            handleSearchQuery();
            return true;
          }
        });

    // Clear search query when no text is entered.
    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          if (MySettings.INSTANCE.isDebug()) {
            Util.debugLog(TAG, "searchViewFocused: " + hasFocus);
          }

          ActionBar actionBar = getSupportActionBar();
          if (actionBar != null) {
            Drawable icon = AppCompatResources.getDrawable(this, R.drawable.search_settings);
            if (icon != null) {
              actionBar.setHomeAsUpIndicator(icon);
            }
          }

          mB.getRoot().closeDrawer(GravityCompat.START, true);
          if (!hasFocus && TextUtils.isEmpty(mSearchView.getQuery())) {
            collapseSearchView();
          }
        });

    mSearchView.setQueryHint(getString(R.string.search_menu_item)); // Show a search hint
    mSearchView.setMaxWidth(Integer.MAX_VALUE); // Hide package name
  }

  private void collapseSearchView() {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "collapsing searchView");
    }
    mSearchView.onActionViewCollapsed();
    mSearchView.setQuery(null, false);
    handleSearchQuery(); // mSearchView.setQuery(null, true) does not work

    if (mMainActivityFlavor != null) {
      mMainActivityFlavor.resetDrawerIcon();
    }
  }

  private void hideSearchSettings() {
    int delay = 0;
    FragmentManager fm = getSupportFragmentManager();
    Fragment frag = fm.findFragmentById(R.id.search_settings_frag);
    if (frag != null) {
      // To enable expansion animation for next time.
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
    boolean wasSearching = MySettings.INSTANCE.isSearching();

    /// Saving queryText to MySettings#mQueryText causes memory leak.
    MySettings.INSTANCE.setQueryText(queryText == null ? null : queryText.toString());

    if (!MySettings.INSTANCE.isSearching() && !wasSearching) {
      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "handleSearchQuery: already empty text set, returning");
      }
      return;
    }

    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "handleSearchQuery: text set to: " + queryText);
    }

    mB.refreshLayout.setRefreshing(
        !MySettings.INSTANCE.isDeepSearchEnabled() || !MySettings.INSTANCE.isSearching());
    PackageParser.INSTANCE.newUpdateRequest();
    PackageParser.INSTANCE.handleSearchQuery();
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PRIVILEGES //////////////////////////
  //////////////////////////////////////////////////////////////////

  private static final Object START_DAEMON_LOCK = new Object();

  private void startPrivDaemon(boolean isFirstRun, boolean preferRoot, boolean showDialog) {
    // We need root or ADB to start daemon.
    if (isFirstRun) {
      checkRootAdb();
    }

    synchronized (START_DAEMON_LOCK) {
      if (!MySettings.INSTANCE.isPrivDaemonAlive()) {
        if (MySettings.INSTANCE.isDebug()) {
          Util.debugLog(TAG, "startPrivDaemon: daemon is dead");
        }
        if (MySettings.INSTANCE.isRootGranted() || MySettings.INSTANCE.isAdbConnected()) {
          Utils.runInFg(this, () -> mB.rndProgTextV.setText(R.string.starting_daemon));

          Boolean res = PrivDaemonHandler.INSTANCE.startDaemon(preferRoot);
          if (res == null) {
            showSnackBar(getString(R.string.daemon_logging_failed), 10000);
          } else if (!res && !MySettings.INSTANCE.isPrivDaemonAlive()) {
            showSnackBar(getString(R.string.daemon_failed), 10000);
          }
        } else {
          Log.w(TAG, "startPrivDaemon: Root access: unavailable, ADB shell: unavailable");

          if (showDialog) {
            String tag = null;
            if (!isFirstRun) {
              tag = TAG_GRANT_ROOT_OR_ADB;
            } else if (MySettings.INSTANCE.shouldRemindMissingPrivileges()) {
              tag = TAG_PRIVS_REQ_FOR_DAEMON;
            }
            if (tag != null) {
              String finalTag = tag;
              Utils.runInFg(this, () -> AlertDialogFragment.show(this, null, finalTag));
            }
          }
        }
      }

      // Get GET_APP_OPS_STATS permission if daemon is up
      checkAppOpsPerm();

      /*
       Do not run through PackageParser unless privileged daemon
       is up or at least we have AppOps permission granted.
      */
      if (MySettings.INSTANCE.isPrivDaemonAlive() || MySettings.INSTANCE.isAppOpsGranted()) {
        // Set observers and/or update package list.
        Utils.runInFg(this, () -> setLiveDataObservers(!isFirstRun));
      } else if (isFirstRun) {
        Utils.runInFg(this, () -> mB.rndProgTextV.setText(R.string.missing_privileges));
      }

      mMainActivityFlavor.onPrivDaemonStarted();
    }
  }

  public void restartPrivDaemon(boolean preferRoot, boolean showDialog) {
    if (Utils.isMainThread()) {
      Utils.runInBg(() -> restartPrivDaemon(preferRoot, showDialog));
      return;
    }

    if (MySettings.INSTANCE.isPrivDaemonAlive()) {
      PrivDaemonHandler.INSTANCE.sendRequest(Commands.SHUTDOWN);
      SystemClock.sleep(1000); // Let the previous processes cleanup
    }
    startPrivDaemon(false, preferRoot, showDialog);
  }

  private void checkRootAdb() {
    Utils.runInFg(this, () -> mB.rndProgTextV.setText(R.string.checking_root_access));
    Utils.checkRootIfEnabled();
    Utils.runInFg(this, () -> mB.rndProgTextV.setText(R.string.checking_adb_access));
    Utils.checkAdbIfEnabled();
  }

  private void checkAppOpsPerm() {
    if (!MySettings.INSTANCE.isAppOpsGranted() && MySettings.INSTANCE.isPrivDaemonAlive()) {
      String command =
          Commands.GRANT_PERMISSION
              + " "
              + getPackageName()
              + " "
              + PERM_GET_APP_OPS_STATS
              + " "
              + Utils.getUserId();

      if (MySettings.INSTANCE.isDebug()) {
        Util.debugLog(TAG, "checkAppOpsPerm: sending command: " + command);
      }
      PrivDaemonHandler.INSTANCE.sendRequest(command);

      if (!MySettings.INSTANCE.isAppOpsGranted()) {
        Log.e(TAG, "checkAppOpsPerm: granting " + PERM_GET_APP_OPS_STATS + " failed");
        String text = getString(R.string.granting_permission_failed, PERM_GET_APP_OPS_STATS);
        showSnackBar(text, 10000);
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////// NAVIGATION DRAWER ///////////////////////
  //////////////////////////////////////////////////////////////////

  private void setNavigationMenu() {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "setNavigationMenu() called");
    }

    mB.navV.invalidate(); // If recreating
    setBoxesChecked();
    setCheckBoxListeners();
    boolean showDonate = !Utils.isPsVersion() && !Utils.isAmazVersion();
    mB.navV.getMenu().findItem(R.id.action_donate).setVisible(showDonate);

    mMainActivityFlavor.setNavMenu(mB.navV.getMenu());
  }

  public void setBoxesChecked() {
    if (mB == null) {
      return;
    }
    Menu menu = mB.navV.getMenu();
    ((CheckBox) menu.findItem(R.id.action_root).getActionView())
        .setChecked(MySettings.INSTANCE.isRootGranted());
    ((CheckBox) menu.findItem(R.id.action_adb).getActionView())
        .setChecked(MySettings.INSTANCE.isAdbConnected());
  }

  private void setCheckBoxListeners() {
    if (mB == null) {
      return;
    }
    Menu menu = mB.navV.getMenu();
    for (int id : new int[] {R.id.action_root, R.id.action_adb}) {
      MenuItem menuItem = menu.findItem(id);
      menuItem.getActionView().setOnClickListener(v -> handleNavigationItemChecked(menuItem));
    }
  }

  private boolean handleNavigationItemSelected(MenuItem item) {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "handleNavigationItemSelected: " + item.getTitle());
    }
    View view = item.getActionView();
    if (view instanceof CheckBox) {
      CheckBox checkBox = (CheckBox) view;
      checkBox.setChecked(!checkBox.isChecked());
    }
    return handleNavigationItemChecked(item);
  }

  private boolean handleNavigationItemChecked(MenuItem item) {
    if (MySettings.INSTANCE.isDebug()) {
      Util.debugLog(TAG, "handleNavigationItemChecked: " + item.getTitle());
    }
    mB.getRoot().closeDrawer(GravityCompat.START, true);

    if (item.getItemId() == R.id.action_settings) {
      startActivity(new Intent(App.getContext(), SettingsActivity.class));
      return true;
    }

    if (item.getItemId() == R.id.action_filter) {
      startActivity(new Intent(App.getContext(), FilterSettingsActivity.class));
      return true;
    }

    if (item.getItemId() == R.id.action_root) {
      Utils.runInBg(
          () -> {
            ANIM_LOCK.lock();
            handleRootCheckBox((CheckBox) item.getActionView());
            ANIM_LOCK.unlock();
          });
      return true;
    }

    if (item.getItemId() == R.id.action_adb) {
      Utils.runInBg(
          () -> {
            ANIM_LOCK.lock();
            handleAdbCheckBox((CheckBox) item.getActionView());
            ANIM_LOCK.unlock();
          });
      return true;
    }

    if (item.getItemId() == R.id.action_advanced_settings) {
      AdvSettingsDialogFrag.show(getSupportFragmentManager());
      return true;
    }

    if (item.getItemId() == R.id.action_backup_restore) {
      AlertDialogFragment.show(this, null, TAG_BACKUP_RESTORE);
      return true;
    }

    if (item.getItemId() == R.id.action_help) {
      HelpActivity.start(this);
      return true;
    }

    if (item.getItemId() == R.id.action_donate) {
      AlertDialogFragment.show(this, null, TAG_DONATION);
      return true;
    }

    if (item.getItemId() == R.id.action_about) {
      startActivity(new Intent(App.getContext(), AboutActivity.class));
      return true;
    }

    return mMainActivityFlavor.handleNavItemChecked(item);
  }

  private void handleRootCheckBox(CheckBox checkBox) {
    AtomicBoolean done = new AtomicBoolean(false);
    Utils.runInFg(
            this,
            () -> {
              if (!checkBox.isChecked()) {
                MySettings.INSTANCE.setRootGranted(false);
                NativeDaemon.INSTANCE_R.stopDaemon();
                restartPrivDaemon(true, false);
                if (mMainActivityFlavor != null) {
                  mMainActivityFlavor.onRootStopped();
                }
                done.set(true);
              }
            })
        .waitForMe();

    if (done.get()) {
      return;
    }

    if (Utils.checkRoot()) {
      showSnackBar(getString(R.string.root_granted), 5000);
      restartPrivDaemon(true, true);
    } else {
      showSnackBar(getString(R.string.getting_root_fail_long), 10000);
      Utils.runInFg(this, () -> checkBox.setChecked(false)).waitForMe();
    }
  }

  private void handleAdbCheckBox(CheckBox checkBox) {
    AtomicBoolean done = new AtomicBoolean(false);
    Utils.runInFg(
            this,
            () -> {
              if (!checkBox.isChecked()) {
                MySettings.INSTANCE.setAdbConnected(false);
                NativeDaemon.INSTANCE_A.stopDaemon();
                restartPrivDaemon(true, false);
                if (mMainActivityFlavor != null) {
                  mMainActivityFlavor.onAdbStopped();
                }
                done.set(true);
              } else {
                checkBox.setEnabled(false);
              }
            })
        .waitForMe();

    if (done.get()) {
      return;
    }

    boolean res = Utils.checkAdb(true);

    Utils.runInFg(
            this,
            () -> {
              if (res) {
                showSnackBar(getString(R.string.connected_to_adb), 5000);
                restartPrivDaemon(false, true);
              } else {
                AlertDialogFragment.show(this, null, TAG_ADB_CONNECT_FAILED);
                checkBox.setChecked(false);
              }
              checkBox.setEnabled(true);
            })
        .waitForMe();
  }

  private static final Object WINDOW_WAITER = new Object();

  // Drawer crashes if a CheckBox is checked while any of the CheckBoxes is animating.
  private final ReentrantLock ANIM_LOCK = new ReentrantLock();

  private void openDrawerForPrivileges() {
    if (Utils.isMainThread()) {
      Utils.runInBg(this::openDrawerForPrivileges);
      return;
    }

    synchronized (WINDOW_WAITER) {
      while (getWindow() == null) {
        try {
          WINDOW_WAITER.wait();
        } catch (InterruptedException ignored) {
        }
      }
    }

    Utils.runInFg(this, () -> mB.getRoot().openDrawer(GravityCompat.START));

    SystemClock.sleep(1000);

    if (ANIM_LOCK.isHeldByCurrentThread() || !ANIM_LOCK.tryLock()) {
      return;
    }

    float angle = new Random().nextBoolean() ? 360 : -360;
    Utils.runInFg(
        this,
        () -> {
          Menu menu = mB.navV.getMenu();
          CheckBox rootCheckBox = (CheckBox) menu.findItem(R.id.action_root).getActionView();
          CheckBox adbCheckBox = (CheckBox) menu.findItem(R.id.action_adb).getActionView();
          if (!rootCheckBox.isChecked() && !adbCheckBox.isChecked()) {
            rootCheckBox.animate().rotationBy(angle).setDuration(1000).start();
            adbCheckBox.animate().rotationBy(-1 * angle).setDuration(1000).start();
          }
        });

    SystemClock.sleep(2000);
    ANIM_LOCK.unlock();
  }

  public class Data {

    public final @ColorInt int progBgColor, progBgSeparatorColor;

    private Data() {
      progBgColor = Utils.getSharpBgColor(MainActivity.this);
      progBgSeparatorColor = Utils.getDimBgColor(MainActivity.this);
    }
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////////// FOR SUBCLASSES ////////////////////////
  //////////////////////////////////////////////////////////////////

  public ActivityMainBinding getRootView() {
    return mB;
  }

  public ProgressFrameLayout getRoundProgressContainer() {
    return mB.rndProgCont;
  }

  public TextView getRoundProgressTextView() {
    return mB.rndProgTextV;
  }

  @SuppressWarnings("UnusedDeclaration")
  public SearchView getSearchView() {
    return mSearchView;
  }

  public MainActivityFlavor getMainActivityFlavor() {
    return mMainActivityFlavor;
  }

  public ActionBarDrawerToggle getDrawerToggle() {
    return mDrawerToggle;
  }
}
