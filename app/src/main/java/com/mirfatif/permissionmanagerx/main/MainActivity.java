package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.parser.PackageParser.PKG_PARSER;
import static com.mirfatif.permissionmanagerx.prefs.MySettings.PERM_GET_APP_OPS_STATS;
import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.ADB_DAEMON;
import static com.mirfatif.permissionmanagerx.privs.NativeDaemon.ROOT_DAEMON;
import static com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler.DAEMON_HANDLER;

import android.content.Intent;
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
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.widget.SearchView;
import androidx.appcompat.widget.SearchView.OnQueryTextListener;
import androidx.core.view.GravityCompat;
import androidx.core.view.MenuCompat;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.ActivityMainBinding;
import com.mirfatif.permissionmanagerx.databinding.ActivityMainPkgDialogBinding;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.FilterSettingsActivity;
import com.mirfatif.permissionmanagerx.prefs.settings.AppUpdate;
import com.mirfatif.permissionmanagerx.prefs.settings.SettingsActivity;
import com.mirfatif.permissionmanagerx.ui.AboutActivity;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.HelpActivity;
import com.mirfatif.permissionmanagerx.ui.PackageActivity;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter.PkgClickListener;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter.PkgLongClickListener;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class MainActivity extends BaseActivity {

  private static final String TAG = "MainActivity";

  public static final String ACTION_SHOW_DRAWER = "com.mirfatif.pmx.ACTION_SHOW_DRAWER";
  public static final String EXTRA_PKG_POSITION = "com.mirfatif.pmx.PKG_POSITION";
  public static final String ACTION_SEARCH_PACKAGES = "com.mirfatif.pmx.ACTION_SEARCH_PACKAGES";
  public static final String EXTRA_SEARCH_STRINGS = "com.mirfatif.pmx.SEARCH_STRINGS";

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

    mB = ActivityMainBinding.inflate(getLayoutInflater());
    setContentView(mB.getRoot());

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
    setDrawerLiveObserver();
    setNavigationMenu();

    mB.refreshLayout.setOnRefreshListener(
        () -> {
          if (SETTINGS.isSearching()) {
            handleSearchQuery();
          } else {
            PKG_PARSER.updatePackagesList();
          }
        });

    Future<?> checkRootAndAdbFuture =
        Utils.runInBg(
            () -> {
              Utils.runInFg(() -> mB.rndProgTextV.setText(R.string.checking_root_access));
              Utils.checkRootIfEnabled();
              Utils.runInFg(() -> mB.rndProgTextV.setText(R.string.checking_adb_access));
              Utils.checkAdbIfEnabled();
            });

    Future<?> privDaemonFuture =
        Utils.runInBg(
            () -> {
              // We need root or ADB to start daemon
              if (waitForFuture(checkRootAndAdbFuture)) {
                // Check if we can read AppOps, even if daemon is alive
                startPrivDaemon(true, true);
              }
            });

    mPackageAdapter = new PackageAdapter(getPkgClickListener(), getPkgLongClickListener());

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

    Utils.runInBg(
        () -> {
          // Do not run through PackageParser unless privileged daemon is up
          if (waitForFuture(privDaemonFuture)) {
            Utils.runInFg(this::setLiveDataObservers);
          }
        });

    // Clear search query on activity refresh
    if (mSearchView != null) {
      collapseSearchView();
    } else {
      SETTINGS.setQueryText(null);
    }

    // Increment app launch count
    if (Intent.ACTION_MAIN.equals(getIntent().getAction())) {
      SETTINGS.plusAppLaunchCount();
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
    synchronized (SEARCH_VIEW_WAITER) {
      mSearchView = (SearchView) searchMenuItem.getActionView();
      setUpSearchView();
      SEARCH_VIEW_WAITER.notifyAll();
    }

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
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "onOptionsItemSelected: " + item.getTitle());
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
      if (SETTINGS.isDebug()) {
        Util.debugLog(TAG, "onBackPressed: closing drawer");
      }
      mB.getRoot().closeDrawer(GravityCompat.START, true);
      return;
    }
    if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
      if (SETTINGS.isDebug()) {
        Util.debugLog(TAG, "onBackPressed: collapsing searchView");
      }
      collapseSearchView();
      return;
    }
    // https://issuetracker.google.com/issues/139738913
    if (Build.VERSION.SDK_INT == Build.VERSION_CODES.Q) {
      finishAfterTransition();
    } else {
      super.onBackPressed();
    }
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (mMainActivityFlavor != null) {
      mMainActivityFlavor.onResumed();
    }
  }

  private static final String CLASS = MainActivity.class.getName();
  private static final String TAG_ADVANCED_SETTINGS = CLASS + ".ADVANCED_SETTINGS";
  private static final String TAG_PRIVS_REQ_FOR_DAEMON = CLASS + ".PRIVS_REQ_FOR_DAEMON";
  private static final String TAG_GRANT_ROOT_OR_ADB = CLASS + ".GRANT_ROOT_OR_ADB";
  private static final String TAG_ADB_CONNECT_FAILED = CLASS + ".ADB_CONNECT_FAILED";
  private static final String TAG_BACKUP_RESTORE = CLASS + ".TAG_BACKUP_RESTORE";
  static final String TAG_DONATION = CLASS + ".TAG_DONATION";

  @Override
  public AlertDialog createDialog(String tag, AlertDialogFragment dialogFragment) {
    if (TAG_PRIVS_REQ_FOR_DAEMON.equals(tag)) {
      AlertDialog dialog =
          new Builder(this)
              .setPositiveButton(android.R.string.ok, (d, which) -> openDrawerForPrivileges())
              .setNeutralButton(R.string.do_not_remind, (d, which) -> SETTINGS.setPrivReminderOff())
              .setNegativeButton(
                  R.string.get_help,
                  (d, which) -> startActivity(new Intent(App.getContext(), HelpActivity.class)))
              .setTitle(R.string.privileges)
              .setMessage(getString(R.string.grant_root_or_adb))
              .create();
      Utils.removeButtonPadding(dialog);
      return dialog;
    }

    if (TAG_GRANT_ROOT_OR_ADB.equals(tag)) {
      return new Builder(this)
          .setPositiveButton(android.R.string.ok, (d, which) -> openDrawerForPrivileges())
          .setNegativeButton(android.R.string.cancel, null)
          .setTitle(R.string.privileges)
          .setMessage(R.string.grant_root_or_adb)
          .create();
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

    if (TAG_ADVANCED_SETTINGS.equals(tag)) {
      return new AdvancedSettings(this).createDialog();
    }

    if (TAG_DONATION.equals(tag)) {
      return new Donate(this).createDialog();
    }

    return super.createDialog(tag, dialogFragment);
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// GENERAL ////////////////////////////
  //////////////////////////////////////////////////////////////////

  public static void restart() {
    Intent intent = new Intent(App.getContext(), MainActivity.class);
    intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
    App.getContext().startActivity(intent);
  }

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

  private PkgClickListener getPkgClickListener() {
    return pkg -> {
      if (SETTINGS.isDebug()) {
        Util.debugLog(TAG, "PkgClickListener: Package received: " + pkg.getLabel());
      }
      Intent intent = new Intent(App.getContext(), PackageActivity.class);
      intent.putExtra(EXTRA_PKG_POSITION, PKG_PARSER.getPackagePosition(pkg));
      startActivity(intent);
    };
  }

  private PkgLongClickListener getPkgLongClickListener() {
    return pkg -> {
      boolean canBeExcluded = SETTINGS.canBeExcluded(pkg);
      boolean canBeDisabled = pkg.isChangeable() && !pkg.getName().equals(getPackageName());
      if (!canBeExcluded && !canBeDisabled) {
        return;
      }

      Builder builder = new Builder(this);
      builder.setPositiveButton(
          R.string.exclude,
          (dialogInterface, i) ->
              Utils.runInBg(
                  () -> {
                    SETTINGS.addPkgToExcludedApps(pkg.getName());
                    PKG_PARSER.removePackage(pkg);
                  }));

      builder.setNegativeButton(android.R.string.cancel, null);

      builder.setNeutralButton(
          pkg.isEnabled() ? R.string.disable : R.string.enable,
          (dialog, which) -> setPackageEnabledState(pkg));

      String message;
      if (canBeDisabled) {
        if (pkg.isEnabled()) {
          message = getString(R.string.disable_app);
        } else {
          message = getString(R.string.enable_app);
        }
        if (canBeExcluded) {
          message = getString(R.string.exclude_from_visible_list, message);
        }
      } else {
        message = getString(R.string.exclude_app_from_visible_list);
      }

      ActivityMainPkgDialogBinding b = ActivityMainPkgDialogBinding.inflate(getLayoutInflater());

      b.pkgNameV.setText(pkg.getName());
      b.msgV.setText(message);

      // Set message, create and show the AlertDialog
      AlertDialog dialog = builder.setTitle(pkg.getLabel()).setView(b.getRoot()).create();
      dialog.setOnShowListener(
          d -> {
            dialog.getButton(AlertDialog.BUTTON_NEUTRAL).setEnabled(canBeDisabled);
            dialog.getButton(AlertDialog.BUTTON_POSITIVE).setEnabled(canBeExcluded);
          });
      AlertDialogFragment.show(this, dialog, "PKG_OPTIONS");
    };
  }

  private void setLiveDataObservers() {
    PKG_PARSER.getProgressMax().observe(this, this::setMaxProgress);
    PKG_PARSER.getProgressNow().observe(this, this::setNowProgress);
    PKG_PARSER.getPackagesListLive().observe(this, this::packagesListReceived);
    PKG_PARSER.getChangedPackage().observe(this, this::packageChanged);
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
      progressTextView.setText(PKG_PARSER.getProgressTextResId(progressMax));
      return;
    }

    mB.movCont.progBar.setIndeterminate(false);
    mB.movCont.progBar.setProgress(0);
    mB.movCont.progNowV.setText("0");
    mB.movCont.progBar.setMax(progressMax);
    mB.movCont.progMaxV.setText(String.valueOf(progressMax));
    mB.rndProgCont.setVisibility(View.GONE);
    mB.movCont.progBarCont.setVisibility(View.VISIBLE);
  }

  // Keep track of received packages, mPackageAdapter.getItemCount() given wrong value.
  // In PackageParser, Packages LiveList must be updated before updating ProgressBars.
  private int mVisiblePkgCount;

  private void setNowProgress(Integer progressNow) {
    int progress = progressNow;
    if (progress < 0 && mB.movCont.progBar.getMax() > 0) {
      progress = mB.movCont.progBar.getMax();
    }
    if (progress >= 0) {
      mB.movCont.progBar.setProgress(progress, true);
      mB.movCont.progNowV.setText(String.valueOf(progress));
    }
    if (progressNow >= 0) {
      return;
    }

    mB.movCont.progBarCont.setVisibility(View.GONE);
    boolean showPkgCount = true;

    if (progressNow == PackageParser.PKG_PROG_ENDS) {
      mMainActivityFlavor.onPackagesUpdated();
      if (SETTINGS.isSearching()) {
        // Don't stop refreshing. We'll receive call later when search ends
        return;
      }
      if (!mB.refreshLayout.isRefreshing()) {
        // Show Toast only if refreshed manually or by shallow search.
        // Otherwise on every onResume() Toast is displayed
        showPkgCount = false;
      }
    } else if (progressNow == PackageParser.SEARCH_ENDS) {
      if (!SETTINGS.isSearching()) {
        // Do not show pkg count when returning from search
        showPkgCount = false;
      }
    }

    mB.refreshLayout.setRefreshing(false);
    if (showPkgCount) {
      showSnackBar(mVisiblePkgCount + " " + getString(R.string.apps), 5000);
    }
  }

  private void packagesListReceived(List<Package> packages) {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "pkgListLiveObserver: " + packages.size() + " packages received");
    }
    mVisiblePkgCount = packages.size();
    mPackageAdapter.submitList(new ArrayList<>(packages));
    setRepeatUpdates();
  }

  private void packageChanged(Package pkg) {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "pkgChangedLiveObserver: Package updated: " + pkg.getLabel());
    }
    int position = mPackageAdapter.getCurrentList().indexOf(pkg);
    if (position != -1) {
      mPackageAdapter.notifyItemChanged(position);
    }
  }

  /*
   Keep on receiving new items from PackageParser unless there are at least 5 invisible items at
   the bottom.
  */
  private void setRepeatUpdates() {
    boolean rep = mPackageAdapter.getItemCount() < mLayoutManager.findLastVisibleItemPosition() + 5;
    PKG_PARSER.setRepeatUpdates(rep);
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "setRepeatUpdates: " + rep);
    }
  }

  private void setPackageEnabledState(Package pkg) {
    if (!SETTINGS.isPrivDaemonAlive()) {
      Utils.logDaemonDead(TAG + ": setPackageEnabledState");
      AlertDialogFragment.show(this, null, TAG_GRANT_ROOT_OR_ADB);
      return;
    }

    boolean enabled = pkg.isEnabled();

    String warn = null;
    if (enabled && SETTINGS.getBoolPref(R.string.pref_main_warn_dang_change_enc_key)) {
      if (pkg.isFrameworkApp()) {
        warn = getString(R.string.disable_pkg_warning, getString(R.string.framework));
      } else if (pkg.isSystemApp()) {
        warn = getString(R.string.disable_pkg_warning, getString(R.string.system));
      }
    }

    if (warn == null) {
      Utils.runInBg(() -> setPackageEnabledState(pkg, enabled));
      return;
    }

    AlertDialog dialog =
        new Builder(this)
            .setPositiveButton(
                R.string.yes,
                (d, which) -> Utils.runInBg(() -> setPackageEnabledState(pkg, enabled)))
            .setNegativeButton(R.string.no, null)
            .setNeutralButton(
                R.string.do_not_remind,
                (d, which) -> {
                  SETTINGS.savePref(R.string.pref_main_warn_dang_change_enc_key, false);
                  Utils.runInBg(() -> setPackageEnabledState(pkg, enabled));
                })
            .setTitle(R.string.warning)
            .setMessage(Utils.breakParas(warn))
            .create();
    AlertDialogFragment.show(this, dialog, "PKG_DISABLE_WARNING");
  }

  private void setPackageEnabledState(Package pkg, boolean enabled) {
    String command = pkg.getName() + " " + Utils.getUserId(pkg.getUid());
    if (enabled) {
      command = Commands.DISABLE_PACKAGE + " " + command;
    } else {
      command = Commands.ENABLE_PACKAGE + " " + command;
    }

    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "setPkgEnabledState: sending command: " + command);
    }
    DAEMON_HANDLER.sendRequest(command);
    PKG_PARSER.updatePackage(pkg);
  }

  void showSnackBar(String text, int duration) {
    Utils.runInFg(
        () -> {
          Snackbar snackBar = Snackbar.make(mB.movCont.progBarCont, text, duration);
          snackBar.setTextColor(getColor(R.color.dynamic_text_color));
          snackBar.getView().setBackgroundColor(getColor(R.color.dynamicBackground));
          snackBar.show();
        });
  }

  private boolean waitForFuture(Future<?> future) {
    if (future == null) {
      return false;
    }
    try {
      future.get();
      return true;
    } catch (ExecutionException | InterruptedException e) {
      e.printStackTrace();
      return false;
    }
  }

  // If called from PackageActivity or WR
  private void handleIntentActions(Intent intent) {
    String action = intent.getAction();
    if (action != null) {
      if (action.equals(ACTION_SHOW_DRAWER)) {
        openDrawerForPrivileges();
      } else if (action.equals(ACTION_SEARCH_PACKAGES)) {
        Utils.runInBg(() -> doSearch(intent));
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
            if (SETTINGS.isDebug()) {
              Util.debugLog(TAG, "searchQueryTextSubmitted: " + query);
            }
            handleSearchQuery();
            return true;
          }

          @Override
          public boolean onQueryTextChange(String newText) {
            if (SETTINGS.isDebug()) {
              Util.debugLog(TAG, "searchQueryTextChanged: " + newText);
            }
            handleSearchQuery();
            return true;
          }
        });

    // Clear search query when no text is entered.
    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          if (SETTINGS.isDebug()) {
            Util.debugLog(TAG, "searchViewFocused: " + hasFocus);
          }
          showSearchActionSettings();
          mB.getRoot().closeDrawer(GravityCompat.START, true);
          if (!hasFocus && TextUtils.isEmpty(mSearchView.getQuery())) {
            collapseSearchView();
          }
        });

    mSearchView.setQueryHint(getString(R.string.search_menu_item)); // Show a search hint
    mSearchView.setMaxWidth(Integer.MAX_VALUE); // Hide package name
  }

  private void showSearchActionSettings() {
    mB.deepSearch.setOnCheckedChangeListener(null);
    mB.caseSensitiveSearch.setOnCheckedChangeListener(null);

    mB.deepSearch.setChecked(SETTINGS.isDeepSearchEnabled());
    mB.caseSensitiveSearch.setChecked(SETTINGS.isCaseSensitiveSearch());

    // For android:ellipsize="marquee" to work
    mB.deepSearch.setSelected(true);
    mB.caseSensitiveSearch.setSelected(true);

    mB.deepSearch.setOnCheckedChangeListener(
        (buttonView, isChecked) -> {
          SETTINGS.setDeepSearchEnabled(isChecked);
          handleSearchQuery();
          if (SETTINGS.isDebug()) {
            Util.debugLog(TAG, "setting deepSearch: " + isChecked);
          }
        });

    mB.caseSensitiveSearch.setOnCheckedChangeListener(
        (buttonView, isChecked) -> {
          SETTINGS.setCaseSensitiveSearch(isChecked);
          handleSearchQuery();
          if (SETTINGS.isDebug()) {
            Util.debugLog(TAG, "setting caseSensitiveSearch: " + isChecked);
          }
        });

    mB.searchSettingsContainer.setVisibility(View.VISIBLE);
  }

  private void collapseSearchView() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "collapsing searchView");
    }
    mSearchView.onActionViewCollapsed();
    mSearchView.setQuery(null, false);
    handleSearchQuery(); // mSearchView.setQuery(null, true) does not work
    mB.searchSettingsContainer.setVisibility(View.GONE);
  }

  private void handleSearchQuery() {
    CharSequence queryText = mSearchView.getQuery();
    boolean wasSearching = SETTINGS.isSearching();

    /// Saving queryText to MySettings#mQueryText causes memory leak.
    SETTINGS.setQueryText(queryText == null ? null : queryText.toString());

    if (!SETTINGS.isSearching() && !wasSearching) {
      if (SETTINGS.isDebug()) {
        Util.debugLog(TAG, "handleSearchQuery: already empty text set, returning");
      }
      return;
    }

    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "handleSearchQuery: text set to: " + queryText);
    }

    mB.refreshLayout.setRefreshing(!SETTINGS.isDeepSearchEnabled() || !SETTINGS.isSearching());
    PKG_PARSER.newUpdateRequest();
    PKG_PARSER.handleSearchQuery(null);
  }

  private static final Object SEARCH_VIEW_WAITER = new Object();

  // Do search if called from PackageActivity or WR
  private void doSearch(Intent intent) {
    String[] pkgArray = intent.getStringArrayExtra(EXTRA_SEARCH_STRINGS);
    if (pkgArray == null) {
      return;
    }

    synchronized (SEARCH_VIEW_WAITER) {
      while (mSearchView == null) {
        try {
          SEARCH_VIEW_WAITER.wait();
        } catch (InterruptedException ignored) {
        }
      }
    }

    StringBuilder queryText = new StringBuilder();
    for (String pkg : pkgArray) {
      queryText.append(pkg).append("|");
    }

    Utils.runInFg(
        () -> {
          SETTINGS.setDeepSearchEnabled(false);
          SETTINGS.setCaseSensitiveSearch(true);
          mSearchView.setIconified(false);
          mSearchView.setQuery(queryText.toString(), true);
          mSearchView.clearFocus();
        });
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PRIVILEGES //////////////////////////
  //////////////////////////////////////////////////////////////////

  private static final Object START_DAEMON_LOCK = new Object();

  private void startPrivDaemon(boolean isFirstRun, boolean preferRoot) {
    synchronized (START_DAEMON_LOCK) {
      if (!SETTINGS.isPrivDaemonAlive()) {
        if (SETTINGS.isDebug()) {
          Util.debugLog(TAG, "startPrivDaemon: daemon is dead");
        }
        if (SETTINGS.isRootGranted() || SETTINGS.isAdbConnected()) {
          Utils.runInFg(() -> mB.rndProgTextV.setText(R.string.starting_daemon));

          Boolean res = DAEMON_HANDLER.startDaemon(preferRoot);
          if (res == null) {
            showSnackBar(getString(R.string.daemon_logging_failed), 10000);
          } else if (!res && !SETTINGS.isPrivDaemonAlive()) {
            showSnackBar(getString(R.string.daemon_failed), 10000);
          }
        } else {
          Log.w(TAG, "startPrivDaemon: Root access: unavailable, ADB shell: unavailable");
          if (SETTINGS.shouldRemindMissingPrivileges()) {
            Utils.runInFg(() -> AlertDialogFragment.show(this, null, TAG_PRIVS_REQ_FOR_DAEMON));
          }
        }
      }

      // Get GET_APP_OPS_STATS permission if daemon is up
      checkAppOpsPerm();

      // If have gained privileges
      if (SETTINGS.isPrivDaemonAlive() || SETTINGS.isAppOpsGranted()) {
        // If observers are set, update packages list.
        if (!isFirstRun) {
          PKG_PARSER.updatePackagesList();
        }
      }

      mMainActivityFlavor.onPrivDaemonStarted();
    }
  }

  void restartPrivDaemon(boolean preferRoot) {
    Utils.runInBg(
        () -> {
          if (SETTINGS.isPrivDaemonAlive()) {
            DAEMON_HANDLER.sendRequest(Commands.SHUTDOWN);
            SystemClock.sleep(1000); // Let the previous processes cleanup
          }
          startPrivDaemon(false, preferRoot);
        });
  }

  private void checkAppOpsPerm() {
    if (!SETTINGS.isAppOpsGranted() && SETTINGS.isPrivDaemonAlive()) {
      String command =
          Commands.GRANT_PERMISSION
              + " "
              + getPackageName()
              + " "
              + PERM_GET_APP_OPS_STATS
              + " "
              + Utils.getUserId();

      if (SETTINGS.isDebug()) {
        Util.debugLog(TAG, "checkAppOpsPerm: sending command: " + command);
      }
      DAEMON_HANDLER.sendRequest(command);

      if (!SETTINGS.isAppOpsGranted()) {
        Log.e(TAG, "checkAppOpsPerm: granting " + PERM_GET_APP_OPS_STATS + " failed");
        String text = getString(R.string.granting_permission_failed, PERM_GET_APP_OPS_STATS);
        showSnackBar(text, 10000);
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////// NAVIGATION DRAWER ///////////////////////
  //////////////////////////////////////////////////////////////////

  private void setDrawerLiveObserver() {
    ROOT_DAEMON.getDrawerChanged().observe(this, res -> setBoxesChecked());
    ADB_DAEMON.getDrawerChanged().observe(this, res -> setBoxesChecked());
  }

  void setNavigationMenu() {
    if (SETTINGS.isDebug()) {
      Util.debugLog(TAG, "setNavigationMenu() called");
    }

    mB.navV.invalidate(); // If recreating
    setBoxesChecked();
    setCheckBoxListeners();
    mB.navV
        .getMenu()
        .findItem(R.id.action_donate)
        .setVisible(mMainActivityFlavor.getDonateVisibility());

    mMainActivityFlavor.setNavMenu(mB.navV.getMenu());
  }

  private void setBoxesChecked() {
    if (mB == null) {
      return;
    }
    Menu menu = mB.navV.getMenu();
    ((CheckBox) menu.findItem(R.id.action_root).getActionView())
        .setChecked(SETTINGS.isRootGranted());
    ((CheckBox) menu.findItem(R.id.action_adb).getActionView())
        .setChecked(SETTINGS.isAdbConnected());
    ((CheckBox) menu.findItem(R.id.action_dark_theme).getActionView())
        .setChecked(SETTINGS.forceDarkMode());
  }

  private void setCheckBoxListeners() {
    if (mB == null) {
      return;
    }
    Menu menu = mB.navV.getMenu();
    for (int id : new int[] {R.id.action_root, R.id.action_adb, R.id.action_dark_theme}) {
      MenuItem menuItem = menu.findItem(id);
      menuItem.getActionView().setOnClickListener(v -> handleNavigationItemChecked(menuItem));
    }
  }

  private boolean handleNavigationItemSelected(MenuItem item) {
    if (SETTINGS.isDebug()) {
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
    if (SETTINGS.isDebug()) {
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
      CheckBox rootCheckBox = (CheckBox) item.getActionView();
      if (!rootCheckBox.isChecked()) {
        SETTINGS.setRootGranted(false);
        ROOT_DAEMON.stopDaemon();
        return true;
      }

      rootCheckBox.setChecked(false);

      Utils.runInBg(
          () -> {
            if (Utils.checkRoot()) {
              showSnackBar(getString(R.string.root_granted), 5000);
              Utils.runInFg(() -> rootCheckBox.setChecked(true));
              restartPrivDaemon(true);
            } else {
              showSnackBar(getString(R.string.getting_root_fail_long), 10000);
            }
          });
      return true;
    }

    if (item.getItemId() == R.id.action_adb) {
      CheckBox adbCheckBox = (CheckBox) item.getActionView();
      if (!adbCheckBox.isChecked()) {
        SETTINGS.setAdbConnected(false);
        ADB_DAEMON.stopDaemon();
        return true;
      }

      adbCheckBox.setChecked(false);
      adbCheckBox.setEnabled(false);

      Utils.runInBg(
          () -> {
            if (Utils.checkAdb(true)) {
              showSnackBar(getString(R.string.connected_to_adb), 5000);
              Utils.runInFg(() -> adbCheckBox.setChecked(true));
              restartPrivDaemon(false);
            } else {
              Utils.runInFg(() -> AlertDialogFragment.show(this, null, TAG_ADB_CONNECT_FAILED));
            }
            Utils.runInFg(() -> adbCheckBox.setEnabled(true));
          });
      return true;
    }

    if (item.getItemId() == R.id.action_advanced_settings) {
      AlertDialogFragment.show(this, null, TAG_ADVANCED_SETTINGS);
      return true;
    }

    if (item.getItemId() == R.id.action_dark_theme) {
      CheckBox darkCheckBox = (CheckBox) item.getActionView();
      SETTINGS.setForceDarkMode(darkCheckBox.isChecked());
      Utils.setNightTheme(this);
      return true;
    }

    if (item.getItemId() == R.id.action_backup_restore) {
      AlertDialogFragment.show(this, null, TAG_BACKUP_RESTORE);
      return true;
    }

    if (item.getItemId() == R.id.action_help) {
      startActivity(new Intent(App.getContext(), HelpActivity.class));
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

  private void openDrawerForPrivileges() {
    Utils.runInBg(
        () -> {
          while (getWindow() == null) {
            SystemClock.sleep(100);
          }
          if (mB != null) {
            Utils.runInFg(() -> mB.getRoot().openDrawer(GravityCompat.START));
          }
          float f = new Random().nextBoolean() ? 360 : -360;
          Utils.runInBg(() -> rotateMenuItemCheckbox(R.id.action_root, f));
          Utils.runInBg(() -> rotateMenuItemCheckbox(R.id.action_adb, -1 * f));
        });
  }

  private void rotateMenuItemCheckbox(int resId, float angle) {
    SystemClock.sleep(1000);
    if (mB != null) {
      Utils.runInFg(
          () ->
              mB.navV
                  .getMenu()
                  .findItem(resId)
                  .getActionView()
                  .animate()
                  .rotationBy(angle)
                  .setDuration(1000)
                  .start());
    }
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////////// FOR SUBCLASSES ////////////////////////
  //////////////////////////////////////////////////////////////////

  ActivityMainBinding getRootView() {
    return mB;
  }

  ProgressFrameLayout getRoundProgressContainer() {
    return mB.rndProgCont;
  }

  TextView getRoundProgressTextView() {
    return mB.rndProgTextV;
  }

  @SuppressWarnings("UnusedDeclaration")
  SearchView getSearchView() {
    return mSearchView;
  }

  MainActivityFlavor getMainActivityFlavor() {
    return mMainActivityFlavor;
  }
}
