package com.mirfatif.permissionmanagerx;

import android.content.Intent;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.os.Process;
import android.os.SystemClock;
import android.text.TextUtils;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.appcompat.widget.AppCompatSpinner;
import androidx.appcompat.widget.SearchView;
import androidx.appcompat.widget.SearchView.OnQueryTextListener;
import androidx.core.view.GravityCompat;
import androidx.core.view.MenuCompat;
import androidx.drawerlayout.widget.DrawerLayout;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;
import com.google.android.material.navigation.NavigationView;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.privdaemon.PrivDaemon;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class MainActivity extends AppCompatActivity {

  static final String SHOW_DRAWER = "com.mirfatif.permissionmanagerx.SHOW_DRAWER";
  static final String START_LOGGING = "com.mirfatif.permissionmanagerx.START_LOGGING";
  static final String PACKAGE_POSITION = "com.mirfatif.permissionmanagerx.PACKAGE_POSITION";
  static final String APP_OPS_PERM = "android.permission.GET_APP_OPS_STATS";

  private MySettings mMySettings;
  private PackageParser mPackageParser;
  private PrivDaemonHandler mPrivDaemonHandler;
  private SwipeRefreshLayout mRefreshLayout;
  private LinearLayoutManager mLayoutManager;
  private ProgressBar mProgressBar;
  private FrameLayout mRoundProgressContainer;
  private TextView mRoundProgressTextView;
  private LinearLayout mProgressBarContainer;
  private SearchView mSearchView;
  private MyViewModel mMyViewModel;
  private Integer mProgressMax;
  private TextView mProgressNowView;
  private TextView mProgressMaxView;
  private PackageAdapter mPackageAdapter;

  private DrawerLayout mDrawerLayout;
  private ActionBarDrawerToggle mDrawerToggle;
  private NavigationView mNavigationView;

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);

    if (intent.getAction() == null) return;

    // called from PackageActivity
    if (intent.getAction().equals(SHOW_DRAWER)) {
      Utils.runInBg(
          () -> {
            while (getWindow() == null) SystemClock.sleep(100);
            Utils.runInFg(() -> mDrawerLayout.openDrawer(GravityCompat.START));
          });
    }

    // called from AboutActivity
    if (intent.getAction().equals(START_LOGGING)) recreate();
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);

    // Create ViewModel instance and associate with current Activity. ViewModel initiates and
    // holds
    // instances of other classes which must be retained irrespective of lifecycle of Activities
    mMyViewModel = new ViewModelProvider(this).get(MyViewModel.class);

    // singleton instances has been created by now in ViewModel
    mMySettings = MySettings.getInstance();
    mPackageParser = PackageParser.getInstance();
    mPrivDaemonHandler = PrivDaemonHandler.getInstance();

    /**
     * Must be after initiating {@link mMySettings}. Activity is recreated on switching to Dark
     * Theme, so return here.
     */
    if (setNightTheme()) return;

    // to show drawer icon
    Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);

    mDrawerLayout = findViewById(R.id.activity_main);
    mDrawerToggle =
        new ActionBarDrawerToggle(this, mDrawerLayout, android.R.string.ok, R.string.close);
    mDrawerLayout.addDrawerListener(mDrawerToggle);
    mDrawerToggle.syncState();

    // drawer items
    mNavigationView = findViewById(R.id.nav_view);
    mNavigationView.setNavigationItemSelectedListener(
        item -> {
          if (handleNavigationItemSelected(item)) return true;
          return super.onOptionsItemSelected(item);
        });

    mRefreshLayout = findViewById(R.id.refresh_layout);
    mRefreshLayout.setOnRefreshListener(() -> updatePackagesList(true));
    mProgressBar = findViewById(R.id.progress_bar);
    mProgressBarContainer = findViewById(R.id.progress_bar_container);
    mProgressNowView = findViewById(R.id.progress_now);
    mProgressMaxView = findViewById(R.id.progress_max);
    mRoundProgressContainer = findViewById(R.id.round_progress_container);
    mRoundProgressTextView = findViewById(R.id.round_progress_text);

    if (mMySettings.doLogging != null && mMySettings.doLogging) {
      mRoundProgressTextView.setText(R.string.start_logging);
      Utils.runInBg(this::startLogging);
    }

    Future<?> checkRootAndAdbFuture = Utils.runInBg(this::checkRootAndAdb);
    Utils.runInBg(
        () -> {
          try {
            checkRootAndAdbFuture.get();
          } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
            return;
          }
          Utils.runInFg(this::setNavigationMenu);
        });

    Future<?> privDaemonFuture =
        Utils.runInBg(
            () -> {
              // We need root or ADB to start daemon
              if (!mMySettings.mPrivDaemonAlive) {
                try {
                  checkRootAndAdbFuture.get();
                } catch (ExecutionException | InterruptedException e) {
                  e.printStackTrace();
                  return;
                }
              }
              // Check if we can read AppOps
              startPrivDaemon(true);
            });

    RecyclerView recyclerView = findViewById(R.id.recycler_view);
    mPackageAdapter = new PackageAdapter(getPkgClickListener(), getPkgLongClickListener());

    // Set Adapter on RecyclerView
    recyclerView.setAdapter(mPackageAdapter);

    // Create and set a vertically scrolling list
    mLayoutManager = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
    recyclerView.setLayoutManager(mLayoutManager);

    // Create and add divider between rows
    recyclerView.addItemDecoration(new DividerItemDecoration(this, LinearLayoutManager.VERTICAL));

    // Set whether to receive new items frequent updates from PackageParser
    recyclerView.setOnScrollChangeListener(
        (v, scrollX, scrollY, oldScrollX, oldScrollY) -> setRepeatUpdates());

    Utils.runInBg(
        () -> {
          /**
           * Do not run through {@link PackageParser} unless privileged daemon is up and {@link
           * PackageParser#buildAppOpsList()} is called from {@link MySettings#getAppOpsList()}
           */
          try {
            privDaemonFuture.get();
          } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
            return;
          }
          Utils.runInFg(this::setLiveDataObservers);
        });

    // clear search query on activity refresh
    if (mSearchView != null) collapseSearchView();
    else mMySettings.mQueryText = null;
  }

  private boolean setNightTheme() {
    if (!mMySettings.getBoolPref(R.string.main_settings_dark_theme_key)) {
      AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
      return false;
    }

    // Dark Mode applied on whole device
    if (Utils.isNightMode(this)) return false;

    // Dark Mode already applied in app
    int defMode = AppCompatDelegate.getDefaultNightMode();
    if (defMode == AppCompatDelegate.MODE_NIGHT_YES) return false;

    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
    return true;
  }

  private PkgClickListener getPkgClickListener() {
    return pkg -> {
      if (mMySettings.DEBUG)
        Utils.debugLog("MainActivity", "PkgClickListener: Package received: " + pkg.getLabel());
      Intent intent = new Intent().setClass(App.getContext(), PackageActivity.class);
      intent.putExtra(PACKAGE_POSITION, mPackageParser.getPackagePosition(pkg));
      startActivity(intent);
    };
  }

  private PkgLongClickListener getPkgLongClickListener() {
    return pkg -> {
      AlertDialog.Builder builder = new AlertDialog.Builder(this);
      builder.setPositiveButton(
          R.string.exclude,
          (dialogInterface, i) -> {
            Set<String> savedExcApps =
                mMySettings.getSetPref(R.string.filter_settings_excluded_apps_key);
            Set<String> excludedApps = new HashSet<>(Collections.singletonList(pkg.getName()));
            if (savedExcApps != null) excludedApps.addAll(savedExcApps);
            Utils.runInBg(
                () -> {
                  mMySettings.savePref(R.string.filter_settings_excluded_apps_key, excludedApps);
                  mPackageParser.removePackage(pkg);
                });
          });

      builder.setNegativeButton(android.R.string.cancel, null);

      builder.setNeutralButton(
          pkg.isEnabled() ? R.string.disable : R.string.enable,
          (dialog, which) -> setPackageEnabledState(pkg));

      String message = pkg.getName() + "\n\n";
      boolean enabled = true;
      if (!pkg.isChangeable() || pkg.getName().equals(getPackageName())) {
        message += getString(R.string.exclude_app_from_visible_list);
        enabled = false;
      } else if (pkg.isEnabled()) {
        message += getString(R.string.disable_app_or_exclude_from_visible_list);
      } else {
        message += getString(R.string.enable_app_or_exclude_from_visible_list);
      }

      // Set message, create and show the AlertDialog
      AlertDialog dialog = builder.setTitle(pkg.getLabel()).setMessage(message).create();
      dialog.show();

      dialog.getButton(AlertDialog.BUTTON_NEUTRAL).setEnabled(enabled);
    };
  }

  private void setWarningLiveObserver() {
    // to avoid duplicate observers
    mMyViewModel.getHiddenAPIsNotWorking().removeObservers(this);

    // do not show again if user opted not to try hidden APIs already, or on app resume if
    // observer removed already
    if (!mMySettings.useHiddenAPIs()) {
      if (mMySettings.DEBUG)
        Utils.debugLog("setWarningLiveObserver", "Not setting because hidden APIs are disabled");
      return;
    }

    mMyViewModel
        .getHiddenAPIsNotWorking()
        .observe(
            this,
            hiddenAPIsNotWorking -> {
              if (mMySettings.DEBUG)
                Utils.debugLog("hiddenAPIsNotWorking", String.valueOf(hiddenAPIsNotWorking));
              if (!hiddenAPIsNotWorking) return;
              // do not show message on next app resume
              mMyViewModel.getHiddenAPIsNotWorking().removeObservers(this);

              if (!mMySettings.mPrivDaemonAlive) {
                Utils.runInBg(() -> startPrivDaemon(false));
              } else {
                mMySettings.savePref(R.string.main_settings_use_hidden_apis_key, false);

                setNavigationMenu();

                new AlertDialog.Builder(this)
                    .setPositiveButton(android.R.string.ok, null)
                    .setTitle(R.string.privileges)
                    .setMessage(R.string.hidden_apis_warning)
                    .create()
                    .show();
              }
            });
  }

  private void setLiveDataObservers() {
    mMyViewModel
        .getProgressMax()
        .observe(
            this,
            progressMax -> {
              if (progressMax < 0) {
                TextView progressTextView;
                if (mRoundProgressContainer.getVisibility() == View.VISIBLE) {
                  progressTextView = mRoundProgressTextView;
                } else {
                  progressTextView = mProgressNowView;
                  mProgressBar.setIndeterminate(true);
                  mProgressMaxView.setText("");
                  mProgressBarContainer.setVisibility(View.VISIBLE);
                }
                switch (progressMax) {
                  case PackageParser.CREATE_PACKAGES_LIST:
                    progressTextView.setText(R.string.creating_packages_list);
                    break;
                  case PackageParser.REF_PERMS_LIST:
                    progressTextView.setText(R.string.reading_reference_perms);
                    break;
                  case PackageParser.OP_TO_SWITCH_LIST:
                    progressTextView.setText(R.string.mapping_op_to_switch);
                    break;
                  case PackageParser.OP_TO_DEF_MODE_LIST:
                    progressTextView.setText(R.string.listing_op_default_modes);
                    break;
                  case PackageParser.PERM_TO_OP_CODE_MAP:
                    progressTextView.setText(R.string.mapping_perms_to_ops);
                    break;
                }
                return;
              }

              mProgressBar.setIndeterminate(false);
              mProgressBar.setProgress(0);
              mProgressNowView.setText("0");
              mProgressMax = progressMax;
              mProgressBar.setMax(progressMax);
              mProgressMaxView.setText(String.valueOf(progressMax));
              mRoundProgressContainer.setVisibility(View.GONE);
              mProgressBarContainer.setVisibility(View.VISIBLE);
            });

    mMyViewModel
        .getProgressNow()
        .observe(
            this,
            progressNow -> {
              mProgressBar.setProgress(progressNow, true);
              mProgressNowView.setText(String.valueOf(progressNow));
              if (progressNow.equals(mProgressMax)) {
                mProgressBarContainer.setVisibility(View.GONE);
                if (mRefreshLayout.isRefreshing()) {
                  mRefreshLayout.setRefreshing(false);
                  Snackbar.make(
                          mProgressBarContainer,
                          mPackageParser.getPackagesListSize()
                              + " "
                              + getString(R.string.filter_settings_packages_title),
                          5000)
                      .show();
                }
              }
            });

    mMyViewModel
        .getPackagesListLive()
        .observe(
            this,
            packages -> {
              if (mMySettings.DEBUG)
                Utils.debugLog(
                    "getPackagesListLiveObserver", packages.size() + " packages received");
              // update visible list through quick search, if active
              mPackageAdapter.submitList(new ArrayList<>(packages));
              setRepeatUpdates();
            });

    mMyViewModel
        .getChangedPackage()
        .observe(
            this,
            pkg -> {
              if (mMySettings.DEBUG)
                Utils.debugLog(
                    "getChangedPackageLiveObserver", "Package updated: " + pkg.getLabel());
              int position = mPackageAdapter.getCurrentList().indexOf(pkg);
              if (position != -1) mPackageAdapter.notifyItemChanged(position);
            });
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.main_search, menu);
    MenuCompat.setGroupDividerEnabled(menu, true);

    MenuItem searchMenuItem = menu.findItem(R.id.action_search);
    mSearchView = searchMenuItem.getActionView().findViewById(R.id.action_search);
    mSearchView.setMaxWidth(Integer.MAX_VALUE);

    // Start listeners on Search view
    // https://stackoverflow.com/a/31490543/9165920
    mSearchView.setOnQueryTextListener(
        new OnQueryTextListener() {
          @Override
          public boolean onQueryTextSubmit(String query) {
            if (mMySettings.DEBUG) Utils.debugLog("searchQueryTextSubmit", query);
            handleSearchQuery(false);
            return true;
          }

          @Override
          public boolean onQueryTextChange(String newText) {
            if (mMySettings.DEBUG) Utils.debugLog("searchQueryTextChange", newText);
            handleSearchQuery(false);
            return true;
          }
        });

    // clear search query when no text is entered
    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          if (mMySettings.DEBUG) Utils.debugLog("searchQueryFocussed", String.valueOf(hasFocus));
          showSearchActionSettings();
          mDrawerLayout.closeDrawer(GravityCompat.START, true);
          if (!hasFocus && TextUtils.isEmpty(mSearchView.getQuery())) {
            collapseSearchView();
          }
        });

    // Show a search hint
    mSearchView.setQueryHint(getString(R.string.search_menu_item));

    return super.onCreateOptionsMenu(menu);
  }

  // required for navigation drawer tap to work
  @Override
  public boolean onOptionsItemSelected(@NonNull MenuItem item) {
    if (mMySettings.DEBUG)
      Utils.debugLog("MainActivity", "onOptionsItemSelected(): " + item.getTitle());
    return mDrawerToggle.onOptionsItemSelected(item) || super.onOptionsItemSelected(item);
  }

  @Override
  public void onBackPressed() {
    if (mDrawerLayout.isDrawerOpen(GravityCompat.START)) {
      if (mMySettings.DEBUG) Utils.debugLog("onBackPressed", "Closing drawer");
      mDrawerLayout.closeDrawer(GravityCompat.START, true);
      return;
    }
    if (!TextUtils.isEmpty(mSearchView.getQuery())) {
      if (mMySettings.DEBUG) Utils.debugLog("onBackPressed", "Collapsing searchView");
      collapseSearchView();
      return;
    }
    super.onBackPressed();
  }

  private void updatePackagesList(boolean doRepeatUpdates) {
    if (mMySettings.DEBUG)
      Utils.debugLog("MainActivity", "updatePackagesList: doRepeatUpdates: " + doRepeatUpdates);
    mPackageParser.updatePackagesList(doRepeatUpdates);
  }

  // Keep on receiving new items from PackageParser unless there are at least 5 invisible items at
  // the bottom.
  // While making search, always do repeat updates.
  private void setRepeatUpdates() {
    boolean doRepeatUpdates;
    if (mMySettings.isSearching()) {
      doRepeatUpdates = true;
    } else {
      doRepeatUpdates =
          mPackageAdapter.getItemCount() < mLayoutManager.findLastVisibleItemPosition() + 5;
    }
    mMySettings.mDoRepeatUpdates = doRepeatUpdates;
    if (mMySettings.DEBUG) Utils.debugLog("setRepeatUpdates", String.valueOf(doRepeatUpdates));
  }

  private void setPackageEnabledState(Package pkg) {
    if (!mMySettings.mPrivDaemonAlive) {
      new AlertDialog.Builder(this)
          .setPositiveButton(
              android.R.string.ok, (dialog, which) -> mDrawerLayout.openDrawer(GravityCompat.START))
          .setTitle(R.string.privileges)
          .setMessage(R.string.grant_root_or_adb)
          .create()
          .show();
      return;
    }

    Utils.runInBg(
        () -> {
          boolean enabled = pkg.isEnabled();
          String command = pkg.getName() + " " + Process.myUid() / 100000;
          if (enabled) {
            command = PrivDaemon.DISABLE_PACKAGE + " " + command;
          } else {
            command = PrivDaemon.ENABLE_PACKAGE + " " + command;
          }

          if (mMySettings.DEBUG)
            Utils.debugLog("setPackageEnabledState", "Sending command: " + command);
          Object res = mPrivDaemonHandler.sendRequest(command);
          mPackageParser.updatePackage(pkg);
          if (res != null) {
            Utils.runInFg(
                () -> Toast.makeText(App.getContext(), "Error occurred", Toast.LENGTH_LONG).show());
            Log.e("setPackageEnabledState", "Response is " + res);
          }
        });
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// SEARCH /////////////////////////////
  //////////////////////////////////////////////////////////////////

  private void showSearchActionSettings() {
    CheckBox deepSearchSettings = findViewById(R.id.deep_search);
    CheckBox caseSensitiveSearchSettings = findViewById(R.id.case_sensitive_search);
    deepSearchSettings.setChecked(mMySettings.isDeepSearchEnabled());
    caseSensitiveSearchSettings.setChecked(mMySettings.isCaseSensitiveSearch());

    deepSearchSettings.setOnClickListener(
        v -> {
          mMySettings.savePref(
              R.string.main_settings_deep_search_key, deepSearchSettings.isChecked());
          handleSearchQuery(true);
          if (mMySettings.DEBUG)
            Utils.debugLog("deepSearch", String.valueOf(deepSearchSettings.isChecked()));
        });

    caseSensitiveSearchSettings.setOnClickListener(
        v -> {
          mMySettings.savePref(
              R.string.main_settings_case_sensitive_search_key,
              caseSensitiveSearchSettings.isChecked());
          handleSearchQuery(false);
          if (mMySettings.DEBUG)
            Utils.debugLog(
                "caseSensitiveSearch", String.valueOf(caseSensitiveSearchSettings.isChecked()));
        });

    findViewById(R.id.search_settings_container).setVisibility(View.VISIBLE);
  }

  private void collapseSearchView() {
    if (mMySettings.DEBUG) Utils.debugLog("searchView", "Collapsing");
    mSearchView.onActionViewCollapsed();
    mSearchView.setQuery(null, false);
    handleSearchQuery(false); // mSearchView.setQuery(null, true) does not work
    findViewById(R.id.search_settings_container).setVisibility(View.GONE);
  }

  private void handleSearchQuery(boolean doDeepSearch) {
    CharSequence queryText = mSearchView.getQuery();
    boolean isSearching = mMySettings.isSearching();

    /** Save {@link queryText} to {@link MySettings#mQueryText} causes memory leak. */
    mMySettings.mQueryText = queryText.toString();

    if (TextUtils.isEmpty(queryText) && !isSearching) {
      if (mMySettings.DEBUG)
        Utils.debugLog("handleSearchQuery", "Already empty text set, returning");
      return;
    }

    if (mMySettings.DEBUG) Utils.debugLog("handleSearchQuery", "Text set to: " + queryText);

    if (doDeepSearch || mMySettings.isDeepSearchEnabled()) {
      updatePackagesList(true);
    } else {
      mPackageParser.handleSearchQuery(true);
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PRIVILEGES //////////////////////////
  //////////////////////////////////////////////////////////////////

  private boolean checkRootPrivileges() {
    boolean res = Utils.runCommand("su -c id  -u", "checkRootPrivileges", "0");
    mMySettings.savePref(R.string.main_settings_root_granted_key, res);
    return res;
  }

  private boolean checkAdbConnected() {
    boolean res = Adb.isConnected();
    mMySettings.savePref(R.string.main_settings_adb_connected_key, res);
    return res;
  }

  private void checkRootAndAdb() {
    Utils.runInFg(() -> mRoundProgressTextView.setText(R.string.checking_root_access));
    boolean rootChecked = mMySettings.isRootGranted();
    if (rootChecked && !checkRootPrivileges()) {
      Utils.runInFg(
          () ->
              Toast.makeText(App.getContext(), R.string.getting_root_fail, Toast.LENGTH_LONG)
                  .show());
      if (mMySettings.DEBUG) Utils.debugLog("checkRootAndAdb", "Getting root privileges failed");
    } else if (mMySettings.DEBUG) {
      Utils.debugLog("checkRootAndAdb", "Getting root privileges succeeded");
    }

    Utils.runInFg(() -> mRoundProgressTextView.setText(R.string.checking_adb_access));
    boolean adbChecked = mMySettings.getBoolPref(R.string.main_settings_adb_connected_key);
    if (adbChecked && !checkAdbConnected()) {
      Utils.runInFg(
          () ->
              Toast.makeText(App.getContext(), R.string.adb_connect_fail, Toast.LENGTH_LONG)
                  .show());
      if (mMySettings.DEBUG) Utils.debugLog("checkRootAndAdb", "Connecting to ADB failed");
    } else if (mMySettings.DEBUG) {
      Utils.debugLog("checkRootAndAdb", "Connecting to ADB succeeded");
    }
  }

  private synchronized void startPrivDaemon(boolean isFirstRun) {
    if (!mMySettings.mPrivDaemonAlive) {
      if (mMySettings.DEBUG) Utils.debugLog("startPrivDaemon", "Daemon is dead");
      if (mMySettings.isRootGranted() || mMySettings.isAdbConnected()) {
        Utils.runInFg(() -> mRoundProgressTextView.setText(R.string.starting_daemon));

        Boolean res = mPrivDaemonHandler.startDaemon();
        String message = null;
        if (res == null) message = getString(R.string.daemon_logging_failed);
        else if (!res) message = getString(R.string.daemon_failed);
        if (message != null) {
          String finalMessage = message;
          Utils.runInFg(() -> Snackbar.make(mProgressBarContainer, finalMessage, 10000).show());
        }
      } else {
        Log.e("startPrivDaemon", "Root access: unavailable, ADB shell: unavailable");

        AlertDialog.Builder builder =
            new AlertDialog.Builder(this)
                .setPositiveButton(
                    android.R.string.ok,
                    (dialog, which) -> mDrawerLayout.openDrawer(GravityCompat.START))
                .setTitle(R.string.privileges)
                .setMessage(getString(R.string.grant_root_or_adb));
        Utils.runInFg(() -> builder.create().show());
      }
    }

    // get AppOps permission if daemon is up
    checkAppOpsPerm();

    /**
     * On first run, set warning observer before possibly triggering {@link
     * AppOpsParser#hiddenAPIsNotWorking(String)}. On later runs set/remove observer as the {@link
     * MySettings#useHiddenAPIs()} settings are changed. Must be after granting {@link APP_OPS_PERM}
     * so that {@link MySettings#useHiddenAPIs()} returns true.
     */
    Utils.runInFg(this::setWarningLiveObserver);

    // if have gained privileges
    if (mMySettings.mPrivDaemonAlive || mMySettings.isAppOpsGranted()) {
      // If observers are set, update packages list.
      if (!isFirstRun) {
        Utils.runInFg(() -> updatePackagesList(false));
      }
    }
  }

  private void restartPrivDaemon() {
    Utils.runInBg(
        () -> {
          if (mMySettings.mPrivDaemonAlive) {
            mPrivDaemonHandler.sendRequest(PrivDaemon.SHUTDOWN);
          }
          startPrivDaemon(false);
        });
  }

  private void checkAppOpsPerm() {
    if (!mMySettings.isAppOpsGranted() && mMySettings.mPrivDaemonAlive) {
      int userId = Process.myUid() / 100000;
      String command =
          PrivDaemon.GRANT_PERMISSION + " " + getPackageName() + " " + APP_OPS_PERM + " " + userId;

      if (mMySettings.DEBUG) Utils.debugLog("startPrivDaemon", "Sending command: " + command);
      mPrivDaemonHandler.sendRequest(command);

      if (!mMySettings.isAppOpsGranted()) {
        Log.e("startPrivDaemon", "Granting " + APP_OPS_PERM + " failed");
        String message = getString(R.string.granting_permission_failed) + ": " + APP_OPS_PERM;
        Utils.runInFg(() -> Snackbar.make(mProgressBarContainer, message, 10000).show());
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////// NAVIGATION DRAWER ///////////////////////
  //////////////////////////////////////////////////////////////////

  private void setNavigationMenu() {
    if (mMySettings.DEBUG) Utils.debugLog("setNavigationMenu", "Called");
    Menu menu = mNavigationView.getMenu();

    // if recreating
    mNavigationView.invalidate();

    setBoxCheckedAndSetListener(menu, R.id.action_root, mMySettings.isRootGranted());
    setBoxCheckedAndSetListener(menu, R.id.action_adb, mMySettings.isAdbConnected());

    boolean darkTheme = mMySettings.getBoolPref(R.string.main_settings_dark_theme_key);
    setBoxCheckedAndSetListener(menu, R.id.action_dark_theme, darkTheme);
  }

  private void setBoxCheckedAndSetListener(Menu menu, int id, boolean checked) {
    MenuItem menuItem = menu.findItem(id);
    CheckBox checkBox = ((CheckBox) menuItem.getActionView());
    checkBox.setChecked(checked);
    checkBox.setOnClickListener(v -> handleNavigationItemChecked(menuItem));
  }

  private boolean handleNavigationItemSelected(MenuItem item) {
    if (mMySettings.DEBUG)
      Utils.debugLog("handleNavigationItemSelected", item.getTitle().toString());
    View view = item.getActionView();
    if (view instanceof CheckBox) {
      CheckBox checkBox = (CheckBox) view;
      checkBox.setChecked(!checkBox.isChecked());
    }
    return handleNavigationItemChecked(item);
  }

  private boolean handleNavigationItemChecked(MenuItem item) {
    if (mMySettings.DEBUG)
      Utils.debugLog("handleNavigationItemChecked", item.getTitle().toString());
    mDrawerLayout.closeDrawer(GravityCompat.START, true);

    if (item.getItemId() == R.id.action_filter) {
      startActivity(new Intent(App.getContext(), FilterSettingsActivity.class));
      return true;
    }

    if (item.getItemId() == R.id.action_root) {
      CheckBox rootCheckBox = (CheckBox) item.getActionView();
      if (!rootCheckBox.isChecked()) {
        mMySettings.savePref(R.string.main_settings_root_granted_key, false);
        return true;
      }

      rootCheckBox.setChecked(false);

      Utils.runInBg(
          () -> {
            if (checkRootPrivileges()) {
              Utils.runInFg(
                  () -> {
                    Snackbar.make(mProgressBarContainer, R.string.root_granted, 5000).show();
                    rootCheckBox.setChecked(true);
                  });
              restartPrivDaemon();
            } else {
              String message =
                  getString(R.string.getting_root_fail) + getString(R.string.are_you_rooted);
              Utils.runInFg(() -> Snackbar.make(mProgressBarContainer, message, 10000).show());
            }
          });
      return true;
    }

    if (item.getItemId() == R.id.action_adb) {
      CheckBox adbCheckBox = (CheckBox) item.getActionView();
      if (!adbCheckBox.isChecked()) {
        mMySettings.savePref(R.string.main_settings_adb_connected_key, false);
        return true;
      }

      adbCheckBox.setChecked(false);

      Utils.runInBg(
          () -> {
            if (checkAdbConnected()) {
              Utils.runInFg(
                  () -> {
                    Snackbar.make(mProgressBarContainer, R.string.connected_to_adb, 5000).show();
                    adbCheckBox.setChecked(true);
                  });
              restartPrivDaemon();
            } else {
              Utils.runInFg(
                  () ->
                      new AlertDialog.Builder(this)
                          .setPositiveButton(android.R.string.ok, null)
                          .setTitle(R.string.privileges)
                          .setMessage(R.string.adb_connect_fail_long)
                          .create()
                          .show());
            }
          });
      return true;
    }

    if (item.getItemId() == R.id.action_advanced_settings) {
      showAdvancedSettingsDialog();
      return true;
    }

    if (item.getItemId() == R.id.action_dark_theme) {
      CheckBox darkCheckBox = (CheckBox) item.getActionView();
      mMySettings.savePref(R.string.main_settings_dark_theme_key, darkCheckBox.isChecked());
      setNightTheme();
      return true;
    }

    if (item.getItemId() == R.id.action_backup_restore) {
      doBackupRestore();
      return true;
    }

    if (item.getItemId() == R.id.action_help) {
      startActivity(new Intent(App.getContext(), HelpActivity.class));
      return true;
    }

    if (item.getItemId() == R.id.action_donate) {
      View layout = getLayoutInflater().inflate(R.layout.qr_code_alert_dialog, null);
      new AlertDialog.Builder(this).setView(layout).create().show();
      return true;
    }

    if (item.getItemId() == R.id.action_about) {
      startActivity(new Intent(App.getContext(), AboutActivity.class));
      return true;
    }

    return false;
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////// ADVANCED SETTINGS ///////////////////////
  //////////////////////////////////////////////////////////////////

  private void showAdvancedSettingsDialog() {
    View layout = getLayoutInflater().inflate(R.layout.advanced_settings_alert_dialog, null);
    CheckBox useHiddenAPIsView = layout.findViewById(R.id.use_hidden_apis);
    CheckBox useSocketView = layout.findViewById(R.id.use_socket);
    AppCompatSpinner daemonUidSpinner = layout.findViewById(R.id.daemon_uid_list);

    ImageView arrow = layout.findViewById(R.id.daemon_uid_list_arrow);
    arrow.setOnClickListener(v -> daemonUidSpinner.performClick());

    if (Utils.isNightMode(this)) {
      ((TextView) layout.findViewById(R.id.use_hidden_apis_title)).setTextColor(Color.WHITE);
      ((TextView) layout.findViewById(R.id.use_socket_title)).setTextColor(Color.WHITE);
      ((TextView) layout.findViewById(R.id.daemon_uid_list_title)).setTextColor(Color.WHITE);
    }

    boolean useHiddenAPIs = mMySettings.getBoolPref(R.string.main_settings_use_hidden_apis_key);
    useHiddenAPIsView.setChecked(useHiddenAPIs);

    boolean useSocket = mMySettings.getBoolPref(R.string.main_settings_use_socket_key);
    useSocketView.setChecked(useSocket);

    List<String> spinnerItems = Arrays.asList(getResources().getStringArray(R.array.daemon_uids));
    int resId = R.string.daemon_uid_system;
    if (mMySettings.getDaemonUid() == 0) resId = R.string.daemon_uid_root;
    else if (mMySettings.getDaemonUid() == 2000) resId = R.string.daemon_uid_adb;
    int selectedItemPosition = spinnerItems.indexOf(getString(resId));
    daemonUidSpinner.setSelection(selectedItemPosition);

    new AlertDialog.Builder(this)
        .setTitle(R.string.advanced_settings_menu_item)
        .setView(layout)
        .setPositiveButton(
            R.string.save,
            (dialog, which) -> {
              boolean startDaemon = false;
              if (useHiddenAPIs != useHiddenAPIsView.isChecked()) {
                startDaemon = saveHiddenAPIsSettings(useHiddenAPIsView.isChecked());
              }

              boolean restartDaemon = false;
              if (useSocket != useSocketView.isChecked()) {
                mMySettings.savePref(R.string.main_settings_use_socket_key, !useSocket);
                restartDaemon = true;
              }

              int selectedItemNewPosition = daemonUidSpinner.getSelectedItemPosition();
              if (selectedItemPosition != selectedItemNewPosition) {
                String newSelection = spinnerItems.get(selectedItemNewPosition);
                int uid = 1000;
                if (newSelection.equals(getString(R.string.daemon_uid_root))) uid = 0;
                else if (newSelection.equals(getString(R.string.daemon_uid_adb))) uid = 2000;
                mMySettings.setDaemonUid(uid);
                restartDaemon = true;
              }

              if (restartDaemon) restartPrivDaemon();
              else if (startDaemon) Utils.runInBg(() -> startPrivDaemon(false));
            })
        .setNegativeButton(android.R.string.cancel, null)
        .create()
        .show();
  }

  private boolean saveHiddenAPIsSettings(boolean useHiddenAPIs) {
    if (useHiddenAPIs) {
      mMySettings.savePref(R.string.main_settings_use_hidden_apis_key, true);

      // make sure read AppOps permission is granted
      return true;
    }

    new AlertDialog.Builder(this)
        .setPositiveButton(
            R.string.yes,
            (dialog, which) -> {
              mMySettings.savePref(R.string.main_settings_use_hidden_apis_key, false);

              // start daemon if not running
              Utils.runInBg(() -> startPrivDaemon(false));
            })
        .setNegativeButton(R.string.no, null)
        .setTitle(R.string.hidden_apis)
        .setMessage(R.string.hidden_apis_confirmation)
        .create()
        .show();
    return false;
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////// BACKUP RESTORE /////////////////////////
  //////////////////////////////////////////////////////////////////

  private void doBackupRestore() {
    new AlertDialog.Builder(this)
        .setPositiveButton(R.string.backup, (dialog, which) -> doBackupRestore(true))
        .setNegativeButton(R.string.restore, (dialog, which) -> doBackupRestore(false))
        .setNeutralButton(android.R.string.cancel, null)
        .setTitle(getString(R.string.backup) + " / " + getString(R.string.restore))
        .setMessage(R.string.choose_backup_restore)
        .create()
        .show();
  }

  private void doBackupRestore(boolean isBackup) {
    Toast.makeText(App.getContext(), R.string.select_backup_file, Toast.LENGTH_LONG).show();
    ActivityResultCallback<Uri> callback =
        uri -> Utils.runInBg(() -> doBackupRestoreInBg(isBackup, uri));
    if (isBackup) {
      registerForActivityResult(new ActivityResultContracts.CreateDocument(), callback)
          .launch("PermissionManagerX_" + Utils.getCurrDateTime() + ".xml");
    } else {
      registerForActivityResult(new ActivityResultContracts.OpenDocument(), callback)
          .launch(new String[] {"text/xml"});
    }
  }

  private void doBackupRestoreInBg(boolean isBackup, Uri uri) {
    BackupRestore backupRestore = new BackupRestore();
    Utils.runInFg(
        () -> {
          LiveData<int[]> backRestResult = backupRestore.getBackupRestoreResult();
          backRestResult.observe(this, result -> handleBackupRestoreResult(result, backRestResult));
        });

    if (isBackup) {
      try (OutputStream outStream =
          getApplication().getContentResolver().openOutputStream(uri, "w")) {
        backupRestore.backup(outStream);
      } catch (IOException e) {
        e.printStackTrace();
      }
    } else {
      try (InputStream inputStream = getApplication().getContentResolver().openInputStream(uri)) {
        /**
         * So that not saved preferences are restored. Must be in background so that {@link
         * PrivDaemonHandler#sendRequest(String)} in {@link PackageParser#buildAppOpsList()} in ADB
         * daemon mode is not called on main thread
         */
        mMySettings.resetToDefaults();
        backupRestore.restore(inputStream);
      } catch (IOException ignored) {
      }
    }
  }

  private void handleBackupRestoreResult(int[] result, LiveData<int[]> backupRestoreResult) {
    boolean isBackup = result[0] == BackupRestore.TYPE_BACKUP;
    if (result.length == 1) {
      mRoundProgressTextView.setText(
          isBackup
              ? getString(R.string.backup_in_progress)
              : getString(R.string.restore_in_progress));
      mRoundProgressContainer.setVisibility(View.VISIBLE);
      return;
    }

    boolean failed = result[1] == BackupRestore.FAILED;
    mRoundProgressContainer.setVisibility(View.GONE);

    if (!isBackup && !failed) {
      Utils.runInBg(
          () -> {
            mMySettings.populateExcludedAppsList(false);
            mMySettings.populateExcludedPermsList();
            mMySettings.populateExtraAppOpsList(false);
            mPackageParser.buildPermRefList();
            updatePackagesList(false);
          });
    }

    String message;
    if (failed) message = getString(R.string.backup_restore_failed);
    else message = getString(R.string.backup_restore_process_entries, result[1], result[2]);
    if (result[3] > 0) message += getString(R.string.backup_restore_bad_entries, result[3]);

    new AlertDialog.Builder(this)
        .setPositiveButton(android.R.string.ok, null)
        .setTitle(isBackup ? R.string.backup : R.string.restore)
        .setMessage(message)
        .create()
        .show();

    // do not show success/failure dialogs on activity changes
    backupRestoreResult.removeObservers(this);
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// LOGGING ////////////////////////////
  //////////////////////////////////////////////////////////////////

  private void startLogging() {
    mMySettings.doLogging = null;
    if (mMySettings.DEBUG) Utils.debugLog("Logging", "Start logging");
    String command = "logcat --pid " + Process.myPid();

    if (Utils.doLoggingFails(new String[] {command})) {
      Utils.stopLogging();
      Utils.runInFg(
          () -> {
            Snackbar.make(mProgressBarContainer, R.string.logging_failed, 10000).show();
            setNavigationMenu();
          });
      return;
    }

    Utils.startLoggingTimer();
    Utils.runInFg(
        () ->
            new AlertDialog.Builder(this)
                .setTitle(R.string.logging)
                .setMessage(R.string.logging_warning)
                .setPositiveButton(android.R.string.ok, null)
                .create()
                .show());
  }
}
