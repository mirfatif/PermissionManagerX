package com.mirfatif.permissionmanagerx;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
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
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.appcompat.widget.AppCompatSpinner;
import androidx.appcompat.widget.SearchView;
import androidx.appcompat.widget.SearchView.OnQueryTextListener;
import androidx.core.content.FileProvider;
import androidx.core.view.GravityCompat;
import androidx.core.view.MenuCompat;
import androidx.drawerlayout.widget.DrawerLayout;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;
import com.google.android.material.navigation.NavigationView;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.privdaemon.PrivDaemon;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;

public class MainActivity extends AppCompatActivity {

  public static final String ACTION_SHOW_DRAWER = BuildConfig.APPLICATION_ID + ".SHOW_DRAWER";
  static final String ACTION_START_LOGGING = BuildConfig.APPLICATION_ID + ".START_LOGGING";
  static final String EXTRA_PKG_POSITION = BuildConfig.APPLICATION_ID + ".PKG_POSITION";
  static final String APP_OPS_PERM = "android.permission.GET_APP_OPS_STATS";
  static final String TAG_GRANT_ROOT_OR_ADB = "GRANT_ROOT_OR_ADB";

  MySettings mMySettings;
  PackageParser mPackageParser;
  private PrivDaemonHandler mPrivDaemonHandler;
  private SwipeRefreshLayout mRefreshLayout;
  private LinearLayoutManager mLayoutManager;
  private ProgressBar mProgressBar;
  MyFrameLayout mRoundProgressContainer;
  TextView mRoundProgressTextView;
  private LinearLayout mProgressBarContainer;
  SearchView mSearchView;
  private MyViewModel mMyViewModel;
  private Integer mProgressMax;
  private TextView mProgressNowView;
  private TextView mProgressMaxView;
  private PackageAdapter mPackageAdapter;
  private MainActivityFlavor mMainActivityFlavor;

  private DrawerLayout mDrawerLayout;
  private ActionBarDrawerToggle mDrawerToggle;
  private NavigationView mNavigationView;

  final FragmentManager mFM = getSupportFragmentManager();

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);

    if (intent.getAction() == null) return;

    // called from PackageActivity
    if (intent.getAction().equals(ACTION_SHOW_DRAWER)) {
      openDrawer();
    }

    // called from AboutActivity
    if (intent.getAction().equals(ACTION_START_LOGGING)) recreate();
  }

  private void openDrawer() {
    Utils.runInBg(
        () -> {
          while (getWindow() == null) SystemClock.sleep(100);
          Utils.runInFg(() -> mDrawerLayout.openDrawer(GravityCompat.START));
        });
  }

  @Override
  protected void onSaveInstanceState(@NonNull Bundle outState) {

    // We can't save state of AlertDialogFragment since AlertDialog is passed as a constructor
    // argument. Otherwise separate AlertDialogFragment class needs to be created for every dialog.
    AlertDialogFragment.removeAll(mFM);

    super.onSaveInstanceState(outState);
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);

    // Create ViewModel instance and associate with current Activity. ViewModel initialize and holds
    // instances of other classes which must be retained irrespective of lifecycle of Activities
    mMyViewModel = new ViewModelProvider(this).get(MyViewModel.class);

    // singleton instances has been created by now in ViewModel
    mMySettings = MySettings.getInstance();
    mPackageParser = PackageParser.getInstance();
    mPrivDaemonHandler = PrivDaemonHandler.getInstance();

    /**
     * Must be after initializing {@link mMySettings}. Activity is recreated on switching to Dark
     * Theme, so return here.
     */
    if (setNightTheme()) return;

    // to show drawer icon
    Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);

    // flavor specific methods
    mMainActivityFlavor = new MainActivityFlavor(this);

    mDrawerLayout = findViewById(R.id.activity_main);
    mDrawerToggle =
        new ActionBarDrawerToggle(this, mDrawerLayout, android.R.string.ok, R.string.close);
    mDrawerLayout.addDrawerListener(mDrawerToggle);
    mDrawerToggle.syncState();

    if (getIntent().getAction().equals(ACTION_SHOW_DRAWER)) {
      openDrawer();
    }

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

    Future<?> checkRootAndAdbFuture =
        Utils.runInBg(
            () -> {
              Utils.runInFg(() -> mRoundProgressTextView.setText(R.string.checking_root_access));
              Utils.checkRootVerbose();
              Utils.runInFg(() -> mRoundProgressTextView.setText(R.string.checking_adb_access));
              Utils.checkAdbVerbose();
            });

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

    // increment app launch count
    mMySettings.plusAppLaunchCount();

    mMainActivityFlavor.onCreated(getIntent());
  }

  private boolean setNightTheme() {
    if (!mMySettings.forceDarkMode()) {
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
      intent.putExtra(EXTRA_PKG_POSITION, mPackageParser.getPackagePosition(pkg));
      startActivity(intent);
    };
  }

  private PkgLongClickListener getPkgLongClickListener() {
    return pkg -> {
      Builder builder = new Builder(this);
      builder.setPositiveButton(
          R.string.exclude,
          (dialogInterface, i) ->
              Utils.runInBg(
                  () -> {
                    mMySettings.removePkgFromExcludedApps(pkg.getName());
                    mPackageParser.removePackage(pkg);
                  }));

      builder.setNegativeButton(android.R.string.cancel, null);

      builder.setNeutralButton(
          pkg.isEnabled() ? R.string.disable : R.string.enable,
          (dialog, which) -> setPackageEnabledState(pkg));

      String message;
      boolean enabled = true;
      if (!pkg.isChangeable() || pkg.getName().equals(getPackageName())) {
        message = getString(R.string.exclude_app_from_visible_list);
        enabled = false;
      } else if (pkg.isEnabled()) {
        message = getString(R.string.disable_app_or_exclude_from_visible_list);
      } else {
        message = getString(R.string.enable_app_or_exclude_from_visible_list);
      }

      View layout = getLayoutInflater().inflate(R.layout.activity_main_pkg_alert_dialog, null);
      ((TextView) layout.findViewById(R.id.package_name_view)).setText(pkg.getName());
      ((TextView) layout.findViewById(R.id.message_view)).setText(message);

      // Set message, create and show the AlertDialog
      AlertDialog dialog = builder.setTitle(pkg.getLabel()).setView(layout).create();
      boolean finalEnabled = enabled;
      dialog.setOnShowListener(
          d -> dialog.getButton(AlertDialog.BUTTON_NEUTRAL).setEnabled(finalEnabled));
      new AlertDialogFragment(dialog).show(mFM, "PKG_OPTIONS", false);
    };
  }

  private void setWarningLiveObserver() {
    // to avoid duplicate observers
    mMyViewModel.getHiddenAPIsNotWorking().removeObservers(this);

    // do not show again if user opted not to try hidden APIs already, or on app resume if
    // observer removed already
    if (!mMySettings.canUseHiddenAPIs()) {
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
                mMySettings.setUseHiddenAPIs(false);

                setNavigationMenu();

                AlertDialog dialog =
                    new Builder(this)
                        .setPositiveButton(android.R.string.ok, null)
                        .setTitle(R.string.privileges)
                        .setMessage(R.string.hidden_apis_warning)
                        .create();
                new AlertDialogFragment(dialog).show(mFM, "HIDDEN_APIS_WARNING", false);
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
                progressTextView.setText(mPackageParser.getProgressTextResId(progressMax));
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
                          mPackageParser.getPackagesListSize() + " " + getString(R.string.packages),
                          5000)
                      .show();
                }
                if (!sendCrashReport()) {
                  mMainActivityFlavor.askForRating();
                }
                mMainActivityFlavor.onPackagesUpdated();
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
    setUpSearchView();

    mMainActivityFlavor.onCreateOptionsMenu();

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

  void updatePackagesList(boolean doRepeatUpdates) {
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
      AlertDialog dialog =
          new Builder(this)
              .setPositiveButton(
                  android.R.string.ok, (d, which) -> mDrawerLayout.openDrawer(GravityCompat.START))
              .setTitle(R.string.privileges)
              .setMessage(R.string.grant_root_or_adb)
              .create();
      new AlertDialogFragment(dialog).show(mFM, TAG_GRANT_ROOT_OR_ADB, false);
      return;
    }

    Utils.runInBg(
        () -> {
          boolean enabled = pkg.isEnabled();
          String command = pkg.getName() + " " + Utils.getUserId();
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

  private boolean sendCrashReport() {
    if (!mMySettings.mAskToSendCrashReport) return false;
    mMySettings.mAskToSendCrashReport = false;

    File logDir = new File(getExternalFilesDir(null), App.crashLogDir);
    if (!logDir.exists()) return false;
    File[] logFiles = logDir.listFiles();
    if (logFiles == null || logFiles.length == 0) return false;

    long crashReportTs = mMySettings.getCrashReportTs();
    mMySettings.setCrashReportTs();

    File fileToDelete, fileToSend = null;
    for (File logFile : logFiles) {
      if (!logFile.isFile()) continue;
      if (!logFile.getName().startsWith(App.logFilePrefix)) continue;
      if (!logFile.getName().endsWith(App.logFileSuffix)) continue;
      if (logFile.lastModified() > crashReportTs
          && (fileToSend == null || logFile.lastModified() > fileToSend.lastModified())) {
        fileToDelete = fileToSend;
        fileToSend = logFile;
      } else {
        fileToDelete = logFile;
      }
      if (fileToDelete != null && !fileToDelete.delete()) {
        Log.e("sendCrashReport", "Failed to delete " + logFile);
      }
    }

    if (fileToSend == null) return false;

    String authority = "com.mirfatif.permissionmanagerx.fileprovider";
    Uri logFileUri = FileProvider.getUriForFile(this, authority, fileToSend);

    Builder builder =
        new Builder(this)
            .setTitle(R.string.crash_report)
            .setMessage(getString(R.string.ask_to_report_crash, fileToSend.getName()))
            .setPositiveButton(
                android.R.string.ok,
                (dialog, which) -> {
                  Intent intent = new Intent(Intent.ACTION_SEND);
                  intent
                      .setData(logFileUri)
                      .setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
                      .setType("*/*")
                      .putExtra(
                          Intent.EXTRA_EMAIL, new String[] {getString(R.string.email_address)})
                      .putExtra(
                          Intent.EXTRA_SUBJECT, getString(R.string.app_name) + " - Crash Report")
                      .putExtra(Intent.EXTRA_TEXT, "Find attachment.")
                      .putExtra(Intent.EXTRA_STREAM, logFileUri);

                  try {
                    startActivity(intent);
                  } catch (ActivityNotFoundException e) {
                    Toast.makeText(
                            App.getContext(), R.string.no_email_app_installed, Toast.LENGTH_LONG)
                        .show();
                  }
                })
            .setNegativeButton(android.R.string.cancel, null);
    new AlertDialogFragment(builder.create()).show(mFM, "CRASH_REPORT", true);
    return true;
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (mMainActivityFlavor != null) mMainActivityFlavor.onResumed();
  }

  @Override
  protected void onDestroy() {
    if (mMainActivityFlavor != null) mMainActivityFlavor.onDestroyed();
    super.onDestroy();
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
            if (mMySettings.DEBUG) Utils.debugLog("searchQueryTextSubmit", query);
            handleSearchQuery();
            return true;
          }

          @Override
          public boolean onQueryTextChange(String newText) {
            if (mMySettings.DEBUG) Utils.debugLog("searchQueryTextChange", newText);
            handleSearchQuery();
            return true;
          }
        });

    // Clear search query when no text is entered.
    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          if (mMySettings.DEBUG) Utils.debugLog("searchQueryFocussed", String.valueOf(hasFocus));
          showSearchActionSettings();
          mDrawerLayout.closeDrawer(GravityCompat.START, true);
          if (!hasFocus && TextUtils.isEmpty(mSearchView.getQuery())) {
            collapseSearchView();
          }
        });

    mSearchView.setQueryHint(getString(R.string.search_menu_item)); // Show a search hint
    mSearchView.setMaxWidth(Integer.MAX_VALUE); // Hide package name
  }

  private void showSearchActionSettings() {
    CheckBox deepSearchSettings = findViewById(R.id.deep_search);
    CheckBox caseSensitiveSearchSettings = findViewById(R.id.case_sensitive_search);

    deepSearchSettings.setOnCheckedChangeListener(null);
    caseSensitiveSearchSettings.setOnCheckedChangeListener(null);

    deepSearchSettings.setChecked(mMySettings.isDeepSearchEnabled());
    caseSensitiveSearchSettings.setChecked(mMySettings.isCaseSensitiveSearch());

    deepSearchSettings.setOnCheckedChangeListener(
        (buttonView, isChecked) -> {
          mMySettings.setDeepSearchEnabled(isChecked);
          handleSearchQuery();
          if (mMySettings.DEBUG) Utils.debugLog("deepSearch", String.valueOf(isChecked));
        });

    caseSensitiveSearchSettings.setOnCheckedChangeListener(
        (buttonView, isChecked) -> {
          mMySettings.setCaseSensitiveSearch(isChecked);
          handleSearchQuery();
          if (mMySettings.DEBUG) Utils.debugLog("caseSensitiveSearch", String.valueOf(isChecked));
        });

    findViewById(R.id.search_settings_container).setVisibility(View.VISIBLE);
  }

  private void collapseSearchView() {
    if (mMySettings.DEBUG) Utils.debugLog("searchView", "Collapsing");
    mSearchView.onActionViewCollapsed();
    mSearchView.setQuery(null, false);
    handleSearchQuery(); // mSearchView.setQuery(null, true) does not work
    findViewById(R.id.search_settings_container).setVisibility(View.GONE);
  }

  private void handleSearchQuery() {
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

    if (mMySettings.isDeepSearchEnabled()) {
      updatePackagesList(true);
    } else {
      mPackageParser.handleSearchQuery(true);
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PRIVILEGES //////////////////////////
  //////////////////////////////////////////////////////////////////

  private synchronized void startPrivDaemon(boolean isFirstRun) {
    if (!mMySettings.mPrivDaemonAlive) {
      if (mMySettings.DEBUG) Utils.debugLog("startPrivDaemon", "Daemon is dead");
      if (mMySettings.isRootGranted() || mMySettings.isAdbConnected()) {
        Utils.runInFg(() -> mRoundProgressTextView.setText(R.string.starting_daemon));

        Boolean res = mPrivDaemonHandler.startDaemon();
        String message = null;
        boolean showDialog = false;
        if (res == null) {
          message = getString(R.string.daemon_logging_failed);
        } else if (!res) {
          message = getString(R.string.daemon_failed);
          if (Utils.getUserId() != 0) {
            message += ". " + getString(R.string.run_main_app);
            showDialog = true;
          }
        }

        if (message != null) {
          if (!showDialog) {
            String finalMessage = message;
            Utils.runInFg(() -> Snackbar.make(mProgressBarContainer, finalMessage, 10000).show());
          } else {
            Builder builder =
                new Builder(this)
                    .setPositiveButton(android.R.string.ok, null)
                    .setTitle(R.string.privileges)
                    .setMessage(message);
            Utils.runInFg(
                () ->
                    new AlertDialogFragment(builder.create())
                        .show(mFM, TAG_GRANT_ROOT_OR_ADB, false));
          }
        }
      } else {
        Log.e("startPrivDaemon", "Root access: unavailable, ADB shell: unavailable");

        Builder builder =
            new Builder(this)
                .setPositiveButton(
                    android.R.string.ok,
                    (dialog, which) -> mDrawerLayout.openDrawer(GravityCompat.START))
                .setTitle(R.string.privileges)
                .setMessage(getString(R.string.grant_root_or_adb));
        Utils.runInFg(
            () ->
                new AlertDialogFragment(builder.create()).show(mFM, TAG_GRANT_ROOT_OR_ADB, false));
      }
    }

    // get AppOps permission if daemon is up
    checkAppOpsPerm();

    /**
     * On first run, set warning observer before possibly triggering {@link
     * AppOpsParser#hiddenAPIsNotWorking(String)}. On later runs set/remove observer as the {@link
     * MySettings#canUseHiddenAPIs()} settings are changed. Must be after granting {@link
     * APP_OPS_PERM} so that {@link MySettings#canUseHiddenAPIs()} returns true.
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
      String command =
          PrivDaemon.GRANT_PERMISSION
              + " "
              + getPackageName()
              + " "
              + APP_OPS_PERM
              + " "
              + Utils.getUserId();

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
    setBoxCheckedAndSetListener(menu, R.id.action_dark_theme, mMySettings.forceDarkMode());

    menu.findItem(R.id.action_donate).setVisible(MainActivityFlavor.SHOW_ACTION_DONATE);
    menu.findItem(R.id.action_settings).setVisible(MainActivityFlavor.SHOW_ACTION_SETTINGS);
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

    if (item.getItemId() == R.id.action_settings) {
      mMainActivityFlavor.openSettings();
      return true;
    }

    if (item.getItemId() == R.id.action_filter) {
      startActivity(new Intent(App.getContext(), FilterSettingsActivity.class));
      return true;
    }

    if (item.getItemId() == R.id.action_root) {
      CheckBox rootCheckBox = (CheckBox) item.getActionView();
      if (!rootCheckBox.isChecked()) {
        mMySettings.setRootGranted(false);
        return true;
      }

      rootCheckBox.setChecked(false);

      Utils.runInBg(
          () -> {
            if (Utils.checkRoot()) {
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
        mMySettings.setAdbConnected(false);
        return true;
      }

      adbCheckBox.setChecked(false);

      Utils.runInBg(
          () -> {
            if (Utils.checkAdb()) {
              Utils.runInFg(
                  () -> {
                    Snackbar.make(mProgressBarContainer, R.string.connected_to_adb, 5000).show();
                    adbCheckBox.setChecked(true);
                  });
              restartPrivDaemon();
            } else {
              String message = getString(R.string.adb_connect_fail_long);
              if (Utils.getUserId() != 0) {
                message += "\n- " + getString(R.string.run_main_app);
              }
              Builder builder =
                  new Builder(this)
                      .setPositiveButton(android.R.string.ok, null)
                      .setTitle(R.string.privileges)
                      .setMessage(message);
              Utils.runInFg(
                  () ->
                      new AlertDialogFragment(builder.create())
                          .show(mFM, "ADB_CONNECT_FAILED", false));
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
      mMySettings.setForceDarkMode(darkCheckBox.isChecked());
      setNightTheme();
      return true;
    }

    if (item.getItemId() == R.id.action_backup_restore) {
      new BackupRestore(this);
      return true;
    }

    if (item.getItemId() == R.id.action_help) {
      startActivity(new Intent(App.getContext(), HelpActivity.class));
      return true;
    }

    if (item.getItemId() == R.id.action_donate) {
      return showDonateDialog();
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

    boolean useHiddenAPIs = mMySettings.useHiddenAPIs();
    useHiddenAPIsView.setChecked(useHiddenAPIs);

    boolean useSocket = mMySettings.useSocket();
    useSocketView.setChecked(useSocket);

    List<String> spinnerItems = Arrays.asList(getResources().getStringArray(R.array.daemon_uids));
    int resId = R.string.daemon_uid_system;
    if (mMySettings.getDaemonUid() == 0) resId = R.string.daemon_uid_root;
    else if (mMySettings.getDaemonUid() == 2000) resId = R.string.daemon_uid_adb;
    int selectedItemPosition = spinnerItems.indexOf(getString(resId));
    daemonUidSpinner.setSelection(selectedItemPosition);

    AlertDialog dialog =
        new Builder(this)
            .setTitle(R.string.advanced_settings_menu_item)
            .setView(layout)
            .setPositiveButton(
                R.string.save,
                (d, which) -> {
                  boolean startDaemon = false;
                  if (useHiddenAPIs != useHiddenAPIsView.isChecked()) {
                    startDaemon = saveHiddenAPIsSettings(useHiddenAPIsView.isChecked());
                  }

                  boolean restartDaemon = false;
                  if (useSocket != useSocketView.isChecked()) {
                    mMySettings.setUseSocket(!useSocket);
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
            .create();
    new AlertDialogFragment(dialog).show(mFM, "ADVANCED_SETTINGS", false);
  }

  private boolean saveHiddenAPIsSettings(boolean useHiddenAPIs) {
    if (useHiddenAPIs) {
      mMySettings.setUseHiddenAPIs(true);

      // make sure read AppOps permission is granted
      return true;
    }

    AlertDialog dialog =
        new Builder(this)
            .setPositiveButton(
                R.string.yes,
                (d, which) -> {
                  mMySettings.setUseHiddenAPIs(false);

                  // start daemon if not running
                  Utils.runInBg(() -> startPrivDaemon(false));
                })
            .setNegativeButton(R.string.no, null)
            .setTitle(R.string.hidden_apis)
            .setMessage(R.string.hidden_apis_confirmation)
            .create();
    new AlertDialogFragment(dialog).show(mFM, "HIDDEN_APIS_CONFIRM", false);
    return false;
  }

  //////////////////////////////////////////////////////////////////
  ////////////////////////////// DONATE ////////////////////////////
  //////////////////////////////////////////////////////////////////

  private boolean showDonateDialog() {
    View layout = getLayoutInflater().inflate(R.layout.donate_alert_dialog, null);

    ((TextView) layout.findViewById(R.id.bitcoin_link))
        .setMovementMethod(
            BetterLinkMovementMethod.newInstance()
                .setOnLinkClickListener((textView, uri) -> handleBitcoinClick()));

    ((TextView) layout.findViewById(R.id.bank_account_link))
        .setMovementMethod(
            BetterLinkMovementMethod.newInstance()
                .setOnLinkClickListener(
                    (tView, url) ->
                        Utils.sendMail(this, getString(R.string.bank_account_request))));

    ((TextView) layout.findViewById(R.id.play_store_link))
        .setMovementMethod(
            BetterLinkMovementMethod.newInstance()
                .setOnLinkClickListener((tView, url) -> Utils.openWebUrl(this, url)));

    new AlertDialogFragment(new Builder(this).setView(layout).create()).show(mFM, "DONATION", true);
    return true;
  }

  private boolean handleBitcoinClick() {
    Intent intent = new Intent(Intent.ACTION_VIEW);
    intent.setData(Uri.parse("bitcoin:" + getString(R.string.bitcoin_address)));
    if (getPackageManager().queryIntentActivities(intent, PackageManager.MATCH_ALL).isEmpty()) {
      Toast.makeText(App.getContext(), R.string.no_bitcoin_app_installed, Toast.LENGTH_LONG).show();
    } else {
      startActivity(intent);
    }
    return true;
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
    Builder builder =
        new Builder(this)
            .setTitle(R.string.logging)
            .setMessage(R.string.logging_warning)
            .setPositiveButton(android.R.string.ok, null);
    Utils.runInFg(
        () -> new AlertDialogFragment(builder.create()).show(mFM, "LOGGING_WARNING", false));
  }
}
