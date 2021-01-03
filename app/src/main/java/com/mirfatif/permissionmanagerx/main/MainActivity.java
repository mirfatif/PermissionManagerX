package com.mirfatif.permissionmanagerx.main;

import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Process;
import android.os.SystemClock;
import android.text.TextUtils;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.appcompat.widget.SearchView;
import androidx.appcompat.widget.SearchView.OnQueryTextListener;
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
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.FilterSettingsActivity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.prefs.settings.AppUpdate;
import com.mirfatif.permissionmanagerx.prefs.settings.SettingsActivity;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import com.mirfatif.permissionmanagerx.ui.AboutActivity;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.ui.BaseActivity;
import com.mirfatif.permissionmanagerx.ui.HelpActivity;
import com.mirfatif.permissionmanagerx.ui.MyViewModel;
import com.mirfatif.permissionmanagerx.ui.PackageActivity;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter.PkgClickListener;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter.PkgLongClickListener;
import com.mirfatif.privtasks.Commands;
import com.mirfatif.privtasks.Util;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class MainActivity extends BaseActivity {

  private static final String TAG = "MainActivity";

  public static final String ACTION_SHOW_DRAWER = BuildConfig.APPLICATION_ID + ".SHOW_DRAWER";
  public static final String EXTRA_PKG_POSITION = BuildConfig.APPLICATION_ID + ".PKG_POSITION";
  public static final String APP_OPS_PERM = "android.permission.GET_APP_OPS_STATS";
  public static final String TAG_GRANT_ROOT_OR_ADB = "GRANT_ROOT_OR_ADB";

  private final MySettings mMySettings = MySettings.getInstance();
  private final PackageParser mPackageParser = PackageParser.getInstance();
  private final PrivDaemonHandler mPrivDaemonHandler = PrivDaemonHandler.getInstance();

  private MyViewModel mMyViewModel;
  private MainActivityFlavor mMainActivityFlavor;
  private final FragmentManager mFM = getSupportFragmentManager();

  private SwipeRefreshLayout mRefreshLayout;
  private LinearLayoutManager mLayoutManager;
  private ProgressBar mProgressBar;
  private MyFrameLayout mRoundProgressContainer;
  private TextView mRoundProgressTextView;
  private LinearLayout mProgressBarContainer;
  private SearchView mSearchView;
  private Integer mProgressMax;
  private TextView mProgressNowView;
  private TextView mProgressMaxView;
  private PackageAdapter mPackageAdapter;

  private DrawerLayout mDrawerLayout;
  private ActionBarDrawerToggle mDrawerToggle;
  private NavigationView mNavigationView;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    if (setNightTheme()) {
      return; // Activity is recreated on switching to Dark Theme, so return here
    }

    setContentView(R.layout.activity_main);

    // Create ViewModel instance and associate with current Activity. ViewModel holds
    // instances of other classes which must be retained irrespective of lifecycle of Activities
    mMyViewModel = new ViewModelProvider(this).get(MyViewModel.class);

    // to show drawer icon
    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setDisplayHomeAsUpEnabled(true);
    }

    // flavor specific methods
    mMainActivityFlavor = new MainActivityFlavor(this);

    mDrawerLayout = findViewById(R.id.activity_main);
    mDrawerToggle =
        new ActionBarDrawerToggle(this, mDrawerLayout, android.R.string.ok, R.string.close);
    mDrawerLayout.addDrawerListener(mDrawerToggle);
    mDrawerToggle.syncState();

    if (getIntent().getAction() != null && getIntent().getAction().equals(ACTION_SHOW_DRAWER)) {
      openDrawerForPrivileges();
    }

    // drawer items
    mNavigationView = findViewById(R.id.nav_view);
    mNavigationView.setNavigationItemSelectedListener(
        item -> {
          if (handleNavigationItemSelected(item)) {
            return true;
          }
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

    if (mMySettings.shouldStartLogging()) {
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
              if (!mMySettings.isPrivDaemonAlive()) {
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
    if (mSearchView != null) {
      collapseSearchView();
    } else {
      mMySettings.setQueryText(null);
    }

    // increment app launch count
    mMySettings.plusAppLaunchCount();

    mMainActivityFlavor.onCreated(getIntent());

    Utils.runInBg(() -> new AppUpdate().check(true));
  }

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
    // Called from PackageActivity
    if (intent.getAction() != null && intent.getAction().equals(ACTION_SHOW_DRAWER)) {
      openDrawerForPrivileges();
    }
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
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "onOptionsItemSelected(): " + item.getTitle());
    }
    return mDrawerToggle.onOptionsItemSelected(item) || super.onOptionsItemSelected(item);
  }

  @Override
  public void onBackPressed() {
    if (mDrawerLayout != null && mDrawerLayout.isDrawerOpen(GravityCompat.START)) {
      if (mMySettings.isDebug()) {
        Util.debugLog("onBackPressed", "Closing drawer");
      }
      mDrawerLayout.closeDrawer(GravityCompat.START, true);
      return;
    }
    if (mSearchView != null && !TextUtils.isEmpty(mSearchView.getQuery())) {
      if (mMySettings.isDebug()) {
        Util.debugLog("onBackPressed", "Collapsing searchView");
      }
      collapseSearchView();
      return;
    }
    super.onBackPressed();
  }

  @Override
  protected void onSaveInstanceState(@NonNull Bundle outState) {

    // We can't save state of AlertDialogFragment since AlertDialog is passed as a constructor
    // argument. Otherwise separate AlertDialogFragment class needs to be created for every dialog.
    AlertDialogFragment.removeAll(mFM);

    super.onSaveInstanceState(outState);
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (mMainActivityFlavor != null) {
      mMainActivityFlavor.onResumed();
    }
  }

  @Override
  protected void onDestroy() {
    if (mMainActivityFlavor != null) {
      mMainActivityFlavor.onDestroyed();
    }
    super.onDestroy();
  }

  //////////////////////////////////////////////////////////////////
  ///////////////////////////// GENERAL ////////////////////////////
  //////////////////////////////////////////////////////////////////

  private boolean setNightTheme() {
    if (!mMySettings.forceDarkMode()) {
      AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
      return false;
    }

    // Dark Mode applied on whole device
    if (Utils.isNightMode(this)) {
      return false;
    }

    // Dark Mode already applied in app
    int defMode = AppCompatDelegate.getDefaultNightMode();
    if (defMode == AppCompatDelegate.MODE_NIGHT_YES) {
      return false;
    }

    AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
    return true;
  }

  private PkgClickListener getPkgClickListener() {
    return pkg -> {
      if (mMySettings.isDebug()) {
        Util.debugLog(TAG, "PkgClickListener: Package received: " + pkg.getLabel());
      }
      Intent intent = new Intent(App.getContext(), PackageActivity.class);
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
                    mMySettings.addPkgToExcludedApps(pkg.getName());
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
      if (mMySettings.isDebug()) {
        Util.debugLog("setWarningLiveObserver", "Not setting because hidden APIs are disabled");
      }
      return;
    }

    mMyViewModel
        .getHiddenAPIsNotWorking()
        .observe(
            this,
            hiddenAPIsNotWorking -> {
              if (mMySettings.isDebug()) {
                Util.debugLog("hiddenAPIsNotWorking", String.valueOf(hiddenAPIsNotWorking));
              }
              if (!hiddenAPIsNotWorking) {
                return;
              }
              // do not show message on next app resume
              mMyViewModel.getHiddenAPIsNotWorking().removeObservers(this);

              if (!mMySettings.isPrivDaemonAlive()) {
                Utils.runInBg(() -> startPrivDaemon(false));
              }
              mMySettings.setUseHiddenAPIs(false);
              Toast.makeText(App.getContext(), R.string.hidden_apis_warning, Toast.LENGTH_LONG)
                  .show();
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
                  showSnackBar(
                      mPackageParser.getPackagesListSize() + " " + getString(R.string.packages),
                      5000);
                }
                mMainActivityFlavor.onPackagesUpdated();
              }
            });

    mMyViewModel
        .getPackagesListLive()
        .observe(
            this,
            packages -> {
              if (mMySettings.isDebug()) {
                Util.debugLog(
                    "getPackagesListLiveObserver", packages.size() + " packages received");
              }
              // update visible list through quick search, if active
              mPackageAdapter.submitList(new ArrayList<>(packages));
              setRepeatUpdates();
            });

    mMyViewModel
        .getChangedPackage()
        .observe(
            this,
            pkg -> {
              if (mMySettings.isDebug()) {
                Util.debugLog(
                    "getChangedPackageLiveObserver", "Package updated: " + pkg.getLabel());
              }
              int position = mPackageAdapter.getCurrentList().indexOf(pkg);
              if (position != -1) {
                mPackageAdapter.notifyItemChanged(position);
              }
            });
  }

  void updatePackagesList(boolean doRepeatUpdates) {
    if (mMySettings.isDebug()) {
      Util.debugLog(TAG, "updatePackagesList: doRepeatUpdates: " + doRepeatUpdates);
    }
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
    mMySettings.setDoRepeatUpdates(doRepeatUpdates);
    if (mMySettings.isDebug()) {
      Util.debugLog("setRepeatUpdates", String.valueOf(doRepeatUpdates));
    }
  }

  private void setPackageEnabledState(Package pkg) {
    if (!mMySettings.isPrivDaemonAlive()) {
      AlertDialog dialog =
          new Builder(this)
              .setPositiveButton(android.R.string.ok, (d, which) -> openDrawerForPrivileges())
              .setNegativeButton(android.R.string.cancel, null)
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
            command = Commands.DISABLE_PACKAGE + " " + command;
          } else {
            command = Commands.ENABLE_PACKAGE + " " + command;
          }

          if (mMySettings.isDebug()) {
            Util.debugLog("setPackageEnabledState", "Sending command: " + command);
          }
          Object res = mPrivDaemonHandler.sendRequest(command);
          mPackageParser.updatePackage(pkg);
          if (res != null) {
            Utils.runInFg(
                () -> Toast.makeText(App.getContext(), "Error occurred", Toast.LENGTH_LONG).show());
            Log.e("setPackageEnabledState", "Response is " + res);
          }
        });
  }

  private void showSnackBar(String text, int duration) {
    Utils.runInFg(
        () -> {
          Snackbar snackBar = Snackbar.make(mProgressBarContainer, text, duration);
          snackBar.setTextColor(getColor(R.color.dynamic_text_color));
          snackBar.getView().setBackgroundColor(getColor(R.color.dynamicBackground));
          snackBar.show();
        });
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
            if (mMySettings.isDebug()) {
              Util.debugLog("searchQueryTextSubmit", query);
            }
            handleSearchQuery(false);
            return true;
          }

          @Override
          public boolean onQueryTextChange(String newText) {
            if (mMySettings.isDebug()) {
              Util.debugLog("searchQueryTextChange", newText);
            }
            handleSearchQuery(false);
            return true;
          }
        });

    // Clear search query when no text is entered.
    mSearchView.setOnQueryTextFocusChangeListener(
        (v, hasFocus) -> {
          if (mMySettings.isDebug()) {
            Util.debugLog("searchQueryFocussed", String.valueOf(hasFocus));
          }
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
          handleSearchQuery(true);
          if (mMySettings.isDebug()) {
            Util.debugLog("deepSearch", String.valueOf(isChecked));
          }
        });

    caseSensitiveSearchSettings.setOnCheckedChangeListener(
        (buttonView, isChecked) -> {
          mMySettings.setCaseSensitiveSearch(isChecked);
          handleSearchQuery(false);
          if (mMySettings.isDebug()) {
            Util.debugLog("caseSensitiveSearch", String.valueOf(isChecked));
          }
        });

    findViewById(R.id.search_settings_container).setVisibility(View.VISIBLE);
  }

  private void collapseSearchView() {
    if (mMySettings.isDebug()) {
      Util.debugLog("searchView", "Collapsing");
    }
    mSearchView.onActionViewCollapsed();
    mSearchView.setQuery(null, false);
    handleSearchQuery(false); // mSearchView.setQuery(null, true) does not work
    findViewById(R.id.search_settings_container).setVisibility(View.GONE);
  }

  private void handleSearchQuery(boolean doDeepSearch) {
    CharSequence queryText = mSearchView.getQuery();
    boolean isSearching = mMySettings.isSearching();

    /** Save {@link queryText} to {@link MySettings#mQueryText} causes memory leak. */
    mMySettings.setQueryText(queryText.toString());

    if (TextUtils.isEmpty(queryText) && !isSearching) {
      if (mMySettings.isDebug()) {
        Util.debugLog("handleSearchQuery", "Already empty text set, returning");
      }
      return;
    }

    if (mMySettings.isDebug()) {
      Util.debugLog("handleSearchQuery", "Text set to: " + queryText);
    }

    if (doDeepSearch || mMySettings.isDeepSearchEnabled()) {
      updatePackagesList(true);
    } else {
      mPackageParser.handleSearchQuery(true);
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////////// PRIVILEGES //////////////////////////
  //////////////////////////////////////////////////////////////////

  private synchronized void startPrivDaemon(boolean isFirstRun) {
    if (!mMySettings.isPrivDaemonAlive()) {
      if (mMySettings.isDebug()) {
        Util.debugLog("startPrivDaemon", "Daemon is dead");
      }
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
            showSnackBar(message, 10000);
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

        if (mMySettings.shouldRemindMissingPrivileges()) {
          Builder builder =
              new Builder(this)
                  .setPositiveButton(
                      android.R.string.ok, (dialog, which) -> openDrawerForPrivileges())
                  .setNeutralButton(
                      R.string.do_not_remind, (d, which) -> mMySettings.setPrivReminderOff())
                  .setNegativeButton(
                      R.string.get_help,
                      (dialog, which) ->
                          startActivity(new Intent(App.getContext(), HelpActivity.class)))
                  .setTitle(R.string.privileges)
                  .setMessage(getString(R.string.grant_root_or_adb));
          Utils.runInFg(
              () -> {
                AlertDialog dialog = builder.create();
                dialog.setOnShowListener(
                    d -> {
                      // With longer button text, unnecessary bottom padding is added to dialog.
                      Button b = dialog.getButton(DialogInterface.BUTTON_NEUTRAL);
                      b.setPadding(
                          b.getPaddingLeft(),
                          b.getPaddingTop(),
                          b.getPaddingRight() / 2,
                          b.getPaddingBottom());
                    });
                new AlertDialogFragment(dialog).show(mFM, TAG_GRANT_ROOT_OR_ADB, false);
              });
        }
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
    if (mMySettings.isPrivDaemonAlive() || mMySettings.isAppOpsGranted()) {
      // If observers are set, update packages list.
      if (!isFirstRun) {
        Utils.runInFg(() -> updatePackagesList(false));
      }
    }
  }

  void restartPrivDaemon() {
    Utils.runInBg(
        () -> {
          if (mMySettings.isPrivDaemonAlive()) {
            mPrivDaemonHandler.sendRequest(Commands.SHUTDOWN);
            SystemClock.sleep(1000); // Let the previous processes cleanup
          }
          startPrivDaemon(false);
        });
  }

  private void checkAppOpsPerm() {
    if (!mMySettings.isAppOpsGranted() && mMySettings.isPrivDaemonAlive()) {
      String command =
          Commands.GRANT_PERMISSION
              + " "
              + getPackageName()
              + " "
              + APP_OPS_PERM
              + " "
              + Utils.getUserId();

      if (mMySettings.isDebug()) {
        Util.debugLog("startPrivDaemon", "Sending command: " + command);
      }
      mPrivDaemonHandler.sendRequest(command);

      if (!mMySettings.isAppOpsGranted()) {
        Log.e("startPrivDaemon", "Granting " + APP_OPS_PERM + " failed");
        showSnackBar(getString(R.string.granting_permission_failed) + ": " + APP_OPS_PERM, 10000);
      }
    }
  }

  //////////////////////////////////////////////////////////////////
  //////////////////////// NAVIGATION DRAWER ///////////////////////
  //////////////////////////////////////////////////////////////////

  private void setNavigationMenu() {
    if (mMySettings.isDebug()) {
      Util.debugLog("setNavigationMenu", "Called");
    }
    Menu menu = mNavigationView.getMenu();

    // if recreating
    mNavigationView.invalidate();

    setBoxCheckedAndSetListener(menu, R.id.action_root, mMySettings.isRootGranted());
    setBoxCheckedAndSetListener(menu, R.id.action_adb, mMySettings.isAdbConnected());
    setBoxCheckedAndSetListener(menu, R.id.action_dark_theme, mMySettings.forceDarkMode());

    menu.findItem(R.id.action_donate).setVisible(BuildConfig.GH_VERSION);
  }

  private void setBoxCheckedAndSetListener(Menu menu, int id, boolean checked) {
    MenuItem menuItem = menu.findItem(id);
    CheckBox checkBox = ((CheckBox) menuItem.getActionView());
    checkBox.setChecked(checked);
    checkBox.setOnClickListener(v -> handleNavigationItemChecked(menuItem));
  }

  private boolean handleNavigationItemSelected(MenuItem item) {
    if (mMySettings.isDebug()) {
      Util.debugLog("handleNavigationItemSelected", item.getTitle().toString());
    }
    View view = item.getActionView();
    if (view instanceof CheckBox) {
      CheckBox checkBox = (CheckBox) view;
      checkBox.setChecked(!checkBox.isChecked());
    }
    return handleNavigationItemChecked(item);
  }

  private boolean handleNavigationItemChecked(MenuItem item) {
    if (mMySettings.isDebug()) {
      Util.debugLog("handleNavigationItemChecked", item.getTitle().toString());
    }
    mDrawerLayout.closeDrawer(GravityCompat.START, true);

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
        mMySettings.setRootGranted(false);
        return true;
      }

      rootCheckBox.setChecked(false);

      Utils.runInBg(
          () -> {
            if (Utils.checkRoot()) {
              showSnackBar(getString(R.string.root_granted), 5000);
              Utils.runInFg(() -> rootCheckBox.setChecked(true));
              restartPrivDaemon();
            } else {
              showSnackBar(
                  getString(R.string.getting_root_fail) + getString(R.string.are_you_rooted),
                  10000);
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
              showSnackBar(getString(R.string.connected_to_adb), 5000);
              Utils.runInFg(() -> adbCheckBox.setChecked(true));
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
      AdvancedSettings.showDialog(this);
      return true;
    }

    if (item.getItemId() == R.id.action_dark_theme) {
      CheckBox darkCheckBox = (CheckBox) item.getActionView();
      mMySettings.setForceDarkMode(darkCheckBox.isChecked());
      setNightTheme();
      return true;
    }

    if (item.getItemId() == R.id.action_backup_restore) {
      new BackupRestore(this).doBackupRestore();
      return true;
    }

    if (item.getItemId() == R.id.action_help) {
      startActivity(new Intent(App.getContext(), HelpActivity.class));
      return true;
    }

    if (item.getItemId() == R.id.action_donate) {
      Donate.showDialog(this);
      return true;
    }

    if (item.getItemId() == R.id.action_about) {
      startActivity(new Intent(App.getContext(), AboutActivity.class));
      return true;
    }

    return false;
  }

  private void openDrawerForPrivileges() {
    Utils.runInBg(
        () -> {
          while (getWindow() == null) {
            SystemClock.sleep(100);
          }
          if (mDrawerLayout != null) {
            Utils.runInFg(() -> mDrawerLayout.openDrawer(GravityCompat.START));
          }
          float f = new Random().nextBoolean() ? 360 : -360;
          Utils.runInBg(() -> rotateMenuItemCheckbox(R.id.action_root, f));
          Utils.runInBg(() -> rotateMenuItemCheckbox(R.id.action_adb, -1 * f));
        });
  }

  private void rotateMenuItemCheckbox(int resId, float angle) {
    SystemClock.sleep(1000);
    if (mNavigationView != null) {
      Utils.runInFg(
          () ->
              mNavigationView
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
  ///////////////////////////// LOGGING ////////////////////////////
  //////////////////////////////////////////////////////////////////

  private void startLogging() {
    mMySettings.setLogging(null);
    Util.debugLog("Logging", "Start logging");
    String command = "logcat --pid " + Process.myPid();

    if (Utils.doLoggingFails(new String[] {command})) {
      Utils.stopLogging();
      showSnackBar(getString(R.string.logging_failed), 10000);
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

  MyFrameLayout getRoundProgressContainer() {
    return mRoundProgressContainer;
  }

  TextView getRoundProgressTextView() {
    return mRoundProgressTextView;
  }

  @SuppressWarnings("UnusedDeclaration")
  SearchView getSearchView() {
    return mSearchView;
  }

  MainActivityFlavor getMainActivityFlavor() {
    return mMainActivityFlavor;
  }
}
