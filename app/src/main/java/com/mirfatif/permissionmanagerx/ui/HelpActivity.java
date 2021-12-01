package com.mirfatif.permissionmanagerx.ui;

import static com.mirfatif.permissionmanagerx.prefs.MySettings.SETTINGS;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import androidx.annotation.NonNull;
import androidx.webkit.WebViewClientCompat;
import androidx.webkit.WebViewCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.about.AboutActivity;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.ActivityHelpBinding;
import com.mirfatif.permissionmanagerx.ui.base.BaseActivity;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.util.ArrayList;
import java.util.List;

public class HelpActivity extends BaseActivity {

  private WebSettings mWebSettings;
  private ActivityHelpBinding mB;

  private static final String HELP_URL = "https://mirfatif.github.io/PermissionManagerX/help/";
  private static final String CONTACT_URL =
      "https://mirfatif.github.io/PermissionManagerX/#contact-us";

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    if (Utils.setNightTheme(this)) {
      return;
    }

    mB = ActivityHelpBinding.inflate(getLayoutInflater());

    /*
     WebView resets App's configuration when loaded for the first time.
     https://stackoverflow.com/questions/40398528
    */
    App.setLocale();
    Utils.setLocale(getBaseContext());

    setContentView(mB.getRoot());

    if (getSupportActionBar() != null) {
      getSupportActionBar().setTitle(R.string.help_menu_item);
    }

    mWebSettings = mB.webView.getSettings();

    mFontSize = SETTINGS.getIntPref(R.string.pref_help_font_size_key);
    setFontSize();

    mWebSettings.setSupportZoom(false);
    mWebSettings.setBlockNetworkLoads(false);
    mWebSettings.setBlockNetworkImage(false);
    mWebSettings.setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);
    mB.webView.setWebViewClient(new MyWebViewClient());

    enableJs();
    mB.webView.addJavascriptInterface(new HelpJsInterface(this), "Android");

    if (SETTINGS.isAppUpdated()) {
      mB.webView.clearCache(true);
    }

    mB.webView.loadUrl(getLocalizedHelpUrl());
  }

  @SuppressLint("SetJavaScriptEnabled")
  private void enableJs() {
    mWebSettings.setJavaScriptEnabled(true);
  }

  private int mFontSize;

  private void setFontSize() {
    mWebSettings.setDefaultFontSize(mFontSize);
    mB.webView.reload(); // To update images size
    if (mFontSize > MAX_FONT_SIZE - 2 || mFontSize < MIN_FONT_SIZE + 2) {
      invalidateOptionsMenu();
    }
    SETTINGS.savePref(R.string.pref_help_font_size_key, mFontSize);
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.help_menu, menu);
    return super.onCreateOptionsMenu(menu);
  }

  private static final int MAX_FONT_SIZE = 24;
  private static final int MIN_FONT_SIZE = 10;

  @Override
  public boolean onPrepareOptionsMenu(Menu menu) {
    menu.findItem(R.id.action_zoom_in).setEnabled(mFontSize < MAX_FONT_SIZE);
    menu.findItem(R.id.action_zoom_out).setEnabled(mFontSize > MIN_FONT_SIZE);
    return true;
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    if (item.getItemId() == R.id.action_help_reload) {
      mB.webView.clearCache(true);
      mB.webView.reload();
      return true;
    }
    if (item.getItemId() == R.id.action_open_browser) {
      Utils.openWebUrl(this, getLocalizedHelpUrl());
      finish();
      return true;
    }
    if (item.getItemId() == R.id.action_zoom_in) {
      mFontSize++;
      setFontSize();
      return true;
    }
    if (item.getItemId() == R.id.action_zoom_out) {
      mFontSize--;
      setFontSize();
      return true;
    }
    return super.onOptionsItemSelected(item);
  }

  private class MyWebViewClient extends WebViewClientCompat {

    @Override
    public boolean shouldOverrideUrlLoading(@NonNull WebView view, WebResourceRequest request) {
      String url = request.getUrl().toString();
      if (url.equals(CONTACT_URL)) {
        startActivity(new Intent(App.getContext(), AboutActivity.class));
        return true;
      }
      if (!url.startsWith(HELP_URL)) {
        Utils.openWebUrl(HelpActivity.this, url);
        return true;
      }
      return super.shouldOverrideUrlLoading(view, request);
    }
  }

  private static String getLocalizedHelpUrl() {
    return HELP_URL + Utils.getString(R.string.help_file_name);
  }

  private static final List<String> WEB_VIEW_PKGS = new ArrayList<>();

  static {
    WEB_VIEW_PKGS.add("com.android.webview");
    WEB_VIEW_PKGS.add("com.google.android.webview");
    WEB_VIEW_PKGS.add("com.android.chrome");
  }

  public static void start(Activity activity) {
    // PackageManager.FEATURE_WEBVIEW is always true even if WebView is disabled.
    PackageInfo pkgInfo = WebViewCompat.getCurrentWebViewPackage(activity);
    if (pkgInfo == null) {
      Utils.showToast(R.string.no_web_view);
      Utils.openWebUrl(activity, getLocalizedHelpUrl());
      return;
    }

    /*
     Workaround for https://bugs.chromium.org/p/chromium/issues/detail?id=925887.
     A sensible way is to use framework WebViewClient, not WebViewClientCompat.
    */
    if (WEB_VIEW_PKGS.contains(pkgInfo.packageName)) {
      try {
        int ver = Integer.parseInt(pkgInfo.versionName.split("\\.")[0]);
        if (ver < 73) {
          Utils.showToast(R.string.outdated_web_view);
          Utils.openWebUrl(activity, getLocalizedHelpUrl());
          return;
        }
      } catch (NumberFormatException ignored) {
      }
    }

    activity.startActivity(new Intent(activity, HelpActivity.class));
  }
}
