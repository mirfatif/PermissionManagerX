package com.mirfatif.permissionmanagerx.help;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuItem;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import androidx.webkit.WebViewClientCompat;
import androidx.webkit.WebViewCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.about.AboutActivity;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.ActivityHelpBinding;
import com.mirfatif.permissionmanagerx.fwk.HelpActivityM;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.LocaleUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueue;
import java.util.ArrayList;
import java.util.List;

public class HelpActivity {

  private final HelpActivityM mA;

  public HelpActivity(HelpActivityM activity) {
    mA = activity;
  }

  private static final String EXTRA_URL = HelpActivity.class.getName() + ".A";

  private WebSettings mWebSettings;
  private ActivityHelpBinding mB;
  private String mUrl;

  public void onCreated() {
    mB = ActivityHelpBinding.inflate(mA.getLayoutInflater());

    App.setLocale();
    LocaleUtils.setLocale(mA.getBaseContext());

    mA.setContentView(mB.getRoot());

    if (mA.getSupportActionBar() != null) {
      mA.getSupportActionBar().setTitle(R.string.help_menu_item);
    }

    mUrl = mA.getIntent().getStringExtra(EXTRA_URL);

    mWebSettings = mB.webV.getSettings();

    mFontSize = MySettings.INS.getHelpFontSize();
    setFontSize();

    mWebSettings.setSupportZoom(false);
    mWebSettings.setBlockNetworkLoads(false);
    mWebSettings.setBlockNetworkImage(false);
    mWebSettings.setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);
    mB.webV.setWebViewClient(new MyWebViewClient());

    enableJs();
    mB.webV.addJavascriptInterface(new HelpJsInterface(mA), "Android");

    if (MySettings.INS.shouldClearWebViewCache()) {
      mB.webV.clearCache(true);
    }

    mB.webV.loadUrl(mUrl);

    mB.refreshLayout.setOnRefreshListener(
        () -> {
          mB.webV.clearCache(true);
          mB.webV.reload();
        });
  }

  public boolean onBackPressed() {
    if (mB != null && mB.webV.canGoBack()) {
      mB.webV.goBack();
      return true;
    }
    return false;
  }

  private void enableJs() {
    mWebSettings.setJavaScriptEnabled(true);
  }

  private int mFontSize;

  private void setFontSize() {
    mWebSettings.setDefaultFontSize(mFontSize);
    mB.webV.reload();
    if (mFontSize > MAX_FONT_SIZE - 2 || mFontSize < MIN_FONT_SIZE + 2) {
      mA.invalidateOptionsMenu();
    }
    MySettings.INS.setHelpFontSize(mFontSize);
  }

  public boolean onCreateOptionsMenu(Menu menu) {
    mA.getMenuInflater().inflate(R.menu.help_menu, menu);
    return true;
  }

  private static final int MAX_FONT_SIZE = 24;
  private static final int MIN_FONT_SIZE = 10;

  public boolean onPrepareOptionsMenu(Menu menu) {
    menu.findItem(R.id.action_zoom_in).setEnabled(mFontSize < MAX_FONT_SIZE);
    menu.findItem(R.id.action_zoom_out).setEnabled(mFontSize > MIN_FONT_SIZE);
    return true;
  }

  public boolean onOptionsItemSelected(MenuItem item) {
    if (item.getItemId() == R.id.action_open_browser) {
      ApiUtils.openWebUrl(mA, mUrl);
      mA.finishAfterTransition();
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
    if (item.getItemId() == android.R.id.home) {

      mA.finishAfterTransition();
      return true;
    }
    return false;
  }

  private static final String HELP_URL = getString(R.string.github_help_url);

  private boolean mFirstRun = true;

  private class MyWebViewClient extends WebViewClientCompat {

    public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request) {
      String url = request.getUrl().toString();
      if (url.equals(getString(R.string.contact_us_url))) {
        AboutActivity.start(mA);
        return true;
      }
      if (!url.startsWith(HELP_URL)) {
        ApiUtils.openWebUrl(mA, url);
        return true;
      }
      return super.shouldOverrideUrlLoading(view, request);
    }

    public void onPageFinished(WebView view, String url) {
      mB.refreshLayout.setRefreshing(false);

      if (mFirstRun && url.contains("#")) {
        String[] splitUrl = url.split("#");
        String id = splitUrl[splitUrl.length - 1];
        if (!TextUtils.isEmpty(id)) {
          String jsf = "function(){document.getElementById('" + id + "').scrollIntoView();}";
          new LiveTasksQueue(mA, 500)
              .onUi(() -> mB.webV.loadUrl("javascript:(" + jsf + ")()"))
              .start();
        }
      }
      mFirstRun = false;
    }
  }

  private static final List<String> WEB_VIEW_PKGS = new ArrayList<>();

  static {
    WEB_VIEW_PKGS.add("com.android.webview");
    WEB_VIEW_PKGS.add("com.google.android.webview");
    WEB_VIEW_PKGS.add("com.android.chrome");
  }

  public static void start(Activity activity, String href) {
    String url = HELP_URL + "/" + getString(R.string.help_dir_name) + "/";
    if (href != null) {
      url += "#" + href;
    }

    PackageInfo pkgInfo = WebViewCompat.getCurrentWebViewPackage(activity);
    if (pkgInfo == null) {
      UiUtils.showToast(R.string.no_web_view);
      ApiUtils.openWebUrl(activity, url);
      return;
    }

    if (WEB_VIEW_PKGS.contains(pkgInfo.packageName)) {
      try {
        int ver = Integer.parseInt(pkgInfo.versionName.split("\\.")[0]);
        if (ver < 73) {
          UiUtils.showToast(R.string.outdated_web_view);
          ApiUtils.openWebUrl(activity, url);
          return;
        }
      } catch (NumberFormatException ignored) {
      }
    }

    activity.startActivity(new Intent(activity, HelpActivityM.class).putExtra(EXTRA_URL, url));
  }
}
