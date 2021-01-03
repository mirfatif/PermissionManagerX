package com.mirfatif.permissionmanagerx.ui;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import androidx.annotation.NonNull;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewClientCompat;
import androidx.webkit.WebViewFeature;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;

public class HelpActivity extends BaseActivity {

  private WebSettings mWebSettings;
  private final MySettings mMySettings = MySettings.getInstance();

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_help);

    if (getSupportActionBar() != null) {
      getSupportActionBar().setTitle(R.string.help_menu_item);
    }

    WebView webView = findViewById(R.id.help_web_view);
    mWebSettings = webView.getSettings();

    if (WebViewFeature.isFeatureSupported(WebViewFeature.FORCE_DARK)) {
      int mode = WebSettingsCompat.FORCE_DARK_AUTO;
      if (MySettings.getInstance().forceDarkMode() || Utils.isNightMode(this)) {
        mode = WebSettingsCompat.FORCE_DARK_ON;
      }
      WebSettingsCompat.setForceDark(mWebSettings, mode);
    }

    mFontSize = mMySettings.getIntPref(R.string.pref_help_font_size_key);
    setFontSize();

    mWebSettings.setSupportZoom(false);

    webView.setWebViewClient(new MyWebViewClient());
    webView.loadUrl("file:///android_asset/help.html");
  }

  private int mFontSize;

  private void setFontSize() {
    mWebSettings.setDefaultFontSize(mFontSize);
    if (mFontSize >= 23 || mFontSize <= 11) {
      invalidateOptionsMenu();
    }
    mMySettings.savePref(R.string.pref_help_font_size_key, mFontSize);
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.help_zoom, menu);
    return super.onCreateOptionsMenu(menu);
  }

  @Override
  public boolean onPrepareOptionsMenu(Menu menu) {
    menu.findItem(R.id.action_zoom_in).setEnabled(mFontSize < 24);
    menu.findItem(R.id.action_zoom_out).setEnabled(mFontSize > 10);
    return true;
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
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
      if (url.startsWith("http://") || url.startsWith("https://")) {
        Utils.openWebUrl(HelpActivity.this, url);
        return true;
      }
      if (url.startsWith("activity://") && url.contains("about")) {
        startActivity(new Intent(App.getContext(), AboutActivity.class));
        return true;
      }
      return super.shouldOverrideUrlLoading(view, request);
    }
  }
}
