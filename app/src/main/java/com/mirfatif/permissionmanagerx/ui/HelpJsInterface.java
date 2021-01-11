package com.mirfatif.permissionmanagerx.ui;

import android.app.Activity;
import android.graphics.Color;
import android.webkit.JavascriptInterface;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.prefs.MySettings;

public class HelpJsInterface {

  private final String mBgColor, mTextColor, mLinkColor;

  HelpJsInterface(Activity act) {
    boolean isNightMode = MySettings.getInstance().forceDarkMode() || Utils.isNightMode(act);
    mBgColor = Utils.colorIntToRGB(Utils.getColor(act, android.R.attr.colorBackground), false);
    mTextColor = Utils.colorIntToRGB(isNightMode ? Color.WHITE : Color.BLACK, false);
    mLinkColor = Utils.colorIntToRGB(Utils.getColor(act, R.attr.accentColor), false);
  }

  @JavascriptInterface
  @SuppressWarnings("UnusedDeclaration")
  public String getBgColor() {
    return mBgColor;
  }

  @JavascriptInterface
  @SuppressWarnings("UnusedDeclaration")
  public String getTextColor() {
    return mTextColor;
  }

  @JavascriptInterface
  @SuppressWarnings("UnusedDeclaration")
  public String getLinkColor() {
    return mLinkColor;
  }

  @JavascriptInterface
  @SuppressWarnings("UnusedDeclaration")
  public String getBulletPadding() {
    return Utils.dpToPx(6f) + "px";
  }

  @JavascriptInterface
  @SuppressWarnings("UnusedDeclaration")
  public String getParaMargin() {
    return Utils.dpToPx(2f) + "px";
  }

  @JavascriptInterface
  @SuppressWarnings("UnusedDeclaration")
  public String getImgMaxWidth() {
    return Utils.dpToPx(200f) + "px";
  }
}
