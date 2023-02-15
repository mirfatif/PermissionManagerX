package com.mirfatif.permissionmanagerx.help;

import android.app.Activity;
import android.graphics.Color;
import android.webkit.JavascriptInterface;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class HelpJsInterface {

  private final String mBgColor, mTextColor, mThemeColor;
  private final boolean mNightMode;

  HelpJsInterface(Activity act) {
    mBgColor = UiUtils.colorIntToRGB(UiUtils.getColor(act, android.R.attr.colorBackground), false);
    mTextColor = UiUtils.colorIntToRGB(UiUtils.isNightMode(act) ? Color.WHITE : Color.BLACK, false);
    mThemeColor = UiUtils.colorIntToRGB(UiUtils.getColor(act, R.attr.accentColor), false);
    mNightMode = UiUtils.isNightMode(act);
  }

  @JavascriptInterface
  public String getBgColor() {
    return mBgColor;
  }

  @JavascriptInterface
  public String getTextColor() {
    return mTextColor;
  }

  @JavascriptInterface
  public String getThemeColor() {
    return mThemeColor;
  }

  @JavascriptInterface
  public boolean isDarkTheme() {
    return mNightMode;
  }
}
