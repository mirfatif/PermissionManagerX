package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.util.AttributeSet;
import android.webkit.WebView;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class MyWebView extends WebView {

  public MyWebView(Context context, AttributeSet attrs) {
    super(context, attrs);
    UiUtils.onCreateLayout(this);
  }
}
