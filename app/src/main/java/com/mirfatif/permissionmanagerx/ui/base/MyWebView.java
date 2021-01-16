package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.util.AttributeSet;
import android.webkit.WebView;
import com.mirfatif.permissionmanagerx.main.MainActivityFlavor;

public class MyWebView extends WebView {

  public MyWebView(Context context) {
    super(context);
  }

  public MyWebView(Context context, AttributeSet attrs) {
    super(context, attrs);
    MainActivityFlavor.onCreateLayout(this);
  }

  public MyWebView(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
    MainActivityFlavor.onCreateLayout(this);
  }

  @SuppressWarnings("UnusedDeclaration")
  public MyWebView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
    super(context, attrs, defStyleAttr, defStyleRes);
    MainActivityFlavor.onCreateLayout(this);
  }
}
