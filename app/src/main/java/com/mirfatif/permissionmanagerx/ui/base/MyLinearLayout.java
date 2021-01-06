package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.LinearLayout;
import com.mirfatif.permissionmanagerx.main.MainActivityFlavor;

public class MyLinearLayout extends LinearLayout {

  public MyLinearLayout(Context context) {
    super(context);
    MainActivityFlavor.onCreateLayout(this);
  }

  public MyLinearLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
    MainActivityFlavor.onCreateLayout(this);
  }

  public MyLinearLayout(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
    MainActivityFlavor.onCreateLayout(this);
  }

  @SuppressWarnings("UnusedDeclaration")
  public MyLinearLayout(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
    super(context, attrs, defStyleAttr, defStyleRes);
    MainActivityFlavor.onCreateLayout(this);
  }
}
