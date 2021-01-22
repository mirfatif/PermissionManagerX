package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.LinearLayout;
import com.mirfatif.permissionmanagerx.util.UtilsFlavor;

public class MyLinearLayout extends LinearLayout {

  public MyLinearLayout(Context context) {
    super(context);
    UtilsFlavor.onCreateLayout(this);
  }

  public MyLinearLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
    UtilsFlavor.onCreateLayout(this);
  }

  public MyLinearLayout(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
    UtilsFlavor.onCreateLayout(this);
  }

  @SuppressWarnings("UnusedDeclaration")
  public MyLinearLayout(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
    super(context, attrs, defStyleAttr, defStyleRes);
    UtilsFlavor.onCreateLayout(this);
  }
}
