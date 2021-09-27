package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.LinearLayout;
import com.mirfatif.permissionmanagerx.util.UtilsFlavor;

public class MyLinearLayout extends LinearLayout {

  public MyLinearLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
    UtilsFlavor.onCreateLayout(this);
  }
}
