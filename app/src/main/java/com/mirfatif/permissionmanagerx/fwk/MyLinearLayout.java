package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.LinearLayout;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class MyLinearLayout extends LinearLayout {

  public MyLinearLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
    UiUtils.onCreateLayout(this);
  }
}
