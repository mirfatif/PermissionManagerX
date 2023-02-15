package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.util.AttributeSet;
import androidx.coordinatorlayout.widget.CoordinatorLayout;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class MyCoordinatorLayout extends CoordinatorLayout {

  public MyCoordinatorLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
    UiUtils.onCreateLayout(this);
  }
}
