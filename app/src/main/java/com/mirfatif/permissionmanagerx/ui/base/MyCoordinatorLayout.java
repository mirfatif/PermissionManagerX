package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.util.AttributeSet;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.coordinatorlayout.widget.CoordinatorLayout;
import com.mirfatif.permissionmanagerx.util.Utils;

public class MyCoordinatorLayout extends CoordinatorLayout {

  public MyCoordinatorLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
    super(context, attrs);
    Utils.onCreateLayout(this);
  }
}
