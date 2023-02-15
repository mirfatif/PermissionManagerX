package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.util.AttributeSet;
import androidx.drawerlayout.widget.DrawerLayout;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class MyDrawerLayout extends DrawerLayout {

  public MyDrawerLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
    UiUtils.onCreateLayout(this);
  }
}
