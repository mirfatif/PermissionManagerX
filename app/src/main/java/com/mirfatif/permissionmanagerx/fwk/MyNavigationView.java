package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.util.AttributeSet;
import com.google.android.material.navigation.NavigationView;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class MyNavigationView extends NavigationView {

  public MyNavigationView(Context context, AttributeSet attrs) {
    super(context, attrs);
    UiUtils.onCreateLayout(this);
  }
}
