package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.util.AttributeSet;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.google.android.material.navigation.NavigationView;
import com.mirfatif.permissionmanagerx.main.MainActivityFlavor;

public class MyNavigationView extends NavigationView {

  public MyNavigationView(@NonNull Context context) {
    super(context);
    MainActivityFlavor.onCreateLayout(this);
  }

  public MyNavigationView(@NonNull Context context, @Nullable AttributeSet attrs) {
    super(context, attrs);
    MainActivityFlavor.onCreateLayout(this);
  }

  public MyNavigationView(
      @NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
    MainActivityFlavor.onCreateLayout(this);
  }
}
