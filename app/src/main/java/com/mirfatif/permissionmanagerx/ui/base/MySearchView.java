package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import android.util.AttributeSet;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.SearchView;
import com.mirfatif.permissionmanagerx.util.UtilsFlavor;

public class MySearchView extends SearchView {

  public MySearchView(@NonNull Context context) {
    super(context);
    UtilsFlavor.onCreateLayout(this);
  }

  public MySearchView(@NonNull Context context, @Nullable AttributeSet attrs) {
    super(context, attrs);
    UtilsFlavor.onCreateLayout(this);
  }

  public MySearchView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
    UtilsFlavor.onCreateLayout(this);
  }
}
