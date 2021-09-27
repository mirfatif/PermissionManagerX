package com.mirfatif.permissionmanagerx.ui.base;

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.SearchView;
import com.mirfatif.permissionmanagerx.util.UtilsFlavor;

public class MySearchView extends SearchView {

  public MySearchView(@NonNull Context context) {
    super(context);
    UtilsFlavor.onCreateLayout(this);
  }
}
