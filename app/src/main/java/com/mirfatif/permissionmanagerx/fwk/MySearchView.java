package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import androidx.appcompat.widget.SearchView;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class MySearchView extends SearchView {

  public MySearchView(Context context) {
    super(context);
    UiUtils.onCreateLayout(this);
  }
}
