package com.mirfatif.permissionmanagerx.parser;

import android.content.res.TypedArray;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import java.util.Arrays;
import java.util.List;

public enum PermGroupsMapping {
  INS;

  private final List<String> PERMS =
      Arrays.asList(App.getRes().getStringArray(R.array.perm_groups_perms));

  private final List<String> APP_OPS =
      Arrays.asList(App.getRes().getStringArray(R.array.perm_groups_app_ops));

  private final int[] ICONS;

  {
    TypedArray icons = App.getRes().obtainTypedArray(R.array.perm_groups_icons);
    ICONS = new int[icons.length()];
    for (int i = 0; i < ICONS.length; i++) {
      ICONS[i] = icons.getResourceId(i, 0);
    }
    icons.recycle();
  }

  PermGroupInfo get(String perm, boolean isAppOp) {
    int i = isAppOp ? APP_OPS.indexOf(perm) : PERMS.indexOf(perm);
    return new PermGroupInfo(
        i >= 0 ? i : ICONS.length, i >= 0 && ICONS[i] != 0 ? ICONS[i] : R.drawable.g_others);
  }

  static class PermGroupInfo {

    public final int groupId;
    public final int icon;

    private PermGroupInfo(int groupId, int icon) {
      this.groupId = groupId;
      this.icon = icon;
    }
  }
}
