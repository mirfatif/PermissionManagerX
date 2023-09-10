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

    if (PERMS.size() != APP_OPS.size() || PERMS.size() != ICONS.length) {
      throw new RuntimeException("Permission arrays size mismatch");
    }
  }

  public PermGroupInfo get(String perm, boolean isAppOp) {
    int id = getGroupId(perm, isAppOp);
    return new PermGroupInfo(
        id, id < ICONS.length && ICONS[id] != 0 ? ICONS[id] : R.drawable.g_others);
  }

  public int getGroupId(String perm, boolean isAppOp) {
    int i = isAppOp ? APP_OPS.indexOf(perm) : PERMS.indexOf(perm);
    return i >= 0 ? i : ICONS.length;
  }

  public static class PermGroupInfo {

    public final int groupId;
    public final int icon;

    private PermGroupInfo(int groupId, int icon) {
      this.groupId = groupId;
      this.icon = icon;
    }
  }
}
