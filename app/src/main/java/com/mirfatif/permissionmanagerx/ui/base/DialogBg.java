package com.mirfatif.permissionmanagerx.ui.base;

import android.app.Activity;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.InsetDrawable;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.Utils;

public class DialogBg extends InsetDrawable {

  private static final int DP16_TO_PX = Utils.dpToPx(16);

  public DialogBg(Activity activity, boolean bordered) {
    super(
        createDrawable(activity, false, bordered),
        DP16_TO_PX / 2,
        DP16_TO_PX,
        DP16_TO_PX / 2,
        DP16_TO_PX);
  }

  public DialogBg(boolean isBottom, Activity activity) {
    super(
        createDrawable(activity, isBottom, false),
        isBottom ? 0 : DP16_TO_PX / 2,
        isBottom ? 0 : DP16_TO_PX,
        isBottom ? 0 : DP16_TO_PX / 2,
        isBottom ? 0 : DP16_TO_PX);
  }

  private static Drawable createDrawable(Activity activity, boolean isBottom, boolean bordered) {
    GradientDrawable d = new GradientDrawable();
    d.setShape(GradientDrawable.RECTANGLE);
    d.setColor(Utils.getBgColor(activity));
    if (isBottom) {
      float[] radii = new float[8];
      radii[0] = radii[1] = radii[2] = radii[3] = DP16_TO_PX;
      d.setCornerRadii(radii);
    } else {
      d.setCornerRadius(DP16_TO_PX);
      if (bordered) {
        d.setStroke(DP16_TO_PX / 8, Utils.getColor(activity, R.attr.accentColor));
      } else {
        d.setStroke(DP16_TO_PX / 8, Utils.getColor(activity, R.attr.accentTransColor));
      }
    }
    return d;
  }
}
