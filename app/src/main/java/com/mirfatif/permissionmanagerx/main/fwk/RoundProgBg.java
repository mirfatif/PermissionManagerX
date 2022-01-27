package com.mirfatif.permissionmanagerx.main.fwk;

import android.app.Activity;
import android.graphics.drawable.GradientDrawable;
import androidx.core.graphics.ColorUtils;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.Utils;

public class RoundProgBg extends GradientDrawable {

  public RoundProgBg(Activity activity) {
    setShape(GradientDrawable.OVAL);
    setColor(ColorUtils.setAlphaComponent(Utils.getSharpBgColor(activity), 255 * 97 / 100));
    setStroke(Utils.dpToPx(2), Utils.getColor(activity, R.attr.accentTransColor));
  }
}
