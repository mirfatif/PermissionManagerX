package com.mirfatif.permissionmanagerx.fwk;

import android.app.Activity;
import android.content.Context;
import android.content.res.ColorStateList;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.InsetDrawable;
import android.graphics.drawable.RippleDrawable;
import android.graphics.drawable.StateListDrawable;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.util.AttributeSet;
import android.view.MotionEvent;
import androidx.appcompat.widget.AppCompatButton;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.UiUtils;

public class CustomButton extends AppCompatButton {

  private static final int DP4_TO_PX = UiUtils.dpToPx(4);

  public CustomButton(Context context, AttributeSet attrs) {
    super(context, attrs);

    GradientDrawable normalBg = new GradientDrawable();
    normalBg.setShape(GradientDrawable.RECTANGLE);
    normalBg.setColor(UiUtils.getDimBgColor((Activity) context));
    normalBg.setCornerRadius(DP4_TO_PX * 3f / 2);
    if (VERSION.SDK_INT >= VERSION_CODES.Q) {

      normalBg.setPadding(2 * DP4_TO_PX, DP4_TO_PX, 2 * DP4_TO_PX, DP4_TO_PX);
    }

    int rippleColor = context.getColor(R.color.colorControlNormalA50);
    ColorStateList colorStateList =
        new ColorStateList(new int[][] {new int[] {}}, new int[] {rippleColor});
    mRipple = new RippleDrawable(colorStateList, null, normalBg);

    mRipple.setPadding(2 * DP4_TO_PX, DP4_TO_PX, 2 * DP4_TO_PX, DP4_TO_PX);

    StateListDrawable d = new StateListDrawable();

    d.addState(new int[] {android.R.attr.state_pressed}, new InsetDrawable(mRipple, DP4_TO_PX));
    d.addState(new int[] {}, new InsetDrawable(normalBg, DP4_TO_PX));
    setBackground(d);
  }

  private final RippleDrawable mRipple;

  public boolean onTouchEvent(MotionEvent event) {
    if (event.getAction() == MotionEvent.ACTION_DOWN && mRipple != null) {
      mRipple.setHotspot(event.getX(), event.getY());
    }
    return super.onTouchEvent(event);
  }
}
