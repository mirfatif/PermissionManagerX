package com.mirfatif.permissionmanagerx.util;

import android.animation.LayoutTransition;
import android.app.Activity;
import android.app.Dialog;
import android.content.DialogInterface;
import android.content.res.ColorStateList;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.Typeface;
import android.text.style.TextAppearanceSpan;
import android.util.TypedValue;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.Toast;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.widget.TooltipCompat;
import androidx.core.graphics.ColorUtils;
import com.google.android.material.color.MaterialColors;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.DialogBg;
import com.mirfatif.permissionmanagerx.util.bg.UiRunner;

public class UiUtils {

  private UiUtils() {}

  public static boolean isNightMode(Activity activity) {
    int uiMode = activity.getResources().getConfiguration().uiMode;
    return (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
  }

  public static int dpToPx(float dp) {
    return (int)
        TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP, dp, Resources.getSystem().getDisplayMetrics());
  }

  public static String colorIntToRGB(int color, boolean retainAlpha) {
    if (retainAlpha) {
      return String.format("#%08X", color);
    } else {
      return String.format("#%06X", 0xFFFFFF & color);
    }
  }

  public static int getColor(Activity activity, int colorAttrResId) {
    return MaterialColors.getColor(activity, colorAttrResId, Color.TRANSPARENT);
  }

  public static int getBgColor(Activity activity) {
    return getColor(activity, android.R.attr.windowBackground);
  }

  public static int getSharpBgColor(Activity activity) {
    return ColorUtils.blendARGB(
        getBgColor(activity), isNightMode(activity) ? Color.BLACK : Color.WHITE, 0.05f);
  }

  public static int getDimBgColor(Activity activity) {
    return ColorUtils.blendARGB(
        getBgColor(activity), isNightMode(activity) ? Color.WHITE : Color.BLACK, 0.05f);
  }

  public static void onCreateLayout(ViewGroup view) {
    LayoutTransition transition = new LayoutTransition();
    transition.enableTransitionType(LayoutTransition.CHANGING);
    view.setLayoutTransition(transition);
  }

  public static void onCreateDialog(Dialog dialog, Activity activity) {
    Window window = dialog.getWindow();
    if (window == null) {
      return;
    }

    window.setBackgroundDrawable(new DialogBg(activity, true));

    window.setWindowAnimations(android.R.style.Animation_Dialog);
  }

  public static AlertDialog removeButtonPadding(AlertDialog dialog) {
    dialog.setOnShowListener(
        d -> {
          Button b = dialog.getButton(DialogInterface.BUTTON_NEUTRAL);
          int padding = dpToPx(4);
          b.setPadding(b.getPaddingLeft(), padding, padding, padding);
        });
    return dialog;
  }

  public static TextAppearanceSpan getTextHighlightSpan(int colorInt) {
    return new TextAppearanceSpan(
        null,
        Typeface.NORMAL,
        -1,
        new ColorStateList(new int[][] {new int[] {}}, new int[] {colorInt}),
        null);
  }

  public static void showToast(String msg) {
    if (msg != null) {
      UiRunner.post(() -> Toast.makeText(App.getCxt(), msg, Toast.LENGTH_LONG).show());
    }
  }

  public static void showToast(int resId, Object... args) {
    if (resId != 0) {
      showToast(ApiUtils.getString(resId, args));
    }
  }

  public static void setTooltip(ImageView imageView) {
    TooltipCompat.setTooltipText(imageView, imageView.getContentDescription());
  }
}
