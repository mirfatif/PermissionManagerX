package com.mirfatif.permissionmanagerx.util;

import android.text.TextPaint;
import android.text.style.MetricAffectingSpan;

public class SmallTextSpan extends MetricAffectingSpan {

  public void updateDrawState(TextPaint tp) {
    tp.setTextSize(tp.getTextSize() * 4 / 5);
  }

  public void updateMeasureState(TextPaint textPaint) {
    updateDrawState(textPaint);
  }
}
