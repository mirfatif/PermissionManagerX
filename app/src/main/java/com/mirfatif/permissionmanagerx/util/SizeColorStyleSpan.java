package com.mirfatif.permissionmanagerx.util;

import android.text.TextPaint;
import android.text.style.MetricAffectingSpan;

public class SizeColorStyleSpan extends MetricAffectingSpan {

  private int mColor;

  private boolean mBold, mSuperScript;

  public SizeColorStyleSpan setColor(int color) {
    mColor = color;
    return this;
  }

  public SizeColorStyleSpan setBold() {
    mBold = true;
    return this;
  }

  public SizeColorStyleSpan setSuperScript() {
    mSuperScript = true;
    return this;
  }

  public void updateDrawState(TextPaint tp) {
    if (mColor != 0) {
      tp.setColor(mColor);
    }
    if (mSuperScript) {
      float sz = tp.getTextSize();
      tp.setTextSize(sz * 3 / 5);
      tp.baselineShift = (int) -sz / 2;
    }
    if (mBold) {
      tp.setFakeBoldText(true);
    }
  }

  public void updateMeasureState(TextPaint textPaint) {
    updateDrawState(textPaint);
  }
}
