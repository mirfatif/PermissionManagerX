package com.mirfatif.permissionmanagerx.util;

import android.graphics.Canvas;
import android.graphics.Paint;
import android.text.style.ReplacementSpan;

public class SmallDimMarginSpan extends ReplacementSpan {

  public int getSize(Paint paint, CharSequence text, int start, int end, Paint.FontMetricsInt fm) {
    if (fm != null) {
      fm.ascent = fm.ascent - fm.bottom / 4;
      fm.bottom *= 2;
    }
    return Math.round(paint.measureText(text, start, end));
  }

  public void draw(
      Canvas canvas,
      CharSequence text,
      int start,
      int end,
      float x,
      int top,
      int y,
      int bottom,
      Paint paint) {
    paint.setTextSize(paint.getTextSize() * 4 / 5);
    paint.setAlpha(170);
    canvas.drawText(text, start, end, x, y, paint);
  }
}
