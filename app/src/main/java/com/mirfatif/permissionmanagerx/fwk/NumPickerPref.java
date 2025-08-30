package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import androidx.preference.DialogPreference;

public class NumPickerPref extends DialogPreference {

  public NumPickerPref(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  protected Object onGetDefaultValue(TypedArray a, int index) {
    return a.getInt(index, 0);
  }
}
