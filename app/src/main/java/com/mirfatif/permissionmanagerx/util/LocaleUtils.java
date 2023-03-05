package com.mirfatif.permissionmanagerx.util;

import android.content.Context;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.text.TextUtils;
import android.util.DisplayMetrics;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import java.text.NumberFormat;
import java.util.Locale;

public class LocaleUtils {

  private LocaleUtils() {}

  public static Context setLocale(Context context) {
    Locale locale = getLocale();
    Locale.setDefault(locale);
    sNumFmt = NumberFormat.getIntegerInstance(Locale.getDefault());
    Configuration config = setLocale(context.getResources().getConfiguration(), locale);

    updateConfiguration(context.getResources(), config, context.getResources().getDisplayMetrics());
    return context;
  }

  private static void updateConfiguration(Resources res, Configuration config, DisplayMetrics dm) {
    res.updateConfiguration(config, dm);
  }

  public static Configuration setLocale(Configuration config) {
    return setLocale(config, getLocale());
  }

  private static Configuration setLocale(Configuration config, Locale locale) {
    config = new Configuration(config);
    config.setLocale(locale);
    return config;
  }

  private static Locale getLocale() {
    String lang = MySettings.INS.getLocale();
    if (TextUtils.isEmpty(lang)) {
      return Resources.getSystem().getConfiguration().getLocales().get(0);
    } else {
      return new Locale(lang);
    }
  }

  private static NumberFormat sNumFmt = NumberFormat.getIntegerInstance(Locale.getDefault());

  public static String toLocalizedNum(long num) {
    return sNumFmt.format(num);
  }
}
