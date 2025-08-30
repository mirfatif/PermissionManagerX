package com.mirfatif.permissionmanagerx.util;

import static android.text.Spanned.SPAN_EXCLUSIVE_EXCLUSIVE;
import static android.text.style.DynamicDrawableSpan.ALIGN_BASELINE;
import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.graphics.drawable.Drawable;
import android.os.Parcel;
import android.text.Html;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.BulletSpan;
import android.text.style.ImageSpan;
import android.text.style.RelativeSizeSpan;
import android.text.style.TextAppearanceSpan;
import android.text.style.URLSpan;
import androidx.core.content.res.ResourcesCompat;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringUtils {

  private StringUtils() {}

  public static Spanned htmlToString(int resId) {
    return htmlToString(getString(resId));
  }

  public static Spanned htmlToString(String str) {
    Spanned spanned = Html.fromHtml(str, Html.FROM_HTML_MODE_COMPACT);

    if (str.isEmpty()) {
      return spanned;
    }

    SpannableStringBuilder string = new SpannableStringBuilder(spanned);

    Parcel parcel = Parcel.obtain();
    parcel.writeInt(UiUtils.dpToPx(4));
    parcel.writeInt(0);
    parcel.writeInt(0);
    parcel.writeInt(UiUtils.dpToPx(2));

    for (BulletSpan span : string.getSpans(0, string.length(), BulletSpan.class)) {
      int start = string.getSpanStart(span);
      int end = string.getSpanEnd(span);
      string.removeSpan(span);
      parcel.setDataPosition(0);
      string.setSpan(new BulletSpan(parcel), start, end, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
    }

    parcel.recycle();

    Drawable d = ResourcesCompat.getDrawable(App.getRes(), R.drawable.link, null);
    if (d != null) {
      d.setTint(UiUtils.getAccentColor());
      d.setBounds(0, 0, UiUtils.dpToPx(12), UiUtils.dpToPx(12));

      for (URLSpan span : string.getSpans(0, string.length(), URLSpan.class)) {
        int start = string.getSpanStart(span);
        int end = string.getSpanEnd(span);
        if (!string.toString().substring(start, end).equals("LINK")) {
          continue;
        }
        string.setSpan(new ImageSpan(d, ALIGN_BASELINE), start, end, SPAN_EXCLUSIVE_EXCLUSIVE);
      }
    }

    return breakParas(string);
  }

  public static SpannableStringBuilder breakParas(String string) {
    return breakParas(new SpannableStringBuilder(string));
  }

  public static SpannableStringBuilder breakParas(SpannableStringBuilder string) {
    int len = string.length();

    if (len == 0) {
      return string;
    }

    while ((len = string.length()) > 0 && string.charAt(len - 1) == '\n') {
      string.delete(len - 1, len);
    }

    Matcher matcher = Pattern.compile("\n").matcher(string);
    int from = 0;
    while (matcher.find(from)) {
      string.replace(matcher.start(), matcher.end(), "\n\n");
      from = matcher.end() + 1;

      string.setSpan(
          new RelativeSizeSpan(0.25f),
          matcher.start() + 1,
          matcher.end() + 1,
          Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

      matcher = Pattern.compile("\n").matcher(string);
    }

    return string;
  }

  public static SpannableString getHighlightString(
      String text, TextAppearanceSpan highlightSpan, boolean isCaseSensitive, String... prominent) {

    if (text == null || highlightSpan == null || prominent == null) {
      return null;
    }

    SpannableString spannable = new SpannableString(text);

    Parcel parcel = Parcel.obtain();
    highlightSpan.writeToParcel(parcel, 0);

    for (String prom : prominent) {
      if (prom == null || prom.length() == 0) {
        continue;
      }
      int startPos;
      if (isCaseSensitive) {
        startPos = text.indexOf(prom);
      } else {
        startPos = text.toUpperCase().indexOf(prom.toUpperCase());
      }
      if (startPos < 0) {
        continue;
      }
      int endPos = startPos + prom.length();
      parcel.setDataPosition(0);
      spannable.setSpan(
          new TextAppearanceSpan(parcel), startPos, endPos, Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
    }
    parcel.recycle();
    return spannable;
  }
}
