package com.mirfatif.permissionmanagerx.about;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.openWebUrl;

import android.content.res.TypedArray;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.databinding.TransCreditsRowBinding;
import com.mirfatif.permissionmanagerx.databinding.TranslationDialogBinding;
import com.mirfatif.permissionmanagerx.util.StringUtils;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;

public class TransCreditsDialogFrag extends BottomSheetDialogFrag {

  public static void show(FragmentManager fm) {
    TransCreditsDialogFrag frag = new TransCreditsDialogFrag();
    frag.show(fm, "TRANS_CREDITS");
  }

  public View onCreateView(
      LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
    TranslationDialogBinding b = TranslationDialogBinding.inflate(mA.getLayoutInflater());

    TypedArray names = App.getRes().obtainTypedArray(R.array.locale_contributor_name_arrays);
    TypedArray links = App.getRes().obtainTypedArray(R.array.locale_contributor_link_arrays);
    String[] locales = App.getRes().getStringArray(R.array.locales);

    StringBuilder sb = new StringBuilder();
    TransCreditsRowBinding row;

    int nameArrayResId, linkArrayResId;
    String[] nameArray, linkArray;
    String link;

    BetterLinkMovementMethod moveMethod = BetterLinkMovementMethod.newInstance();
    moveMethod.setOnLinkClickListener((textView, url) -> openWebUrl(mA, url));

    for (int i = 0; i < names.length(); i++) {
      nameArrayResId = names.getResourceId(i, 0);
      linkArrayResId = links.getResourceId(i, 0);
      if (nameArrayResId == 0 || linkArrayResId == 0) {
        continue;
      }

      row = TransCreditsRowBinding.inflate(mA.getLayoutInflater());
      b.table.addView(row.getRoot());

      row.lang.setText(locales[i]);

      nameArray = App.getRes().getStringArray(nameArrayResId);
      linkArray = App.getRes().getStringArray(linkArrayResId);

      sb.setLength(0);

      for (int n = 0; n < nameArray.length; n++) {
        if (n > 0) {
          sb.append("<br />");
        }

        sb.append(nameArray[n]).append(" ");

        link = linkArray[n];
        if (link.startsWith("http://") || link.startsWith("https://")) {
          sb.append("<a href=\"").append(link).append("\">LINK</a>");
        } else if (!link.isEmpty()) {
          sb.append("(").append(link).append(")");
        }
      }

      if (sb.length() != 0) {
        row.credits.setText(StringUtils.htmlToString(sb.toString()));
      }
      row.credits.setMovementMethod(moveMethod);
    }

    names.recycle();
    links.recycle();

    b.addMyLang.setOnClickListener(
        v -> {
          openWebUrl(mA, getString(R.string.translation_link));
          dismissAllowingStateLoss();
        });
    return b.getRoot();
  }
}
