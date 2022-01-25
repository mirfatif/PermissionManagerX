package com.mirfatif.permissionmanagerx.about;

import static com.mirfatif.permissionmanagerx.util.Utils.openWebUrl;

import android.content.res.TypedArray;
import android.os.Bundle;
import android.text.Spanned;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.TranslationDialogBinding;
import com.mirfatif.permissionmanagerx.ui.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.util.Utils;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;

public class TransCreditsDialogFrag extends BottomSheetDialogFrag {

  public static void show(FragmentManager fm) {
    TransCreditsDialogFrag frag = new TransCreditsDialogFrag();
    frag.show(fm, "TRANS_CREDITS");
  }

  @Nullable
  @Override
  public View onCreateView(
      @NonNull LayoutInflater inflater,
      @Nullable ViewGroup container,
      @Nullable Bundle savedInstanceState) {
    TranslationDialogBinding b = TranslationDialogBinding.inflate(mA.getLayoutInflater());
    b.langCreditsV.setText(createTransCreditsString());
    BetterLinkMovementMethod method = BetterLinkMovementMethod.newInstance();
    method.setOnLinkClickListener((tv, url) -> openWebUrl(mA, url));
    b.langCreditsV.setMovementMethod(method);
    b.addMyLang.setOnClickListener(
        v -> {
          openWebUrl(mA, getString(R.string.translation_link));
          dismiss();
        });
    return b.getRoot();
  }

  private Spanned createTransCreditsString() {
    TypedArray names = App.getRes().obtainTypedArray(R.array.locale_contributor_name_arrays);
    TypedArray links = App.getRes().obtainTypedArray(R.array.locale_contributor_link_arrays);
    String[] locales = App.getRes().getStringArray(R.array.locales);
    StringBuilder string = new StringBuilder();

    for (int i = 0; i < names.length(); i++) {
      int nameArrayResId = names.getResourceId(i, 0);
      int linkArrayResId = links.getResourceId(i, 0);
      if (nameArrayResId == 0 || linkArrayResId == 0) {
        continue;
      }

      if (string.length() != 0) {
        string.append("<br/>");
      }
      string.append("<b>").append(locales[i]).append("</b><ul>");

      String[] nameArray = App.getRes().getStringArray(nameArrayResId);
      String[] linkArray = App.getRes().getStringArray(linkArrayResId);
      for (int n = 0; n < nameArray.length; n++) {
        string.append("<li>").append(nameArray[n]).append(" ");
        String link = linkArray[n];
        if (link.startsWith("http://") || link.startsWith("https://")) {
          string.append("<a href=\"").append(link).append("\">LINK</a>");
        } else {
          string.append("(").append(link).append(")");
        }
        string.append("</li>");
      }
      string.append("</ul>");
    }

    names.recycle();
    links.recycle();

    return Utils.htmlToString(string.toString());
  }
}
