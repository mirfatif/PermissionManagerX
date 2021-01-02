package com.mirfatif.permissionmanagerx.ui;

import android.os.Bundle;
import android.widget.TextView;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;

public class HelpActivity extends BaseActivity {

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_help);

    if (getSupportActionBar() != null) {
      getSupportActionBar().setTitle(R.string.help_menu_item);
    }

    handleUrl(R.id.faq1_ans);
    handleUrl(R.id.faq2_ans);
    handleUrl(R.id.faq10_ans);
    handleUrl(R.id.perm_help3);
  }

  private void handleUrl(int resId) {
    ((TextView) findViewById(resId))
        .setMovementMethod(
            BetterLinkMovementMethod.newInstance()
                .setOnLinkClickListener((tView, url) -> Utils.openWebUrl(this, url)));
  }
}
