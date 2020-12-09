package com.mirfatif.permissionmanagerx;

import android.os.Bundle;
import android.view.MenuItem;
import android.widget.TextView;
import androidx.appcompat.app.AppCompatActivity;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;

public class HelpActivity extends AppCompatActivity {

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

  void handleUrl(int resId) {
    ((TextView) findViewById(resId))
        .setMovementMethod(
            BetterLinkMovementMethod.newInstance()
                .setOnLinkClickListener((tView, url) -> Utils.openWebUrl(this, url)));
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    // do not recreate parent (Main) activity
    if (item.getItemId() == android.R.id.home) {
      onBackPressed();
      return true;
    }
    return super.onOptionsItemSelected(item);
  }
}
