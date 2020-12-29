package com.mirfatif.permissionmanagerx.main;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.Utils;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;
import me.saket.bettermovementmethod.BetterLinkMovementMethod.OnLinkClickListener;

class Donate {

  private final MainActivity mA;
  private final View layout;
  private final View bitcoinButton;
  private final View bitcoinContainer;
  private final TextView bitcoinLinkView;
  private final View bankButton;
  private final TextView bankLinkView;
  private final View psButton;
  private final TextView psLinkView;
  private final View redeemButton;
  private final TextView redeemLinkView;

  @SuppressLint("InflateParams")
  private Donate(MainActivity activity) {
    mA = activity;
    layout = mA.getLayoutInflater().inflate(R.layout.donate_alert_dialog, null);
    bitcoinButton = layout.findViewById(R.id.bitcoin_button);
    bitcoinContainer = layout.findViewById(R.id.bitcoin_container);
    bitcoinLinkView = layout.findViewById(R.id.bitcoin_link);
    bankButton = layout.findViewById(R.id.bank_account_button);
    bankLinkView = layout.findViewById(R.id.bank_account_link);
    psButton = layout.findViewById(R.id.play_store_button);
    psLinkView = layout.findViewById(R.id.play_store_link);
    redeemButton = layout.findViewById(R.id.redeem_code_button);
    redeemLinkView = layout.findViewById(R.id.redeem_code_link);
  }

  static void showDialog(MainActivity activity) {
    new Donate(activity).show();
  }

  private void show() {
    setButtonClickListener(bitcoinButton, bitcoinContainer);
    setButtonClickListener(bankButton, bankLinkView);
    setButtonClickListener(psButton, psLinkView);
    setButtonClickListener(redeemButton, redeemLinkView);

    setMovementMethod(bitcoinLinkView, (textView, url) -> handleBitcoinClick());
    setMovementMethod(psLinkView, (textView, url) -> Utils.openWebUrl(mA, url));
    sendMail(bankLinkView, R.string.bank_account_request);
    sendMail(redeemLinkView, R.string.redeem_code_request);

    new AlertDialogFragment(
            new Builder(mA).setTitle(R.string.donate_menu_item).setView(layout).create())
        .show(mA.getSupportFragmentManager(), "DONATION", true);
  }

  private void hideAll() {
    bitcoinContainer.setVisibility(View.GONE);
    bankLinkView.setVisibility(View.GONE);
    psLinkView.setVisibility(View.GONE);
    redeemLinkView.setVisibility(View.GONE);
  }

  private void setButtonClickListener(View button, View detailsView) {
    button.setOnClickListener(
        v -> {
          hideAll();
          detailsView.setVisibility(View.VISIBLE);
        });
  }

  private void setMovementMethod(TextView view, OnLinkClickListener listener) {
    view.setMovementMethod(BetterLinkMovementMethod.newInstance().setOnLinkClickListener(listener));
  }

  private void sendMail(TextView view, int msgResId) {
    setMovementMethod(view, (textView, url) -> Utils.sendMail(mA, Utils.getString(msgResId)));
  }

  private boolean handleBitcoinClick() {
    Intent intent = new Intent(Intent.ACTION_VIEW);
    intent.setData(Uri.parse("bitcoin:" + Utils.getString(R.string.bitcoin_address)));
    if (App.getContext()
        .getPackageManager()
        .queryIntentActivities(intent, PackageManager.MATCH_ALL)
        .isEmpty()) {
      Toast.makeText(App.getContext(), R.string.no_bitcoin_app_installed, Toast.LENGTH_LONG).show();
    } else {
      mA.startActivity(intent);
    }
    return true;
  }
}
