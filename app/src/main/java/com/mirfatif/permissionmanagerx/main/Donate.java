package com.mirfatif.permissionmanagerx.main;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.view.View;
import android.widget.TextView;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.DonateAlertDialogBinding;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.util.Utils;
import me.saket.bettermovementmethod.BetterLinkMovementMethod;
import me.saket.bettermovementmethod.BetterLinkMovementMethod.OnLinkClickListener;

class Donate {

  private final MainActivity mA;
  private final DonateAlertDialogBinding mB;

  @SuppressLint("InflateParams")
  private Donate(MainActivity activity) {
    mA = activity;
    mB = DonateAlertDialogBinding.inflate(mA.getLayoutInflater());
  }

  static void showDialog(MainActivity activity) {
    new Donate(activity).show();
  }

  private void show() {
    setButtonClickListener(mB.bitcoinButton, mB.bitcoinContainer);
    setButtonClickListener(mB.bankAccountButton, mB.bankAccountLink);
    setButtonClickListener(mB.playStoreButton, mB.playStoreLink);
    setButtonClickListener(mB.redeemCodeButton, mB.redeemCodeLink);

    setMovementMethod(mB.bitcoinLink, (textView, url) -> handleBitcoinClick());
    setMovementMethod(mB.playStoreLink, (textView, url) -> Utils.openWebUrl(mA, url));
    sendMail(mB.bankAccountLink, R.string.bank_account_request);
    sendMail(mB.redeemCodeLink, R.string.redeem_code_request);

    new AlertDialogFragment(
            new Builder(mA).setTitle(R.string.donate_menu_item).setView(mB.getRoot()).create())
        .show(mA, "DONATION", true);
  }

  private void hideAll() {
    mB.bitcoinContainer.setVisibility(View.GONE);
    mB.bankAccountLink.setVisibility(View.GONE);
    mB.playStoreLink.setVisibility(View.GONE);
    mB.redeemCodeLink.setVisibility(View.GONE);
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
      Utils.showToast(R.string.no_bitcoin_app_installed);
    } else {
      mA.startActivity(intent);
    }
    return true;
  }
}
