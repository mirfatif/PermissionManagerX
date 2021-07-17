package com.mirfatif.permissionmanagerx.main;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.view.View;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.DonateAlertDialogBinding;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.util.Utils;

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
    setButtonClickListener(mB.bankAccountButton, mB.bankAccountButton2);
    setButtonClickListener(mB.playStoreButton, mB.playStoreButton2);

    mB.bitcoinButton2.setOnClickListener(v -> handleBitcoinClick());
    String psLink = Utils.getString(R.string.play_store_url);
    mB.playStoreButton2.setOnClickListener(v -> Utils.openWebUrl(mA, psLink));
    String text = Utils.getString(R.string.bank_account_request);
    mB.bankAccountButton2.setOnClickListener(v -> Utils.sendMail(mA, text));

    new AlertDialogFragment(
            new Builder(mA).setTitle(R.string.donate_menu_item).setView(mB.getRoot()).create())
        .show(mA, "DONATION", true);
  }

  private void hideAll() {
    mB.bitcoinContainer.setVisibility(View.GONE);
    mB.bankAccountButton2.setVisibility(View.GONE);
    mB.playStoreButton2.setVisibility(View.GONE);
  }

  private void setButtonClickListener(View button, View detailsView) {
    button.setOnClickListener(
        v -> {
          hideAll();
          detailsView.setVisibility(View.VISIBLE);
        });
  }

  private void handleBitcoinClick() {
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
  }
}
