package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.util.Utils.getString;
import static com.mirfatif.permissionmanagerx.util.Utils.isProVersion;

import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.view.View;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.DonateAlertDialogBinding;
import com.mirfatif.permissionmanagerx.main.fwk.MainActivity;
import com.mirfatif.permissionmanagerx.util.Utils;

public class Donate {

  private final MainActivity mA;
  private final DonateAlertDialogBinding mB;

  public Donate(MainActivity activity) {
    mA = activity;
    mB = DonateAlertDialogBinding.inflate(mA.getLayoutInflater());

    setButtonClickListener(mB.bitcoinButton, mB.bitcoinContainer);
    setButtonClickListener(mB.bmcButton, mB.bmcButton2);
    setButtonClickListener(mB.bankAccountButton, mB.bankAccountContainer);
    setButtonClickListener(mB.proButton, mB.proButton2);
    setButtonClickListener(mB.playStoreButton, mB.playStoreButton2);

    mB.bitcoinButton2.setOnClickListener(v -> handleBitcoinClick());
    mB.bmcButton2.setOnClickListener(v -> Utils.openWebUrl(mA, getString(R.string.bmc_url)));
    String psLink = getString(R.string.play_store_url);
    mB.playStoreButton2.setOnClickListener(v -> Utils.openWebUrl(mA, psLink));
    String text = getString(R.string.bank_account_request);
    mB.bankAccountButton2.setOnClickListener(v -> Utils.sendMail(mA, text));
    mB.proButton2.setOnClickListener(
        v -> Utils.openWebUrl(mA, getString(R.string.purchase_pro_url)));

    if (isProVersion()) {
      mB.proButton.setVisibility(View.GONE);
      mB.playStoreButton.setVisibility(View.GONE);
    }
  }

  public AlertDialog createDialog() {
    return new Builder(mA).setTitle(R.string.donate_menu_item).setView(mB.getRoot()).create();
  }

  private void hideAll() {
    mB.bitcoinContainer.setVisibility(View.GONE);
    mB.bankAccountContainer.setVisibility(View.GONE);
    mB.bmcButton2.setVisibility(View.GONE);
    mB.proButton2.setVisibility(View.GONE);
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
    intent.setData(Uri.parse("bitcoin:" + getString(R.string.bitcoin_address)));
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
