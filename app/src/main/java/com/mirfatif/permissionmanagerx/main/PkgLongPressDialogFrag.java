package com.mirfatif.permissionmanagerx.main;

import android.app.Dialog;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.databinding.PkgLongPressDialogBinding;
import com.mirfatif.permissionmanagerx.fwk.MainActivityM;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.privs.DaemonStarter;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.StringUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.UserUtils;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.util.bg.BgRunner;

public class PkgLongPressDialogFrag extends BottomSheetDialogFrag {

  private final Package mPkg;

  public PkgLongPressDialogFrag(Package pkg) {
    mPkg = pkg;
  }

  public PkgLongPressDialogFrag() {
    mPkg = null;
  }

  public View onCreateView(
      LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {

    if (mPkg == null) {
      return null;
    }

    PkgLongPressDialogBinding b = PkgLongPressDialogBinding.inflate(mA.getLayoutInflater());

    b.pkgLabelV.setText(mPkg.getLabel());
    b.pkgLabelV.setSelected(true);
    if (!mPkg.getLabel().equals(mPkg.getName())) {
      b.pkgNameV.setText(mPkg.getName());
      b.pkgNameV.setVisibility(View.VISIBLE);
      b.pkgNameV.setSelected(true);
    }

    if (ExcFiltersData.INS.canBeExcluded(mPkg)) {
      b.excludePkg.setOnClickListener(
          v -> {
            dismissAllowingStateLoss();
            BgRunner.execute(
                () -> {
                  MySettings.INS.addPkgToExcludedApps(mPkg.getName());
                  PackageParser.INS.removePackage(mPkg);
                });
          });
    } else {
      b.excludePkg.setEnabled(false);
    }

    b.disablePkg.setText(mPkg.isEnabled() ? R.string.disable_app : R.string.enable_app);
    if (mPkg.isChangeable() && !mPkg.getName().equals(App.getCxt().getPackageName())) {
      b.disablePkg.setOnClickListener(
          v -> {
            dismissAllowingStateLoss();
            setPackageEnabledState();
          });
    } else {
      b.disablePkg.setEnabled(false);
    }

    b.openPkgInfo.setOnClickListener(
        v -> {
          dismissAllowingStateLoss();
          openAppInfo();
        });

    if (!Utils.isFdroidVersion()
        || ApiUtils.resolveActivity(
                new Intent(WRUN_ACTION_SEARCH_PKG), PackageManager.MATCH_DEFAULT_ONLY)
            != null) {
      b.findPkgProc.setVisibility(View.VISIBLE);
      b.findPkgProc.setOnClickListener(
          v -> {
            dismissAllowingStateLoss();
            openAppProc();
          });
    }

    return b.getRoot();
  }

  public Dialog onCreateDialog(Bundle savedInstanceState) {
    if (mPkg == null) {

      dismissAllowingStateLoss();
    }
    return super.onCreateDialog(savedInstanceState);
  }

  void setPackageEnabledState() {
    if (mPkg == null) {
      return;
    }
    if (!DaemonHandler.INS.isDaemonAlive()) {
      requireDaemon();
      return;
    }

    boolean enabled = mPkg.isEnabled();

    String warn = null;
    if (enabled && MySettings.INS.warnDangerousPkgChanges()) {
      if (mPkg.isFrameworkApp()) {
        warn =
            ApiUtils.getString(
                R.string.disable_pkg_warning, ApiUtils.getString(R.string.framework));
      } else if (mPkg.isSystemApp()) {
        warn =
            ApiUtils.getString(R.string.disable_pkg_warning, ApiUtils.getString(R.string.system));
      }
    }

    if (warn == null) {
      BgRunner.execute(() -> setPackageEnabledState(mPkg, enabled));
      return;
    }

    AlertDialog dialog =
        new Builder(mA)
            .setPositiveButton(
                R.string.yes,
                (d, which) -> BgRunner.execute(() -> setPackageEnabledState(mPkg, true)))
            .setNegativeButton(R.string.no, null)
            .setNeutralButton(
                R.string.do_not_remind,
                (d, which) -> {
                  MySettings.INS.disableWarnDangerousPkgChanges();
                  BgRunner.execute(() -> setPackageEnabledState(mPkg, true));
                })
            .setTitle(R.string.warning)
            .setMessage(StringUtils.breakParas(warn))
            .create();

    AlertDialogFragment.show(mA, dialog, "PKG_DISABLE_WARNING");
  }

  private void setPackageEnabledState(Package pkg, boolean enabled) {
    DaemonIface.INS.setPkgState(!enabled, pkg.getName(), UserUtils.getUserId(pkg.getUid()));
    PackageParser.INS.updatePackage(pkg, true);
  }

  private void openAppInfo() {
    if (mPkg == null) {
      return;
    }
    int pkgUserId = UserUtils.getUserId(mPkg.getUid());
    if (UserUtils.getUserId() == pkgUserId) {
      startActivity(
          new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
              .setData(Uri.parse("package:" + mPkg.getName())));
    } else if (DaemonHandler.INS.isDaemonAlive()) {
      BgRunner.execute(() -> DaemonIface.INS.openAppInfo(mPkg.getName(), pkgUserId));
    } else {
      requireDaemon();
    }
  }

  private static final String WRUN_ACTION_SEARCH_PKG = "com.mirfatif.wrun.action.SEARCH_PKG";
  private static final String WRUN_EXTRA_PKG_NAME = "com.mirfatif.wrun.extra.PKG_NAME";
  private static final String WRUN_EXTRA_PKG_UID = "com.mirfatif.wrun.extra.PKG_UID";

  private void openAppProc() {
    if (mPkg == null) {
      return;
    }
    Intent intent = new Intent(WRUN_ACTION_SEARCH_PKG);
    intent
        .putExtra(WRUN_EXTRA_PKG_NAME, mPkg.getName())
        .putExtra(WRUN_EXTRA_PKG_UID, mPkg.getUid());
    try {
      startActivity(intent);
    } catch (ActivityNotFoundException ignored) {
      Snackbar sb =
          ((MainActivityM) mA)
              .mA.createSnackBar(ApiUtils.getString(R.string.wrun_not_installed), 10);
      sb.setTextColor(mA.getColor(R.color.sharpText));
      sb.getView().setBackgroundColor(UiUtils.getSharpBgColor(mA));
      sb.setAction(
          R.string.install, v -> ApiUtils.openWebUrl(mA, ApiUtils.getString(R.string.wrun_url)));
      sb.show();
    }
  }

  private void requireDaemon() {
    DaemonStarter.INS.startPrivDaemon(false, false, true, true);
  }
}
