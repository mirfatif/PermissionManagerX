package com.mirfatif.permissionmanagerx.pkg;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.databinding.PermLongPressDialogBinding;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.pkg.fwk.PackageActivity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.base.BottomSheetDialogFrag;
import com.mirfatif.permissionmanagerx.util.Utils;

public class PermLongPressDialogFrag extends BottomSheetDialogFrag {

  private final Permission mPerm;
  private final Package mPkg;
  private final PkgActivityFlavor mPkgActivityFlavor;

  PermLongPressDialogFrag(Permission perm, Package pkg, PkgActivityFlavor pkgActivityFlavor) {
    mPerm = perm;
    mPkg = pkg;
    mPkgActivityFlavor = pkgActivityFlavor;
  }

  public PermLongPressDialogFrag() {
    this(null, null, null);
  }

  private PackageActivity mA;

  @Override
  public void onAttach(@NonNull Context context) {
    super.onAttach(context);
    mA = (PackageActivity) getActivity();
  }

  @Nullable
  @Override
  public View onCreateView(
      @NonNull LayoutInflater inflater,
      @Nullable ViewGroup container,
      @Nullable Bundle savedInstanceState) {

    if (mPerm == null || mPkg == null) {
      return null;
    }

    PermLongPressDialogBinding b = PermLongPressDialogBinding.inflate(mA.getLayoutInflater());

    b.permNameV.setText(mPerm.getName());

    if (MySettings.INSTANCE.canBeExcluded(mPerm)) {
      b.excludePerm.setOnClickListener(
          v -> {
            dismissAllowingStateLoss();
            Utils.runInBg(this::excludePerm);
          });
    } else {
      b.excludePerm.setEnabled(false);
    }

    if (mPerm.isChangeable()) {
      boolean isReferenced = mPerm.isReferenced() != null && mPerm.isReferenced();
      String permState = mA.getPermState(mPerm);

      if (isReferenced) {
        b.refButton.setText(R.string.clear_reference);
      } else {
        String permStateStr;
        if (mPerm.isAppOps()) {
          permStateStr = PermissionAdapter.getLocalizedMode(permState);
          if (permStateStr == null) {
            permStateStr = permState;
          }
        } else if (permState.equals(Permission.GRANTED)) {
          permStateStr = getString(R.string.perm_mode_granted);
        } else {
          permStateStr = getString(R.string.perm_mode_revoked);
        }
        b.refButton.setText(
            Utils.htmlToString(getString(R.string.set_perm_state_reference, permStateStr)));
      }

      b.refButton.setOnClickListener(
          v -> {
            dismissAllowingStateLoss();
            Utils.runInBg(() -> setClearRef(isReferenced, permState));
          });
    } else {
      b.refButton.setText(R.string.set_reference);
      b.refButton.setEnabled(false);
    }

    return b.getRoot();
  }

  @NonNull
  @Override
  public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
    if (mPerm == null) {
      dismissAllowingStateLoss();
    }
    return super.onCreateDialog(savedInstanceState);
  }

  private void excludePerm() {
    MySettings.INSTANCE.addPermToExcludedPerms(mPerm.getName());
    mA.updatePackage();

    // Other packages are also affected
    PackageParser.INSTANCE.updatePackagesList();
  }

  private void setClearRef(boolean isReferenced, String permState) {
    if (isReferenced) {
      MySettings.INSTANCE.getPermDb().deletePermission(mPkg.getName(), mPerm.getName());
      PackageParser.INSTANCE.updatePermReferences(mPkg.getName(), mPerm.getName(), null);
      mA.updatePackage();
      mPkgActivityFlavor.pkgRefChanged(mPkg);
      return;
    }

    PermissionEntity entity = new PermissionEntity();
    entity.isAppOps = mPerm.isAppOps();
    entity.permName = mPerm.getName();
    entity.pkgName = mPkg.getName();
    entity.state = permState;

    int id = MySettings.INSTANCE.getPermDb().getId(entity.pkgName, entity.permName);
    if (id > 0) {
      entity.id = id;
    }
    MySettings.INSTANCE.getPermDb().insertAll(entity);
    PackageParser.INSTANCE.updatePermReferences(mPkg.getName(), mPerm.getName(), permState);
    mA.updatePackage();
    mPkgActivityFlavor.pkgRefChanged(mPkg);
  }

  public static void show(
      Permission perm, Package pkg, PkgActivityFlavor pkgActivityFlavor, FragmentManager fm) {
    if (!MySettings.INSTANCE.canBeExcluded(perm) && !perm.isChangeable()) {
      Utils.showToast(R.string.no_action_available);
    } else {
      new PermLongPressDialogFrag(perm, pkg, pkgActivityFlavor).show(fm, "PERM_OPTIONS");
    }
  }
}
