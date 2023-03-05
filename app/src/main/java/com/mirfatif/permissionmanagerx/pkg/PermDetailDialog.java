package com.mirfatif.permissionmanagerx.pkg;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

import android.view.Gravity;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.PermDetailsDialogBinding;
import com.mirfatif.permissionmanagerx.fwk.MyLinearLayout;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.parser.PkgParserFlavor;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.privtasks.Constants;
import com.mirfatif.privtasks.util.Util;
import java.util.ArrayList;
import java.util.List;

class PermDetailDialog {

  private final PackageActivity mA;

  PermDetailDialog(PackageActivity act) {
    mA = act;
  }

  private int mPreChecked, mChecked;
  private int[] mAppOpModes;

  void show(Permission perm, int yLocation) {
    PermDetailsDialogBinding b = PermDetailsDialogBinding.inflate(mA.mA.getLayoutInflater());

    CharSequence name = PkgParserFlavor.INS.getPermName(perm);
    b.permNameV.setText(name);
    b.permNameV.setSelected(true);

    if (!name.equals(perm.getName())) {
      b.permNameSubV.setText(perm.getName());
      b.permNameSubV.setSelected(true);
      b.permNameSubV.setVisibility(View.VISIBLE);
    }

    if (perm.isAppOp() && perm.hasDependsOnPerm()) {
      b.dependsPermNameV.setText(perm.getDependsOnName());
      b.dependsPermNameV.setSelected(true);
      b.dependsPermNameCont.setVisibility(View.VISIBLE);
    }

    b.protLevelV.setText(perm.getProtLevelString());

    new LiveTasksQueueTyped<>(mA.mA, () -> PkgParserFlavor.INS.getPermDesc(perm))
        .onUiWith(
            desc -> {
              if (desc != null) {
                b.permDescV.setText(desc);
                b.permDescV.setVisibility(View.VISIBLE);
              }
            })
        .start();

    CharSequence[] permList = null;

    if (perm.isChangeable()) {
      if (!perm.isAppOp()) {
        permList =
            new CharSequence[] {
              getString(R.string.perm_mode_granted), getString(R.string.perm_mode_revoked)
            };
        mPreChecked = perm.isGranted() ? 0 : 1;
      } else if (!perm.hasDependsOnPerm()) {
        boolean noFg =
            perm.getName().equals("RUN_IN_BACKGROUND")
                || perm.getName().equals("RUN_ANY_IN_BACKGROUND");

        List<String> modeNames = new ArrayList<>(AppOpsParser.INS.getAppOpsModes());

        List<String> localized = new ArrayList<>();
        List<Integer> modes = new ArrayList<>();

        for (int mode = 0; mode < modeNames.size(); mode++) {
          String modeName = modeNames.get(mode);
          if (noFg && modeName.equals(Constants.APP_OP_MODE_FG)) {
            continue;
          }

          localized.add(PermissionAdapter.getLocalizedMode(modeNames.get(mode)));
          modes.add(mode);
        }

        permList = localized.toArray(new CharSequence[0]);
        mAppOpModes = Util.getArray(modes);
        mPreChecked = modes.indexOf(perm.getAppOpMode());
      }
    }

    mChecked = mPreChecked;

    AlertDialog.Builder builder = new AlertDialog.Builder(mA.mA).setCustomTitle(b.getRoot());

    if (permList != null) {
      builder
          .setPositiveButton(R.string.ok_button, (d, w) -> setPerm(perm))
          .setNegativeButton(R.string.cancel_button, null)
          .setSingleChoiceItems(permList, mPreChecked, (d, which) -> mChecked = which);
    } else {
      MyLinearLayout v = b.getRoot();
      v.setPadding(v.getPaddingLeft(), v.getPaddingTop(), v.getPaddingRight(), UiUtils.dpToPx(20));
    }

    AlertDialog dialog = builder.create();

    Window dialogWindow = dialog.getWindow();
    if (dialogWindow != null) {
      WindowManager.LayoutParams layoutParams = dialogWindow.getAttributes();
      layoutParams.gravity = Gravity.TOP;
      layoutParams.y = yLocation;
    }

    AlertDialogFragment.show(mA.mA, dialog, "PERM_DETAILS");
  }

  private void setPerm(Permission perm) {
    if (mPreChecked == mChecked) {
      return;
    }
    if (!perm.isAppOp()) {
      mA.onManifestPermStateChanged(perm);
    } else {
      mA.onAppOpModeSelect(perm, mAppOpModes[mChecked]);
    }
  }
}
