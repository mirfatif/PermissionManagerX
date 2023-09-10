package com.mirfatif.permissionmanagerx.pkg;

import android.view.Gravity;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.PermDetailsDialogBinding;
import com.mirfatif.permissionmanagerx.fwk.MyLinearLayout;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.parser.PkgParserFlavor;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

class PermDetailDialog {

  private final PackageActivity mA;

  PermDetailDialog(PackageActivity act) {
    mA = act;
  }

  private int mPreCheckedIndex, mCheckedIndex;
  private final List<Integer> mAppOpModes = new ArrayList<>();

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

    b.protLevelV.setText(perm.getLocalizedProtLevelString());

    new LiveTasksQueueTyped<>(mA.mA, () -> PkgParserFlavor.INS.getPermDesc(perm))
        .onUiWith(
            desc -> {
              if (desc != null) {
                b.permDescV.setText(desc);
                b.permDescV.setVisibility(View.VISIBLE);
              }
            })
        .start();

    List<CharSequence> permList = new ArrayList<>();
    AtomicInteger preCheckedIndex = new AtomicInteger();

    if (perm.isChangeable()) {
      if (perm.isAppOp()) {
        Permission.getLocalizedAppOpModeNames(
            permList, mAppOpModes, preCheckedIndex, perm.getAppOpMode(), perm.getName());
      } else {
        Permission.getLocalizedPermStateNames(
            permList, new ArrayList<>(), preCheckedIndex, perm.isGranted());
      }
    }

    mCheckedIndex = mPreCheckedIndex = preCheckedIndex.get();

    AlertDialog.Builder builder = new AlertDialog.Builder(mA.mA).setCustomTitle(b.getRoot());

    if (!permList.isEmpty()) {
      builder
          .setPositiveButton(R.string.ok_button, (d, w) -> setPerm(perm))
          .setNegativeButton(R.string.cancel_button, null)
          .setSingleChoiceItems(
              permList.toArray(new CharSequence[0]),
              mPreCheckedIndex,
              (d, which) -> mCheckedIndex = which);
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
    if (mPreCheckedIndex == mCheckedIndex) {
      return;
    }
    if (!perm.isAppOp()) {
      mA.onManifestPermStateChanged(perm);
    } else {
      mA.onAppOpModeSelect(perm, mAppOpModes.get(mCheckedIndex));
    }
  }
}
