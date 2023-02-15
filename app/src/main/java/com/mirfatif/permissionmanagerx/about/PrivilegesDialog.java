package com.mirfatif.permissionmanagerx.about;

import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.RectShape;
import androidx.appcompat.app.AlertDialog;
import androidx.fragment.app.FragmentActivity;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.AboutPrivilegesDialogBinding;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.privs.DaemonHandler;
import com.mirfatif.permissionmanagerx.privs.DaemonIface;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.privtasks.bind.PrivsStatus;

public class PrivilegesDialog {

  private final FragmentActivity mA;

  PrivilegesDialog(FragmentActivity activity) {
    mA = activity;
  }

  AlertDialog create(AlertDialogFragment dialogFragment) {
    AboutPrivilegesDialogBinding b = AboutPrivilegesDialogBinding.inflate(mA.getLayoutInflater());
    PrivilegesAdapter adapter = new PrivilegesAdapter();

    new LiveTasksQueueTyped<>(dialogFragment, DaemonIface.INS::getPrivsStatus)
        .onUiWith(result -> updatePermStatusDialog(result, b, adapter, dialogFragment))
        .start();

    b.uidV.setText(String.valueOf(DaemonHandler.INS.getUid()));
    b.recyclerV.setLayoutManager(new LinearLayoutManager(mA));
    DividerItemDecoration divider = new DividerItemDecoration(mA, DividerItemDecoration.VERTICAL);
    divider.setDrawable(new Divider(mA.getColor(R.color.colorControlNormal)));
    b.recyclerV.addItemDecoration(divider);
    b.recyclerV.setAdapter(adapter);
    return new AlertDialog.Builder(mA)
        .setTitle(R.string.perm_status_menu_item)
        .setView(b.getRoot())
        .create();
  }

  private void updatePermStatusDialog(
      PrivsStatus status,
      AboutPrivilegesDialogBinding b,
      PrivilegesAdapter adapter,
      AlertDialogFragment dialogFragment) {
    if (status == null) {
      dialogFragment.dismissAllowingStateLoss();
      return;
    }

    adapter.submitList(status.permStatusList);
    b.opToDefModeV.setImageResource(getIcon(status.opToDefModeWorks));
    b.opToSwV.setImageResource(getIcon(status.opToSwitchWorks));
    b.opToNameV.setImageResource(getIcon(status.opToNameWorks));
    b.getOpsV.setImageResource(getIcon(status.getOpsWorks));
    b.consAppOpNumV.setImageResource(getIcon(status.opNumConsistent));
    b.consAppOpModeV.setImageResource(
        getIcon(PackageParser.INS.mOpModesConsistent && status.opModeConsistent));
  }

  private int getIcon(boolean ok) {
    return ok ? R.drawable.tick : R.drawable.cross_red;
  }

  private static class Divider extends ShapeDrawable {

    public Divider(int color) {
      super(new RectShape());
      super.setAlpha(100);
      super.getPaint().setColor(color);
      super.setIntrinsicHeight(UiUtils.dpToPx(1.2f));
    }
  }
}
