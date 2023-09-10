package com.mirfatif.permissionmanagerx.prefs.settings;

import android.net.Uri;
import android.os.Bundle;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AlertDialog;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.backup.BackupFileSelector;
import com.mirfatif.permissionmanagerx.backup.BackupRestore;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.ProgressDialogBinding;
import com.mirfatif.permissionmanagerx.fwk.AdvSettingsActivityM;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.parser.PkgParserFlavor;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionDao;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermsDb;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.StringUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.UserUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import java.util.ArrayList;
import java.util.List;

public class AdvSettingsActivity {

  private final AdvSettingsActivityM mA;

  public AdvSettingsActivity(AdvSettingsActivityM activity) {
    mA = activity;
  }

  private BackupFileSelector mCleanupLauncher, mResetLauncher;

  public void onCreate(Bundle savedInstanceState) {
    mA.setContentView(R.layout.activity_fragment_container);

    ActionBar actionBar = mA.getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(R.string.advanced_settings_menu_item);
    }

    if (savedInstanceState == null) {
      mA.getSupportFragmentManager()
          .beginTransaction()
          .replace(R.id.fragment_container, new AdvSettingsFrag())
          .commit();
    }

    mCleanupLauncher = new BackupFileSelector(mA, true, uri -> startPermDbReset(true, uri));
    mResetLauncher = new BackupFileSelector(mA, true, uri -> startPermDbReset(false, uri));
  }

  void showPermDbResetDialog() {
    AlertDialog.Builder builder =
        new AlertDialog.Builder(mA)
            .setTitle(R.string.reset_perm_db_dialog_title)
            .setMessage(StringUtils.htmlToString(R.string.reset_perm_db_dialog_detail))
            .setPositiveButton(
                R.string.reset_perm_db_dialog_cleanup_button, (d, w) -> mCleanupLauncher.launch())
            .setNegativeButton(
                R.string.reset_perm_db_dialog_reset_button, (d, w) -> mResetLauncher.launch());

    AlertDialogFragment.show(mA, builder.create(), "RESET_PERM_DB");
  }

  private void startPermDbReset(boolean cleanup, Uri uri) {
    if (uri == null) {
      return;
    }

    ProgressDialogBinding b = ProgressDialogBinding.inflate(mA.getLayoutInflater());
    b.progText.setText(R.string.backup_in_progress);

    AlertDialog dialog =
        new AlertDialog.Builder(mA)
            .setTitle(R.string.reset_perm_db_dialog_title)
            .setPositiveButton(android.R.string.cancel, null)
            .setView(b.getRoot())
            .create();

    AlertDialogFragment frag = AlertDialogFragment.show(mA, dialog, "RESET_PERM_DB");

    new LiveTasksQueueTyped<>(frag, () -> BackupRestore.INS.backupNoThrow(uri, false, false, null))
        .onUiWith(result -> handleBackupResult(result, b, frag, cleanup))
        .start();
  }

  private void handleBackupResult(
      BackupRestore.Result result,
      ProgressDialogBinding b,
      AlertDialogFragment frag,
      boolean cleanup) {
    if (result == null) {
      UiUtils.showToast(R.string.reset_perm_db_backup_failed_toast);
      return;
    }

    if (!cleanup) {
      b.progText.setText(R.string.reset_perm_db_optimizing_perm_db);

      new LiveTasksQueueTyped<>(
              frag,
              () -> {
                int count = PermsDb.INS.getDb().getAll().size();
                PermsDb.INS.getDb().deleteAll();
                PermsDb.INS.buildRefs();
                return count;
              })
          .onUiWith(removed -> onResetCompleted(frag, removed))
          .start();
    } else {
      b.progText.setText(R.string.reset_perm_db_building_app_list);

      new LiveTasksQueueTyped<>(frag, PkgParserFlavor.INS::getAllUsersPkgList)
          .onUiWith(pkgList -> handlePkgList(pkgList, b, frag))
          .start();
    }
  }

  private void handlePkgList(
      List<Package> pkgList, ProgressDialogBinding b, AlertDialogFragment frag) {
    if (pkgList == null) {
      UiUtils.showToast(R.string.reset_perm_db_pkg_list_failed_toast);
      return;
    }

    b.progText.setText(R.string.reset_perm_db_optimizing_perm_db);

    new LiveTasksQueueTyped<>(frag, () -> doPermDbCleanup(pkgList))
        .onUiWith(removed -> onResetCompleted(frag, removed))
        .start();
  }

  public static int doPermDbCleanup(List<Package> pkgList) {
    List<String> perms = new ArrayList<>();

    for (Package pkg : pkgList) {
      for (Permission perm : pkg.getFullPermsList()) {
        perms.add(
            PermsDb.createKey(
                    pkg.getName(),
                    perm.getName(),
                    perm.isAppOp(),
                    perm.isPerUid(),
                    UserUtils.getUserId(pkg.getUid()))
                + "_"
                + perm.createRefStringForDb());
      }
    }

    List<Integer> ids = new ArrayList<>();

    for (PermissionEntity entity : PermsDb.INS.getDb().getAll()) {
      if (!perms.contains(
          PermsDb.createKey(
                  entity.pkgName, entity.permName, entity.isAppOps, entity.isPerUid, entity.userId)
              + "_"
              + entity.state)) {
        ids.add(entity.id);
      }
    }

    PermissionDao.deletePerms(PermsDb.INS.getDb(), ids);

    PermsDb.INS.buildRefs();

    return ids.size();
  }

  private void onResetCompleted(AlertDialogFragment frag, int removed) {
    frag.dismissAllowingStateLoss();
    UiUtils.showToast(
        ApiUtils.getQtyString(R.plurals.reset_perm_db_removed_count_toast, removed, removed));
    PackageParser.INS.updatePkgList();
  }
}
