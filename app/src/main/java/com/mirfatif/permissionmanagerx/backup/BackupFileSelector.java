package com.mirfatif.permissionmanagerx.backup;

import android.content.ActivityNotFoundException;
import android.net.Uri;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.fragment.app.FragmentActivity;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.privtasks.util.Util;

public class BackupFileSelector {

  public static final String BACKUP_FILE_MIME = "text/xml";

  private final ActivityResultLauncher<String> mBackupLauncher;
  private final ActivityResultLauncher<String[]> mRestoreLauncher;

  public BackupFileSelector(
      FragmentActivity act, boolean forBackup, ActivityResultCallback<Uri> callback) {
    if (forBackup) {
      mBackupLauncher =
          act.registerForActivityResult(
              new ActivityResultContracts.CreateDocument(BACKUP_FILE_MIME), callback);

      mRestoreLauncher = null;
    } else {
      mBackupLauncher = null;
      mRestoreLauncher =
          act.registerForActivityResult(new ActivityResultContracts.OpenDocument(), callback);
    }
  }

  public void launch() {
    UiUtils.showToast(R.string.select_backup_file);

    try {
      if (mBackupLauncher != null) {
        mBackupLauncher.launch("PermissionManagerX_" + Util.getCurrDateTime(false, false) + ".xml");
      } else {
        mRestoreLauncher.launch(new String[] {"text/xml"});
      }
    } catch (ActivityNotFoundException e) {
      UiUtils.showToast(R.string.no_file_picker_installed);
    }
  }
}
