package com.mirfatif.permissionmanagerx.main;

import android.annotation.SuppressLint;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.View;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Toast;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.appcompat.widget.AppCompatSpinner;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.AlertDialogFragment;
import java.util.Arrays;
import java.util.List;

class AdvancedSettings {

  private final MainActivity mA;
  private final MySettings mMySettings = MySettings.getInstance();

  private final View dialogLayout;
  private final CheckBox useHiddenAPIsView;
  private final EditText adbPortView;
  private final CheckBox useSocketView;
  private final AppCompatSpinner daemonUidSpinner;
  private final AppCompatSpinner daemonContextSpinner;
  private final View daemonUidListArrow;
  private final View daemonContextListArrow;

  private final List<String> spinnerUids, spinnerContexts;
  private final int uidSelectedPos, contextSelectedPos;

  private final boolean useHiddenAPIs = mMySettings.useHiddenAPIs();
  private final String adbPort = String.valueOf(mMySettings.getAdbPort());
  private final boolean useSocket = mMySettings.useSocket();

  @SuppressLint("InflateParams")
  private AdvancedSettings(MainActivity activity) {
    mA = activity;

    dialogLayout = mA.getLayoutInflater().inflate(R.layout.advanced_settings_alert_dialog, null);

    useHiddenAPIsView = dialogLayout.findViewById(R.id.use_hidden_apis);
    adbPortView = dialogLayout.findViewById(R.id.adb_port);
    useSocketView = dialogLayout.findViewById(R.id.use_socket);
    daemonUidSpinner = dialogLayout.findViewById(R.id.daemon_uid_list);
    daemonContextSpinner = dialogLayout.findViewById(R.id.daemon_context_list);

    daemonUidListArrow = dialogLayout.findViewById(R.id.daemon_uid_list_arrow);
    daemonContextListArrow = dialogLayout.findViewById(R.id.daemon_context_list_arrow);

    spinnerUids = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_uids));
    spinnerContexts = Arrays.asList(mA.getResources().getStringArray(R.array.daemon_contexts));

    int uidResId = R.string.daemon_uid_system;
    if (mMySettings.getDaemonUid() == 0) {
      uidResId = R.string.daemon_uid_root;
    } else if (mMySettings.getDaemonUid() == 2000) {
      uidResId = R.string.daemon_uid_adb;
    }
    uidSelectedPos = spinnerUids.indexOf(getString(uidResId));

    int contextResId = R.string.daemon_context_shell;
    if (mMySettings.getDaemonContext().equals(MySettings.CONTEXT_DEFAULT)) {
      contextResId = R.string.daemon_context_default;
    }
    contextSelectedPos = spinnerContexts.indexOf(getString(contextResId));
  }

  private void show() {
    daemonUidListArrow.setOnClickListener(v -> daemonUidSpinner.performClick());
    daemonContextListArrow.setOnClickListener(v -> daemonContextSpinner.performClick());
    useHiddenAPIsView.setChecked(useHiddenAPIs);
    adbPortView.setText(adbPort);
    adbPortView.addTextChangedListener(new PortNumberWatcher());
    useSocketView.setChecked(useSocket);
    daemonUidSpinner.setSelection(uidSelectedPos);
    daemonContextSpinner.setSelection(contextSelectedPos);

    AlertDialog dialog =
        new Builder(mA)
            .setTitle(R.string.advanced_settings_menu_item)
            .setView(dialogLayout)
            .setPositiveButton(R.string.save, (d, which) -> saveSettings())
            .setNegativeButton(android.R.string.cancel, null)
            .create();
    new AlertDialogFragment(dialog)
        .show(mA.getSupportFragmentManager(), "ADVANCED_SETTINGS", false);
  }

  private void saveSettings() {
    boolean restartDaemon = false;
    if (useHiddenAPIs != useHiddenAPIsView.isChecked()) {
      restartDaemon = saveHiddenAPIsSettings(useHiddenAPIsView.isChecked());
    }

    String adbPortNew = adbPortView.getText().toString().trim();
    if (!TextUtils.isEmpty(adbPortNew) && !adbPort.equals(adbPortNew)) {
      int port = Integer.parseInt(adbPortNew);
      if (port > 65535 || port <= 0) {
        Toast.makeText(App.getContext(), R.string.bad_port_number, Toast.LENGTH_LONG).show();
      } else {
        mMySettings.setAdbPort(port);
        restartDaemon = true;
      }
    }

    if (useSocket != useSocketView.isChecked()) {
      mMySettings.setUseSocket(useSocketView.isChecked());
      restartDaemon = true;
    }

    int uidSelectedPosNew = daemonUidSpinner.getSelectedItemPosition();
    if (uidSelectedPos != uidSelectedPosNew) {
      String newSelection = spinnerUids.get(uidSelectedPosNew);
      int uid = 1000;
      if (newSelection.equals(getString(R.string.daemon_uid_root))) {
        uid = 0;
      } else if (newSelection.equals(getString(R.string.daemon_uid_adb))) {
        uid = 2000;
      }
      mMySettings.setDaemonUid(uid);
      restartDaemon = true;
    }

    int contextSelectedPosNew = daemonContextSpinner.getSelectedItemPosition();
    if (contextSelectedPos != contextSelectedPosNew) {
      String newSelection = spinnerContexts.get(contextSelectedPosNew);
      String context = MySettings.CONTEXT_SHELL;
      if (newSelection.equals(getString(R.string.daemon_context_default))) {
        context = MySettings.CONTEXT_DEFAULT;
      }
      mMySettings.setDaemonContext(context);
      restartDaemon = true;
    }

    if (restartDaemon) {
      mA.restartPrivDaemon();
    }
  }

  private boolean saveHiddenAPIsSettings(boolean useHiddenAPIs) {
    if (useHiddenAPIs) {
      mMySettings.setUseHiddenAPIs(true);

      // make sure read AppOps permission is granted
      return true;
    }

    AlertDialog dialog =
        new Builder(mA)
            .setPositiveButton(
                R.string.yes,
                (d, which) -> {
                  mMySettings.setUseHiddenAPIs(false);
                  mA.restartPrivDaemon(); // Start daemon if not running
                })
            .setNegativeButton(R.string.no, null)
            .setTitle(R.string.hidden_apis)
            .setMessage(R.string.hidden_apis_confirmation)
            .create();
    new AlertDialogFragment(dialog)
        .show(mA.getSupportFragmentManager(), "HIDDEN_APIS_CONFIRM", false);
    return false;
  }

  private String getString(int resId) {
    return App.getContext().getString(resId);
  }

  private class PortNumberWatcher implements TextWatcher {
    @Override
    public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

    @Override
    public void onTextChanged(CharSequence s, int start, int before, int count) {
      if (TextUtils.isEmpty(s)) {
        return;
      }
      int port = Integer.parseInt(s.toString().trim());
      if (port > 65535 || port <= 0) {
        adbPortView.setError(getString(R.string.bad_port_number));
      }
    }

    @Override
    public void afterTextChanged(Editable s) {}
  }

  static void showDialog(MainActivity mainActivity) {
    new AdvancedSettings(mainActivity).show();
  }
}
