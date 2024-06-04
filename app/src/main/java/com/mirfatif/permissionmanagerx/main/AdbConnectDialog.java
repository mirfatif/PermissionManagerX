package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.main.MainActivity.TAG_ADB_CONNECTION;
import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;
import static java.lang.System.currentTimeMillis;

import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.os.SystemClock;
import android.system.Os;
import android.system.OsConstants;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.View;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.MutableLiveData;
import com.google.android.material.textfield.TextInputLayout;
import com.mirfatif.err.AdbException;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.AlertDialogFragment;
import com.mirfatif.permissionmanagerx.databinding.AdbConnectDialogBinding;
import com.mirfatif.permissionmanagerx.databinding.DilogTitleWithHelpBinding;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.permissionmanagerx.help.HelpActivity;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.AdbConnManager;
import com.mirfatif.permissionmanagerx.privs.DaemonStarter;
import com.mirfatif.permissionmanagerx.privs.NativeDaemon;
import com.mirfatif.permissionmanagerx.util.StringUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueueTyped;
import com.mirfatif.privtasks.util.MyLog;
import com.mirfatif.privtasks.util.NonBlockingReader;
import io.github.muntashirakon.adb.AdbPairingRequiredException;
import io.github.muntashirakon.adb.AdbStream;
import io.github.muntashirakon.adb.android.AdbMdns;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import org.lsposed.hiddenapibypass.HiddenApiBypass;

public class AdbConnectDialog {

  private static final String TAG = "AdbConnectDialog";

  private final MainActivity mA;
  private final AdbConnectDialogBinding mB;
  private final AlertDialogFragment mDialogFrag;

  public AdbConnectDialog(MainActivity activity, AlertDialogFragment dialogFragment) {
    mA = activity;
    mB = AdbConnectDialogBinding.inflate(mA.mA.getLayoutInflater());
    mB.connectIpV.setText(MySettings.INS.getAdbHost());
    mB.connectPortV.setText(String.valueOf(MySettings.INS.getAdbPort()));

    mDialogFrag = dialogFragment;
    LifecycleWatcher.addOnDestroyed(dialogFragment, this::stopMdnsPairingListener);

    connectToAdb(true);
  }

  private void onFirstTryFailed() {
    if (VERSION.SDK_INT >= VERSION_CODES.R) {
      mB.pairHelpV.setText(StringUtils.htmlToString(R.string.adb_pair_help_msg));

      mB.pairingIpV.setText(MySettings.INS.getAdbHost());
      mB.pairButton.setOnClickListener(v -> doPairing());

      mB.pairingCodeV.addTextChangedListener(new EditTextWatcher(mB.pairingCodeCont));
      mB.pairingIpV.addTextChangedListener(new EditTextWatcher(mB.pairingIpCont));
      mB.pairingPortV.addTextChangedListener(new EditTextWatcher(mB.pairingPortCont));

      mB.pairHeaderCont.setVisibility(View.VISIBLE);
      mB.divider.setVisibility(View.VISIBLE);
      mB.connectHeaderCont.setVisibility(View.VISIBLE);
      mB.pairArrow.setOnClickListener(v -> onArrowClicked(v, false));
      mB.connectArrow.setOnClickListener(v -> onArrowClicked(v, false));

      setAutoPairingHostPort();
    }

    mB.connectHelpV.setVisibility(View.VISIBLE);
    setConnectHelpText(false);

    mB.connectButton.setOnClickListener(v -> connectToAdb(false));

    mB.connectIpV.addTextChangedListener(new EditTextWatcher(mB.connectIpCont));
    mB.connectPortV.addTextChangedListener(new EditTextWatcher(mB.connectPortCont));
  }

  private final MutableLiveData<HostPort> mMdnsPairingEvent = new MutableLiveData<>();

  private static class HostPort {

    public final String host;
    public final int port;

    public HostPort(String host, int port) {
      this.host = host;
      this.port = port;
    }
  }

  private AdbMdns mMdnsPairingListener;

  private void setAutoPairingHostPort() {
    synchronized (mMdnsPairingEvent) {
      mMdnsPairingListener =
          new AdbMdns(
              App.getCxt(),
              AdbMdns.SERVICE_TYPE_TLS_PAIRING,
              (hostAddress, port) -> {
                String host = hostAddress == null ? null : hostAddress.getHostAddress();
                if (host != null && port > 0) {
                  mMdnsPairingEvent.postValue(new HostPort(host, port));
                }
              });
      mMdnsPairingListener.start();
    }

    mMdnsPairingEvent.observe(
        mDialogFrag,
        daemon -> {
          mB.pairingIpV.setText(daemon.host);
          mB.pairingPortV.setText(String.valueOf(daemon.port));
        });
  }

  private void stopMdnsPairingListener() {
    synchronized (mMdnsPairingEvent) {
      if (mMdnsPairingListener != null) {
        mMdnsPairingListener.stop();
        mMdnsPairingListener = null;
      }
    }
  }

  private void setConnectHelpText(boolean afterPairing) {
    int resId;
    if (VERSION.SDK_INT <= VERSION_CODES.Q) {
      resId = R.string.adb_connect_help_msg;
    } else if (afterPairing) {
      resId = R.string.adb_connect_help_after_pair_msg;
    } else {
      resId = R.string.adb_connect_help_11_msg;
    }
    mB.connectHelpV.setText(StringUtils.htmlToString(resId));
  }

  private void onArrowClicked(View arrow, boolean hideOnly) {
    View hideArrow = null, hideCont = null, showArrow, showCont;

    if (arrow == mB.pairArrow) {
      if (mB.pairCont.getVisibility() == View.GONE) {
        if (hideOnly) {
          return;
        }
        showArrow = arrow;
        showCont = mB.pairCont;
      } else {
        hideArrow = arrow;
        hideCont = mB.pairCont;
        showArrow = mB.connectArrow;
        showCont = mB.connectCont;
      }
    } else {
      if (mB.connectCont.getVisibility() == View.GONE) {
        if (hideOnly) {
          return;
        }
        showArrow = arrow;
        showCont = mB.connectCont;
      } else {
        hideArrow = arrow;
        hideCont = mB.connectCont;
        showArrow = mB.pairArrow;
        showCont = mB.pairCont;
      }
    }

    if (hideArrow != null) {
      hideArrow.setRotation(0);
      hideCont.setVisibility(View.GONE);
    }

    showArrow.setRotation(90);
    showCont.setVisibility(View.VISIBLE);
  }

  private long mShowTs;

  public AlertDialog createDialog() {
    int helpHrefRes;
    if (VERSION.SDK_INT <= VERSION_CODES.Q) {
      helpHrefRes = R.string.adb_connect_help_href_10;
    } else {
      helpHrefRes = R.string.adb_connect_help_href_11;
    }

    DilogTitleWithHelpBinding b = DilogTitleWithHelpBinding.inflate(mA.mA.getLayoutInflater());
    b.titleV.setText(R.string.adb_title);
    b.helpV.setOnClickListener(v -> HelpActivity.start(mA.mA, getString(helpHrefRes)));

    AlertDialog d = new Builder(mA.mA).setCustomTitle(b.getRoot()).setView(mB.getRoot()).create();
    d.setOnShowListener(dialog -> mShowTs = currentTimeMillis());
    return d;
  }

  private void doPairing() {
    CharSequence s = mB.pairingCodeV.getText();
    if (TextUtils.isEmpty(s)) {
      mB.pairingCodeCont.setError(getString(R.string.required_field_error));
      s = null;
    } else if ((s = s.toString().trim()).length() != 6) {
      s = null;
    }
    String code = s == null ? null : s.toString();

    String host = getHost(mB.pairingIpV.getText(), mB.pairingIpCont);
    int port = getPort(mB.pairingPortV.getText(), mB.pairingPortCont);

    if (code != null && host != null && port != 0) {
      mB.pairButton.setEnabled(false);
      mB.pairButton.setText(R.string.pairing_button);
      mB.pairingProg.setVisibility(View.VISIBLE);

      new LiveTasksQueueTyped<>(mDialogFrag, () -> doPairing(host, port, code))
          .onUiWith(this::onPairingComplete)
          .inBgWithFor(this::autoConnectAfterAutoPairing)
          .onUiWith(this::dismissDialog)
          .start();
    }
  }

  private static boolean mHiddenApiExempted = false;

  private boolean doPairing(String host, int port, String code) {
    if (!mHiddenApiExempted && VERSION.SDK_INT >= VERSION_CODES.P) {
      if (!HiddenApiBypass.addHiddenApiExemptions("Lcom/android/org/conscrypt/Conscrypt")) {
        MyLog.e(
            TAG, "doPairing", "Failed to access hidden class com.android.org.conscrypt.Conscrypt");
        return false;
      }
      mHiddenApiExempted = true;
    }

    try (AdbConnManager connMgr = new AdbConnManager()) {
      if (connMgr.pair(host, port, code)) {
        return true;
      } else {
        MyLog.e(TAG, "doPairing", "Adb pairing failed");
      }
    } catch (Exception e) {
      MyLog.e(TAG, "doPairing", e);
    }
    return false;
  }

  private void onPairingComplete(boolean result) {
    setConnectHelpText(result);

    if (result) {
      onArrowClicked(mB.pairArrow, true);
      mB.connectPortV.setText(null);
    } else {
      mB.pairButton.setEnabled(true);
      mB.pairButton.setText(R.string.pair_button);
      mB.pairingProg.setVisibility(View.INVISIBLE);
      UiUtils.showToast(R.string.adb_pairing_failed_toast);
    }
  }

  private boolean autoConnectAfterAutoPairing(boolean paired) {
    if (paired) {
      return connToAdbAndRestartDaemon(null, -1, false);
    }
    return false;
  }

  private void connectToAdb(boolean isFirstTry) {
    String host = getHost(mB.connectIpV.getText(), mB.connectIpCont);
    int port = getPort(mB.connectPortV.getText(), mB.connectPortCont);

    mB.connectButton.setEnabled(false);
    mB.connectButton.setText(R.string.connecting_button);
    mB.connectProg.setVisibility(View.VISIBLE);

    mA.setAdbCheckBox(false, false);

    new LiveTasksQueueTyped<>(mDialogFrag, () -> connToAdbAndRestartDaemon(host, port, !isFirstTry))
        .onUiWith(result -> onAdbConnect(result, isFirstTry))
        .start();
  }

  private boolean connToAdbAndRestartDaemon(String host, int port, boolean buttonPressed) {
    boolean retryAdbConnect = false;
    boolean onlyAutoConnect = host == null || port <= 0;

    if (!onlyAutoConnect) {
      if (buttonPressed
          && VERSION.SDK_INT >= VERSION_CODES.R
          && port != MySettings.INS.getAdbPort()) {

        try (AdbConnManager connMgr = new AdbConnManager()) {
          connMgr.setTimeout(10, TimeUnit.SECONDS);
          if (connMgr.connect(port)) {
            retryAdbConnect = callTcpIp(connMgr, port);
          } else {
            MyLog.e(TAG, "connToAdbAndRestartDaemon", "Adb connect for tcpip failed");
          }
        } catch (AdbException
            | InterruptedException
            | IOException
            | AdbPairingRequiredException e) {
          MyLog.e(TAG, "connToAdbAndRestartDaemon", e);
        }
      }

      MySettings.INS.setAdbHost(host);
      MySettings.INS.saveAdbPort(port);
    }

    boolean res;
    if (!buttonPressed || onlyAutoConnect) {
      res = NativeDaemon.getAdb(false, false, buttonPressed, host, port);
      SystemClock.sleep(mShowTs == 0 ? 1000 : Math.max(mShowTs + 1000 - currentTimeMillis(), 0));
    } else {
      res = NativeDaemon.forceGetAdb(retryAdbConnect);
    }

    if (res) {
      DaemonStarter.INS.switchToRootOrAdbDaemon(false);
    }

    return res;
  }

  private boolean callTcpIp(AdbConnManager connMgr, int port) {
    try (AdbStream adbStream = connMgr.openStream("tcpip:" + port)) {
      waitForTcpIpResponse(adbStream);
      return true;
    } catch (IOException | InterruptedException e) {
      MyLog.e(TAG, "callTcpIp", e);
      return false;
    }
  }

  private void waitForTcpIpResponse(AdbStream adbStream) {
    NonBlockingReader reader = new NonBlockingReader(adbStream.openInputStream());
    try {
      String line = reader.readLine(2000);
      if (line != null) {
        MyLog.i(TAG, "waitForTcpIpResponse", line);
      }
    } catch (IOException | InterruptedException e) {
      MyLog.e(TAG, "waitForTcpIpResponse", e);
    }
  }

  private void onAdbConnect(boolean result, boolean isFirstTry) {
    boolean dismissed = false;

    if (result) {
      dismissed = dismissDialog(true);
    } else if (isFirstTry) {
      onFirstTryFailed();
    } else {
      UiUtils.showToast(R.string.adb_connect_failed_toast);
    }

    if (!dismissed) {
      mB.connectButton.setEnabled(true);
      mB.connectButton.setText(R.string.connect_button);
      mB.connectProg.setVisibility(View.INVISIBLE);
    }
  }

  private boolean dismissDialog(boolean result) {
    boolean dismissed = false;
    if (result) {
      FragmentManager fm = mA.mA.getSupportFragmentManager();
      Fragment frag = fm.findFragmentByTag(TAG_ADB_CONNECTION);
      if (frag != null) {
        fm.beginTransaction().remove(frag).commitNowAllowingStateLoss();
        dismissed = true;
      }
      mA.showSnackBar(getString(R.string.connected_to_adb_toast), 5);
    }
    mA.setAdbCheckBox(result, true);
    return dismissed;
  }

  public static final int MIN_PORT = 1;
  public static final int MAX_PORT = 65535;

  private static int getPort(CharSequence portStr, TextInputLayout container) {
    if (!TextUtils.isEmpty(portStr)) {
      int port;
      try {
        port = Integer.parseInt(portStr.toString().trim());
      } catch (NumberFormatException ignored) {
        port = -1;
      }
      if (port <= MAX_PORT && port >= MIN_PORT) {
        return port;
      }
      container.setError(getString(R.string.invalid_field_error));
    } else {
      container.setError(getString(R.string.required_field_error));
    }
    return 0;
  }

  private static String getHost(CharSequence ipStr, TextInputLayout container) {
    if (!TextUtils.isEmpty(ipStr)) {
      String host = ipStr.toString().trim();

      if (Os.inet_pton(OsConstants.AF_INET, host) != null) {
        return host;
      }
      container.setError(getString(R.string.invalid_field_error));
    } else {
      container.setError(getString(R.string.required_field_error));
    }
    return null;
  }

  private static class EditTextWatcher implements TextWatcher {

    private final TextInputLayout mCont;

    private EditTextWatcher(TextInputLayout container) {
      mCont = container;
    }

    public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

    public void onTextChanged(CharSequence s, int start, int before, int count) {
      mCont.setErrorEnabled(false);
    }

    public void afterTextChanged(Editable s) {}
  }
}
