package com.mirfatif.permissionmanagerx;

import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.net.Uri;
import android.util.Log;
import android.util.Xml;
import android.view.View;
import android.widget.CheckBox;
import android.widget.Toast;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AlertDialog.Builder;
import androidx.preference.PreferenceManager;
import com.mirfatif.permissionmanagerx.permsdb.PermissionEntity;
import com.mirfatif.privdaemon.Util;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlSerializer;

public class BackupRestore {

  static final String TAG = "BackupRestore";
  private static final String TAG_BACKUP_RESTORE = "BACKUP_RESTORE";

  private final MySettings mMySettings;

  private final String KEY = "key";
  private final String VALUE = "value";
  private final String TYPE = "type";

  private final String BOOLEAN = "boolean";
  private final String FLOAT = "float";
  private final String INT = "int";
  private final String LONG = "long";
  private final String SET = "Set";
  private final String STRING = "String";

  // root element
  private final String ROOT = "PermissionManagerX";

  // SharedPreferences
  private final String PREFERENCES = "preferences";
  private final String PREF = "pref";

  // permissions
  private final String PERMISSIONS = "permissions";
  private final String PERM = "perm";

  private final MainActivity mActivity;
  private final SharedPreferences mPreferences;

  private boolean mSkipUninstalledApps = false;

  BackupRestore() {
    mActivity = null;
    mMySettings = MySettings.getInstance();
    mPreferences = PreferenceManager.getDefaultSharedPreferences(App.getContext());
  }

  BackupRestore(MainActivity activity) {
    mActivity = activity;
    mMySettings = MySettings.getInstance();
    mPreferences = PreferenceManager.getDefaultSharedPreferences(App.getContext());
  }

  void doBackupRestore() {
    View layout = mActivity.getLayoutInflater().inflate(R.layout.backup_restore_alert_dialog, null);
    CheckBox checkbox = layout.findViewById(R.id.skip_uninstalled_packages);
    checkbox.setOnClickListener(v -> mSkipUninstalledApps = checkbox.isChecked());

    AlertDialog dialog =
        new Builder(mActivity)
            .setPositiveButton(R.string.backup, (d, which) -> doBackupRestore(true))
            .setNegativeButton(R.string.restore, (d, which) -> doBackupRestore(false))
            .setNeutralButton(android.R.string.cancel, null)
            .setTitle(getString(R.string.backup) + " / " + getString(R.string.restore))
            .setView(layout)
            .create();
    new AlertDialogFragment(dialog).show(mActivity.mFM, TAG_BACKUP_RESTORE, false);
  }

  private void doBackupRestore(boolean isBackup) {
    Toast.makeText(App.getContext(), R.string.select_backup_file, Toast.LENGTH_LONG).show();
    ActivityResultCallback<Uri> callback =
        uri -> Utils.runInBg(() -> doBackupRestoreInBg(isBackup, uri));
    if (isBackup) {
      mActivity
          .registerForActivityResult(new ActivityResultContracts.CreateDocument(), callback)
          .launch("PermissionManagerX_" + Util.getCurrDateTime() + ".xml");
    } else {
      mActivity
          .registerForActivityResult(new ActivityResultContracts.OpenDocument(), callback)
          .launch(new String[] {"text/xml"});
    }
  }

  private void doBackupRestoreInBg(boolean isBackup, Uri uri) {
    if (isBackup) {
      try (OutputStream outStream =
          mActivity.getApplication().getContentResolver().openOutputStream(uri, "w")) {
        if (!backup(outStream)) {
          failed(true);
        }
      } catch (IOException e) {
        e.printStackTrace();
      }
    } else {
      try (InputStream inputStream =
          mActivity.getApplication().getContentResolver().openInputStream(uri)) {
        /**
         * So that not saved preferences are restored. Must be in background so that {@link
         * PrivDaemonHandler#sendRequest(String)} in {@link PackageParser#buildAppOpsList()} in ADB
         * daemon mode is not called on main thread
         */
        mMySettings.resetToDefaults();
        if (!restore(inputStream)) {
          failed(false);
        }
      } catch (IOException ignored) {
      }
    }
  }

  boolean backup(OutputStream outputStream) {
    showProgressBar(true);
    XmlSerializer serializer = Xml.newSerializer();
    StringWriter stringWriter = new StringWriter();
    try {
      serializer.setOutput(stringWriter);
      serializer.startDocument("UTF-8", true);
      serializer.startTag(null, ROOT);
      serializer.startTag(null, PREFERENCES);
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }

    int invalidPrefs = 0;

    // preferences
    Map<String, ?> prefEntries = mPreferences.getAll();
    for (Map.Entry<String, ?> entry : prefEntries.entrySet()) {
      Object value = entry.getValue();
      String type;

      if (value instanceof Boolean) type = BOOLEAN;
      else if (value instanceof Float) type = FLOAT;
      else if (value instanceof Integer) type = INT;
      else if (value instanceof Long) type = LONG;
      else if (value instanceof Set) {
        type = SET;
        StringBuilder stringBuilder = null;
        for (Object object : (Set<?>) value) {
          if (stringBuilder == null) stringBuilder = new StringBuilder(object.toString());
          else stringBuilder.append(",").append(object.toString());
        }
        if (stringBuilder != null) value = stringBuilder;
      } else if (value instanceof String) type = STRING;
      else {
        Log.e(TAG, "Unknown preference type: " + value.toString());
        invalidPrefs++;
        continue;
      }

      try {
        serializer.startTag(null, PREF);
        serializer.attribute(null, KEY, entry.getKey());
        serializer.attribute(null, VALUE, value.toString());
        serializer.attribute(null, TYPE, type);
        serializer.endTag(null, PREF);
      } catch (IOException e) {
        e.printStackTrace();
        return false;
      }
    }

    try {
      serializer.endTag(null, PREFERENCES);
      serializer.startTag(null, PERMISSIONS);
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }

    // permissions
    List<PermissionEntity> permEntities = mMySettings.getPermDb().getAll();
    int skippedApps = 0;

    if (mSkipUninstalledApps) {
      List<PermissionEntity> permEntitiesCleaned = new ArrayList<>();
      for (PermissionEntity entity : permEntities) {
        if (isInstalled(entity.pkgName)) {
          permEntitiesCleaned.add(entity);
        } else {
          skippedApps++;
        }
      }
      permEntities = permEntitiesCleaned;
    }

    for (PermissionEntity entity : permEntities) {
      try {
        serializer.startTag(null, PERM);
        serializer.attribute(null, KEY, entity.pkgName);
        serializer.attribute(null, VALUE, entity.state);
        serializer.attribute(null, TYPE, entity.permName);
        serializer.endTag(null, PERM);
      } catch (IOException e) {
        e.printStackTrace();
        return false;
      }
    }

    try {
      serializer.endTag(null, PERMISSIONS);
      serializer.endTag(null, ROOT);
      serializer.endDocument();
      serializer.flush();
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }

    // pretty formatting
    Source input = new StreamSource(new StringReader(stringWriter.toString()));
    StreamResult output = new StreamResult(outputStream);
    try {
      Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, "yes");
      transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
      transformer.transform(input, output);
    } catch (TransformerException e) {
      e.printStackTrace();
    }

    try {
      stringWriter.flush();
      stringWriter.close();
      outputStream.flush();
      outputStream.close();
    } catch (IOException ignored) {
    }

    succeeded(true, prefEntries.size(), permEntities.size(), invalidPrefs, skippedApps);
    return true;
  }

  private boolean restore(InputStream inputStream) {
    showProgressBar(false);
    // create a copy of InputStream before consuming it
    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
    byte[] buffer = new byte[1024];
    int len;
    try {
      while ((len = inputStream.read(buffer)) > -1) {
        byteArrayOutputStream.write(buffer, 0, len);
      }
      byteArrayOutputStream.flush();
    } catch (IOException e) {
      e.printStackTrace();
    }
    InputStream inputStream1 = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
    InputStream inputStream2 = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());

    int invalidPrefs = 0;

    // preferences
    List<BackupEntry> prefEntries = getKeyValueEntries(inputStream1, PREFERENCES, PREF);
    if (prefEntries == null) {
      return false;
    }

    SharedPreferences.Editor prefEdit = mPreferences.edit();
    for (BackupEntry entry : prefEntries) {

      if (!isValidPrefKey(entry.key)) {
        Log.e(TAG, "Invalid preference: " + entry.key);
        invalidPrefs++;
        continue;
      }

      switch (entry.type) {
        case BOOLEAN:
          prefEdit.putBoolean(entry.key, Boolean.parseBoolean(entry.value));
          break;
        case FLOAT:
          prefEdit.putFloat(entry.key, Float.parseFloat(entry.value));
          break;
        case INT:
          prefEdit.putInt(entry.key, Integer.parseInt(entry.value));
          break;
        case LONG:
          prefEdit.putLong(entry.key, Long.parseLong(entry.value));
          break;
        case SET:
          prefEdit.putStringSet(entry.key, new HashSet<>(Arrays.asList(entry.value.split(","))));
          break;
        case STRING:
          prefEdit.putString(entry.key, entry.value);
          break;
        default:
          Log.e(TAG, "Unknown preference type: " + entry.type);
          invalidPrefs++;
          break;
      }
      prefEdit.apply();
    }

    // permissions
    List<BackupEntry> permEntries = getKeyValueEntries(inputStream2, PERMISSIONS, PERM);
    if (permEntries == null) {
      return false;
    }

    int skippedApps = 0;
    if (mSkipUninstalledApps) {
      List<BackupEntry> permEntriesCleaned = new ArrayList<>();
      for (BackupEntry entry : permEntries) {
        if (isInstalled(entry.key)) {
          permEntriesCleaned.add(entry);
        } else {
          skippedApps++;
        }
      }
      permEntries = permEntriesCleaned;
    }

    updatePermissionEntities(permEntries);

    succeeded(false, prefEntries.size(), permEntries.size(), invalidPrefs, skippedApps);
    return true;
  }

  private List<BackupEntry> getKeyValueEntries(
      InputStream inputStream, String mainTag, String entryTag) {
    XmlPullParser xmlParser = Xml.newPullParser();
    List<BackupEntry> backupEntryList = new ArrayList<>();
    boolean rootTagFound = false;
    boolean mainTagFound = false;
    try {
      xmlParser.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, false);
      xmlParser.setInput(inputStream, null);
      while (true) {
        int eventType = xmlParser.next(); // get the next parsing event
        if (eventType == XmlPullParser.END_DOCUMENT) break;

        String tagName = xmlParser.getName();
        if (eventType == XmlPullParser.START_TAG && tagName.equals(ROOT)) rootTagFound = true;
        if (eventType == XmlPullParser.START_TAG && tagName.equals(mainTag)) mainTagFound = true;

        if (!rootTagFound || !mainTagFound) continue;

        // if we reach the end of "preferences" or "permissions"
        if (eventType == XmlPullParser.END_TAG && tagName.equals(mainTag)) break;

        // if we are at the start of "pref" or "perm"
        if (eventType == XmlPullParser.START_TAG && tagName.equals(entryTag)) {
          BackupEntry entry = new BackupEntry();
          entry.key = xmlParser.getAttributeValue(null, KEY);
          entry.value = xmlParser.getAttributeValue(null, VALUE);
          entry.type = xmlParser.getAttributeValue(null, TYPE);
          backupEntryList.add(entry);
        }
      }
    } catch (IOException | XmlPullParserException e) {
      e.printStackTrace();
      return null;
    }
    return backupEntryList;
  }

  private final List<String> mPrefKeys = new ArrayList<>();

  private boolean isValidPrefKey(String prefKey) {
    if (mPrefKeys.isEmpty()) {
      for (Field field : R.string.class.getDeclaredFields()) {
        String strName = field.getName();
        if (!strName.startsWith("pref_")) continue;
        if (!strName.endsWith("_key")) continue;
        if (strName.endsWith("_enc_key")) continue;

        int strKeyResId = Utils.getIntField(strName, R.string.class, TAG);
        String _prefKey = getString(strKeyResId);
        mPrefKeys.add(_prefKey);
      }
    }
    return mPrefKeys.contains(prefKey);
  }

  private final List<String> mInstalledPackages = new ArrayList<>();

  private boolean isInstalled(String pkgName) {
    if (mInstalledPackages.isEmpty()) {
      for (PackageInfo info : App.getContext().getPackageManager().getInstalledPackages(0)) {
        mInstalledPackages.add(info.packageName);
      }
    }
    return mInstalledPackages.contains(pkgName);
  }

  static void updatePermissionEntities(List<BackupEntry> permEntries) {
    MySettings mySettings = MySettings.getInstance();
    Map<String, Integer> map = new HashMap<>();
    for (PermissionEntity entity : mySettings.getPermDb().getAll()) {
      map.put(entity.pkgName + "_" + entity.permName, entity.id);
    }

    List<PermissionEntity> permEntities = new ArrayList<>();
    for (BackupEntry entry : permEntries) {
      PermissionEntity entity = new PermissionEntity();
      entity.pkgName = entry.key;
      entity.state = entry.value;
      entity.permName = entry.type;
      Integer id = map.get(entity.pkgName + "_" + entity.permName);
      if (id != null && id > 0) entity.id = id;
      permEntities.add(entity);
    }
    mySettings.getPermDb().insertAll(permEntities.toArray(new PermissionEntity[0]));
  }

  private String getString(int resId) {
    return App.getContext().getString(resId);
  }

  private void showProgressBar(boolean isBackup) {
    if (mActivity == null) return;
    Utils.runInFg(
        () -> {
          mActivity.mRoundProgressTextView.setText(
              isBackup
                  ? getString(R.string.backup_in_progress)
                  : getString(R.string.restore_in_progress));
          mActivity.mRoundProgressContainer.setVisibility(View.VISIBLE);
        });
  }

  private void failed(boolean isBackup) {
    if (mActivity == null) {
      Log.e(TAG, (isBackup ? "Backup" : "Restore") + " failed");
      return;
    }
    Utils.runInFg(() -> mActivity.mRoundProgressContainer.setVisibility(View.GONE));
    showFinalDialog(isBackup, getString(R.string.backup_restore_failed));
  }

  private void succeeded(
      boolean isBackup, int prefs, int perms, int invalidPrefs, int skippedApps) {
    if (mActivity == null) {
      Log.i(TAG, (isBackup ? "Backup" : "Restore") + " succeeded");
      return;
    }
    Utils.runInFg(() -> mActivity.mRoundProgressContainer.setVisibility(View.GONE));
    if (!isBackup) {
      mMySettings.populateExcludedAppsList(false);
      mMySettings.populateExcludedPermsList();
      mMySettings.populateExtraAppOpsList(false);
      mActivity.mPackageParser.buildPermRefList();
      mActivity.updatePackagesList(false);
    }

    String message = mActivity.getString(R.string.backup_restore_process_entries, prefs, perms);
    if (invalidPrefs > 0) {
      message += mActivity.getString(R.string.backup_restore_invalid_prefs, invalidPrefs);
    }
    if (skippedApps > 0) {
      message += mActivity.getString(R.string.backup_restore_uninstalled_apps, skippedApps);
    }

    showFinalDialog(isBackup, message);
  }

  private void showFinalDialog(boolean isBackup, String message) {
    if (mActivity == null) return;
    Builder builder =
        new Builder(mActivity)
            .setPositiveButton(android.R.string.ok, null)
            .setTitle(isBackup ? R.string.backup : R.string.restore)
            .setMessage(message);
    Utils.runInFg(
        () ->
            new AlertDialogFragment(builder.create())
                .show(mActivity.mFM, TAG_BACKUP_RESTORE, false));
  }
}

class BackupEntry {
  String key, type, value;
}
