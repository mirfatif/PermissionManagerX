package com.mirfatif.permissionmanagerx;

import android.content.SharedPreferences;
import android.util.Log;
import android.util.Xml;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.preference.PreferenceManager;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
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

  private static final MySettings mMySettings = MySettings.getInstance();

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

  private final SharedPreferences mPreferences;

  BackupRestore() {
    mPreferences = PreferenceManager.getDefaultSharedPreferences(App.getContext());
  }

  void backup(OutputStream outputStream) {
    showProgressBar(TYPE_BACKUP);
    XmlSerializer serializer = Xml.newSerializer();
    StringWriter stringWriter = new StringWriter();
    try {
      serializer.setOutput(stringWriter);
      serializer.startDocument("UTF-8", true);
      serializer.startTag(null, ROOT);
      serializer.startTag(null, PREFERENCES);
    } catch (IOException e) {
      e.printStackTrace();
      callFailed(TYPE_BACKUP);
      return;
    }

    int badEntries = 0;

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
        Log.e(TAG, "Unrecognizable value: " + value.toString());
        badEntries++;
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
        callFailed(TYPE_BACKUP);
        return;
      }
    }

    try {
      serializer.endTag(null, PREFERENCES);
      serializer.startTag(null, PERMISSIONS);
    } catch (IOException e) {
      e.printStackTrace();
      callFailed(TYPE_BACKUP);
      return;
    }

    // permissions
    List<PermissionEntity> permEntities = mMySettings.getPermDb().getAll();
    for (PermissionEntity entity : permEntities) {
      try {
        serializer.startTag(null, PERM);
        serializer.attribute(null, KEY, entity.pkgName);
        serializer.attribute(null, VALUE, entity.state);
        serializer.attribute(null, TYPE, entity.permName);
        serializer.endTag(null, PERM);
      } catch (IOException e) {
        e.printStackTrace();
        callFailed(TYPE_BACKUP);
        return;
      }
    }

    try {
      serializer.endTag(null, PERMISSIONS);
      serializer.endTag(null, ROOT);
      serializer.endDocument();
      serializer.flush();
    } catch (IOException e) {
      e.printStackTrace();
      callFailed(TYPE_BACKUP);
      return;
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

    callSucceeded(TYPE_BACKUP, prefEntries.size(), permEntities.size(), badEntries);
  }

  void restore(InputStream inputStream) {
    showProgressBar(TYPE_RESTORE);
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

    int badEntries = 0;

    // preferences
    List<BackupEntry> prefEntries = getKeyValueEntries(inputStream1, PREFERENCES, PREF);
    if (prefEntries == null) {
      callFailed(TYPE_RESTORE);
      return;
    }

    SharedPreferences.Editor prefEdit = mPreferences.edit();
    for (BackupEntry entry : prefEntries) {
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
          Log.e(TAG, "Bad type: " + entry.type);
          badEntries++;
          break;
      }
      prefEdit.apply();
    }

    // permissions
    List<BackupEntry> permEntries = getKeyValueEntries(inputStream2, PERMISSIONS, PERM);
    if (permEntries == null) {
      callFailed(TYPE_RESTORE);
      return;
    }

    updatePermissionEntities(permEntries);

    callSucceeded(TYPE_RESTORE, prefEntries.size(), permEntries.size(), badEntries);
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

  static void updatePermissionEntities(List<BackupEntry> permEntries) {
    Map<String, Integer> map = new HashMap<>();
    for (PermissionEntity entity : mMySettings.getPermDb().getAll()) {
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
    mMySettings.getPermDb().insertAll(permEntities.toArray(new PermissionEntity[0]));
  }

  static final int TYPE_BACKUP = 1;
  static final int TYPE_RESTORE = 2;
  static final int FAILED = -1;

  private final MutableLiveData<int[]> backupRestoreResult = new MutableLiveData<>();

  LiveData<int[]> getBackupRestoreResult() {
    return backupRestoreResult;
  }

  private void showProgressBar(int type) {
    Utils.runInFg(() -> backupRestoreResult.setValue(new int[] {type}));
  }

  private void callFailed(int type) {
    Utils.runInFg(() -> backupRestoreResult.setValue(new int[] {type, FAILED}));
  }

  private void callSucceeded(int type, int prefs, int perms, int badEntries) {
    Utils.runInFg(() -> backupRestoreResult.setValue(new int[] {type, prefs, perms, badEntries}));
  }
}

class BackupEntry {
  String key, type, value;
}
