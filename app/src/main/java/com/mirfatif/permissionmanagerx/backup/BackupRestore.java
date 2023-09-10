package com.mirfatif.permissionmanagerx.backup;

import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.util.Xml;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermissionEntity;
import com.mirfatif.permissionmanagerx.parser.permsdb.PermsDb;
import com.mirfatif.permissionmanagerx.prefs.BackupRestoreFlavor;
import com.mirfatif.permissionmanagerx.prefs.ExcFiltersData;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.profile.PermProfileBackupRestore;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.privtasks.util.MyLog;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
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

public enum BackupRestore {
  INS;

  private static final String TAG = "BackupRestore";

  private static final String TAG_ROOT = "PermissionManagerX";

  private static final String TAG_PREFERENCES = "preferences";
  private static final String TAG_PREF = "pref";

  private static final String TAG_PERMISSIONS = "permissions";
  private static final String TAG_PERM = "perm";

  private static final String ATTR_KEY = "key";
  private static final String ATTR_VALUE = "value";
  private static final String ATTR_TYPE = "type";

  private static final String BOOLEAN = "boolean";
  private static final String FLOAT = "float";
  private static final String INT = "int";
  private static final String LONG = "long";
  private static final String SET = "Set";
  private static final String STRING = "String";

  private static final String SEPARATOR = ",";

  private static final String ATTR_PKG = "pkg";
  private static final String ATTR_PERM = "perm";
  private static final String ATTR_STATE = "state";
  private static final String ATTR_APP_OP = "app_op";
  private static final String ATTR_PER_UID = "per_uid";
  private static final String ATTR_USER_ID = "user_id";

  public static class SwapUserIds {

    private final int from, to;

    public SwapUserIds(int from, int to) {
      this.from = from;
      this.to = to;
    }
  }

  public Result backup(File file) {
    try (FileOutputStream fos = new FileOutputStream(file, false)) {
      return backup(fos, true, false, null);
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
      return null;
    }
  }

  public Result backup(Uri file) throws FileNotFoundException {
    return backup(file, true, false, null);
  }

  public Result backupNoThrow(
      Uri file, boolean backupPrefs, boolean skipUninstalledApps, SwapUserIds swapUserIds) {
    try {
      return backup(file, backupPrefs, skipUninstalledApps, swapUserIds);
    } catch (FileNotFoundException e) {
      MyLog.e(TAG, "backupNoThrow", e);
      return null;
    }
  }

  private Result backup(
      Uri file, boolean backupPrefs, boolean skipUninstalledApps, SwapUserIds swapUserIds)
      throws FileNotFoundException {
    try (OutputStream os = App.getCxt().getContentResolver().openOutputStream(file, "w")) {
      return backup(os, backupPrefs, skipUninstalledApps, swapUserIds);
    } catch (FileNotFoundException e) {
      throw e;
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
      return null;
    }
  }

  public Result backup(
      OutputStream outputStream,
      boolean backupPrefs,
      boolean skipUninstalledApps,
      SwapUserIds swap) {
    XmlSerializer serializer = Xml.newSerializer();
    StringWriter stringWriter = new StringWriter();
    try {
      serializer.setOutput(stringWriter);
      serializer.startDocument("UTF-8", true);
      serializer.startTag(null, TAG_ROOT);
      serializer.startTag(null, TAG_PREFERENCES);
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
      return null;
    }

    int prefCount = 0, invalidPrefs = 0;

    if (backupPrefs) {
      Map<String, ?> prefEntries = MySettings.getDefPrefs().getAll();

      for (Map.Entry<String, ?> entry : prefEntries.entrySet()) {
        String key = entry.getKey();

        if (isInvalidPrefKey(key)) {
          MyLog.i(TAG, "backup", "Skipping " + key);
          continue;
        }

        prefCount++;

        Object value = entry.getValue();
        String type;

        if (value instanceof Boolean) {
          type = BOOLEAN;
        } else if (value instanceof Float) {
          type = FLOAT;
        } else if (value instanceof Integer) {
          type = INT;
        } else if (value instanceof Long) {
          type = LONG;
        } else if (value instanceof Set) {
          type = SET;
          StringBuilder stringBuilder = new StringBuilder();
          for (Object object : (Set<?>) value) {
            if (stringBuilder.length() != 0) {

              stringBuilder.append(SEPARATOR);
            }
            stringBuilder.append(object.toString());
          }
          value = stringBuilder;
        } else if (value instanceof String) {
          type = STRING;
        } else {
          MyLog.e(TAG, "backup", "Unknown preference type: " + value.toString());
          invalidPrefs++;
          continue;
        }

        try {
          serializer.startTag(null, TAG_PREF);
          serializer.attribute(null, ATTR_KEY, key);
          serializer.attribute(null, ATTR_VALUE, value.toString());
          serializer.attribute(null, ATTR_TYPE, type);
          serializer.endTag(null, TAG_PREF);
        } catch (IOException e) {
          MyLog.e(TAG, "backup", e);
          return null;
        }
      }
    }

    try {
      serializer.endTag(null, TAG_PREFERENCES);
      serializer.startTag(null, TAG_PERMISSIONS);
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
      return null;
    }

    List<PermissionEntity> permEntities = PermsDb.INS.getDb().getAll();

    int permCount = permEntities.size();
    int skippedApps = 0;

    if (skipUninstalledApps) {
      List<String> pkgList = getAllPkgList();
      int permEntitiesSize = permEntities.size();
      permEntities.removeIf(entity -> !pkgList.contains(entity.pkgName));
      skippedApps = permEntitiesSize - permEntities.size();
    }

    for (PermissionEntity entity : permEntities) {
      if (swap != null && entity.userId == swap.from) {
        entity.userId = swap.to;
      }

      try {
        serializer.startTag(null, TAG_PERM);

        serializer.attribute(null, ATTR_PKG, entity.pkgName);
        serializer.attribute(null, ATTR_PERM, entity.permName);
        serializer.attribute(null, ATTR_STATE, entity.state);
        serializer.attribute(null, ATTR_APP_OP, String.valueOf(entity.isAppOps));
        serializer.attribute(null, ATTR_PER_UID, String.valueOf(entity.isPerUid));
        serializer.attribute(null, ATTR_USER_ID, String.valueOf(entity.userId));

        serializer.endTag(null, TAG_PERM);
      } catch (IOException e) {
        MyLog.e(TAG, "backup", e);
        return null;
      }
    }

    try {
      serializer.endTag(null, TAG_PERMISSIONS);
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
      return null;
    }

    int profileCount;

    try {
      profileCount = PermProfileBackupRestore.backup(serializer);
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
      return null;
    }

    try {
      serializer.endTag(null, TAG_ROOT);
      serializer.endDocument();
      serializer.flush();
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
      return null;
    }

    Source input = new StreamSource(new StringReader(stringWriter.toString()));
    StreamResult output = new StreamResult(outputStream);
    try {
      Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, "yes");
      transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
      transformer.transform(input, output);
    } catch (TransformerException e) {
      MyLog.e(TAG, "backup", e);
    }

    try {
      outputStream.flush();
      outputStream.close();
    } catch (IOException e) {
      MyLog.e(TAG, "backup", e);
    }

    MyLog.i(TAG, "backup", "Succeeded");

    return new Result(prefCount, permCount, profileCount, invalidPrefs, skippedApps);
  }

  public Result restore(Uri file, boolean skipUninstalledApps, SwapUserIds swapUserIds) {
    Result res;

    try (InputStream is = App.getCxt().getContentResolver().openInputStream(file)) {
      if (is == null) {
        MyLog.e(TAG, "restore", "Failed to get InputStream");
        return null;
      }
      res = restore(is, skipUninstalledApps, swapUserIds);
    } catch (IOException | SecurityException e) {

      MyLog.e(TAG, "restore", e);
      return null;
    }

    if (res != null) {
      ExcFiltersData.INS.populateLists(true);
      PermsDb.INS.buildRefs();
      BackupRestoreFlavor.onRestoreDone();
    }

    return res;
  }

  public Result restore(InputStream is, boolean skipUninstalledApps, SwapUserIds swap) {

    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
    byte[] buffer = new byte[1024];
    int len;
    try {
      while ((len = is.read(buffer)) > -1) {
        byteArrayOutputStream.write(buffer, 0, len);
      }
      byteArrayOutputStream.flush();
    } catch (IOException e) {
      MyLog.e(TAG, "restore", e);
      return null;
    }

    InputStream inputStream1 = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
    InputStream inputStream2 = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
    InputStream inputStream3 = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());

    int invalidPrefs = 0;

    String[] attrNames = new String[] {ATTR_KEY, ATTR_VALUE, ATTR_TYPE};
    List<String[]> entries = getKeyValueEntries(inputStream1, TAG_PREFERENCES, TAG_PREF, attrNames);

    if (entries == null) {
      return null;
    }

    int prefCount = entries.size();

    String key, value, type;

    SharedPreferences.Editor prefEdit = MySettings.getDefPrefs().edit();
    for (String[] entry : entries) {
      key = entry[0];
      value = entry[1];
      type = entry[2];

      if (isInvalidPrefKey(key)) {
        MyLog.e(TAG, "restore", "Invalid preference: " + key);
        invalidPrefs++;
        continue;
      }

      switch (type) {
        case BOOLEAN -> prefEdit.putBoolean(key, Boolean.parseBoolean(value));
        case FLOAT -> prefEdit.putFloat(key, Float.parseFloat(value));
        case INT -> prefEdit.putInt(key, Integer.parseInt(value));
        case LONG -> prefEdit.putLong(key, Long.parseLong(value));
        case SET -> {
          if (value.length() == 0) {

            prefEdit.putStringSet(key, new HashSet<>());
          } else {
            prefEdit.putStringSet(key, new HashSet<>(Arrays.asList(value.split(SEPARATOR))));
          }
        }
        case STRING -> prefEdit.putString(key, value);
        default -> {
          MyLog.e(TAG, "restore", "Unknown preference type: " + type);
          invalidPrefs++;
        }
      }
      prefEdit.apply();
    }

    attrNames =
        new String[] {
          ATTR_PKG,
          ATTR_PERM,
          ATTR_STATE,
          ATTR_APP_OP,
          ATTR_PER_UID,
          ATTR_USER_ID,
          ATTR_KEY,
          ATTR_VALUE,
          ATTR_TYPE
        };

    entries = getKeyValueEntries(inputStream2, TAG_PERMISSIONS, TAG_PERM, attrNames);
    if (entries == null) {
      return null;
    }

    int permCount = entries.size();

    List<PermissionEntity> entities = new ArrayList<>();

    String pkgName, permName, state;
    boolean isAppOp, isPerUid;
    int userId;

    for (String[] entry : entries) {
      pkgName = entry[0] != null ? entry[0] : entry[6];
      permName = entry[1] != null ? entry[1] : entry[8];
      state = entry[2] != null ? entry[2] : entry[7];
      isAppOp = entry[3] != null && Boolean.parseBoolean(entry[3]);
      isPerUid = entry[4] != null && Boolean.parseBoolean(entry[4]);

      try {
        userId = entry[5] == null ? 0 : Integer.parseInt(entry[5]);
      } catch (NumberFormatException ignored) {
        userId = 0;
      }

      if (swap != null && userId == swap.from) {
        userId = swap.to;
      }

      entities.add(new PermissionEntity(pkgName, permName, state, isAppOp, isPerUid, userId));
    }

    int skippedApps = 0;

    if (skipUninstalledApps) {
      List<String> pkgList = getAllPkgList();
      int permEntriesSize = entities.size();
      entities.removeIf(entity -> !pkgList.contains(entity.pkgName));
      skippedApps = permEntriesSize - entities.size();
    }

    PermsDb.INS.updateRefsDb(entities.toArray(new PermissionEntity[0]));

    if (!entries.isEmpty() && entries.get(0)[0] == null) {

      if (!AppOpsParser.INS.fixPermDb()) {
        MySettings.INS.setFixPermDb(true);
      }
    }

    int profileCount;

    try {
      profileCount = PermProfileBackupRestore.restore(inputStream3);
    } catch (Exception ignored) {
      return null;
    }

    MyLog.i(TAG, "restore", "Succeeded");

    return new Result(prefCount, permCount, profileCount, invalidPrefs, skippedApps);
  }

  public static List<String[]> getKeyValueEntries(
      InputStream inputStream, String mainTag, String entryTag, String[] attrNames) {
    XmlPullParser xmlParser = Xml.newPullParser();

    List<String[]> entries = new ArrayList<>();

    boolean rootTagFound = false;
    boolean mainTagFound = false;

    try {
      xmlParser.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, false);
      xmlParser.setInput(inputStream, null);
      while (true) {
        int eventType = xmlParser.next();
        if (eventType == XmlPullParser.END_DOCUMENT) {
          break;
        }

        String tagName = xmlParser.getName();
        if (eventType == XmlPullParser.START_TAG && tagName.equals(TAG_ROOT)) {
          rootTagFound = true;
        }
        if (eventType == XmlPullParser.START_TAG && tagName.equals(mainTag)) {
          mainTagFound = true;
        }

        if (!rootTagFound || !mainTagFound) {
          continue;
        }

        if (eventType == XmlPullParser.END_TAG && tagName.equals(mainTag)) {
          break;
        }

        if (eventType == XmlPullParser.START_TAG && tagName.equals(entryTag)) {
          String[] values = new String[attrNames.length];
          for (int i = 0; i < attrNames.length; i++) {
            values[i] = xmlParser.getAttributeValue(null, attrNames[i]);
          }

          entries.add(values);
        }
      }
    } catch (IOException | XmlPullParserException e) {
      MyLog.e(TAG, "getKeyValueEntries", e);
      return null;
    }

    return entries;
  }

  private final List<String> mBackupAblePrefs = new ArrayList<>();

  private boolean isInvalidPrefKey(String prefKey) {
    synchronized (mBackupAblePrefs) {
      if (mBackupAblePrefs.isEmpty()) {
        int resId;
        String name;

        for (Field field : R.string.class.getDeclaredFields()) {
          name = field.getName();

          try {
            resId = R.string.class.getDeclaredField(name).getInt(null);
          } catch (NoSuchFieldException | IllegalAccessException e) {
            continue;
          }

          String str = App.getRes().getString(resId);

          if (str.startsWith("pref_") && !str.endsWith("_enc") && name.equals(str + "_key")) {
            mBackupAblePrefs.add(str);
          }
        }
      }
    }

    return !mBackupAblePrefs.contains(prefKey);
  }

  private List<String> getAllPkgList() {
    return ApiUtils.getInstalledPackages(PackageManager.MATCH_UNINSTALLED_PACKAGES).parallelStream()
        .map(pkgInfo -> pkgInfo.packageName)
        .collect(Collectors.toList());
  }

  public static class Result {

    public final int prefs, perms, profiles, invalidPrefs, skippedApps;

    private Result(int prefs, int perms, int profiles, int invalidPrefs, int skippedApps) {
      this.prefs = prefs;
      this.perms = perms;
      this.profiles = profiles;
      this.invalidPrefs = invalidPrefs;
      this.skippedApps = skippedApps;
    }
  }
}
