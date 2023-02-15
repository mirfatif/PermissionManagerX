package com.mirfatif.permissionmanagerx.parser.permsdb;

import androidx.room.Room;
import androidx.room.migration.Migration;
import androidx.sqlite.db.SupportSQLiteDatabase;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.privtasks.util.bg.BgRunner;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public enum PermsDb {
  INS;

  private final PermissionDao mPermDb =
      Room.databaseBuilder(App.getCxt(), PermissionDatabase.class, "permissions.db")
          .addMigrations(new MigrationV1ToV2())
          .build()
          .permissionDao();

  private static class MigrationV1ToV2 extends Migration {

    public MigrationV1ToV2() {
      super(1, 2);
    }

    public void migrate(SupportSQLiteDatabase database) {
      database.execSQL(
          "ALTER TABLE PermissionEntity ADD COLUMN `isPerUid` INTEGER NOT NULL default 0");
      database.execSQL(
          "ALTER TABLE PermissionEntity ADD COLUMN `userId` INTEGER NOT NULL default 0");
    }
  }

  public PermissionDao getDb() {
    return mPermDb;
  }

  private final Map<String, String> mRefs = Collections.synchronizedMap(new HashMap<>());

  private boolean mRefsBuilt = false;

  public boolean refsBuilt() {
    return mRefsBuilt;
  }

  public void buildRefs() {
    synchronized (mRefs) {
      mRefs.clear();

      String key;

      for (PermissionEntity entity : mPermDb.getAll()) {
        key =
            createKey(
                entity.pkgName, entity.permName, entity.isAppOps, entity.isPerUid, entity.userId);

        mRefs.put(key, entity.state);
      }

      mRefsBuilt = true;
    }

    BgRunner.execute(mPermDb::deleteDuplicates);
  }

  public static String createKey(
      String pkgName, String permName, boolean isAppOp, boolean isPerUid, int userId) {
    return pkgName + "_" + permName + "_" + isAppOp + "_" + isPerUid + "_" + userId;
  }

  public void updateRefs(
      String pkgName,
      String permName,
      String state,
      boolean isAppOp,
      boolean isPerUid,
      int userId) {
    String key = createKey(pkgName, permName, isAppOp, isPerUid, userId);
    mRefs.remove(key);
    if (state != null) {
      mRefs.put(key, state);
    }
  }

  public String getRef(
      String pkgName, String permName, boolean isAppOp, boolean isPerUid, int uid) {
    isPerUid = isPerUid && MySettings.INS.useUniqueRefForAppOpUidMode();
    int userId = PermsDbFlavor.getUserIdForPermRefs(uid);

    String ref = mRefs.get(createKey(pkgName, permName, isAppOp, isPerUid, userId));

    if (ref == null && isPerUid) {
      ref = mRefs.get(createKey(pkgName, permName, isAppOp, false, userId));
    }

    if (ref == null && userId != 0) {
      ref = mRefs.get(createKey(pkgName, permName, isAppOp, isPerUid, 0));
    }

    if (ref == null && isPerUid && userId != 0) {
      ref = mRefs.get(createKey(pkgName, permName, isAppOp, false, 0));
    }

    return ref;
  }

  public void updateRefsDb(PermissionEntity... entities) {
    for (PermissionEntity entity : entities) {
      int id =
          mPermDb.getId(
              entity.pkgName, entity.permName, entity.isAppOps, entity.isPerUid, entity.userId);
      if (id > 0) {
        entity.id = id;
      }
    }

    mPermDb.insertAll(entities);
  }
}
