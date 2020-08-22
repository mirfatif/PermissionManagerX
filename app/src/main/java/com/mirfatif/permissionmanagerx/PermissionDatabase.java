package com.mirfatif.permissionmanagerx;

import androidx.room.Database;
import androidx.room.RoomDatabase;

@Database(entities = PermissionEntity.class, version = 1, exportSchema = false)
public abstract class PermissionDatabase extends RoomDatabase {
  public abstract PermissionDao permissionDao();
}
