package com.mirfatif.permissionmanagerx.permsdb;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import java.util.List;

@Dao
public interface PermissionDao {
  @SuppressWarnings("UnusedDeclaration")
  @Query("SELECT * FROM PermissionEntity")
  LiveData<List<PermissionEntity>> watchAll();

  @Query("SELECT * FROM PermissionEntity")
  List<PermissionEntity> getAll();

  @Query("SELECT id FROM PermissionEntity WHERE pkgName IS :pkg AND permName IS :permName")
  int getId(String pkg, String permName);

  @Insert(onConflict = OnConflictStrategy.REPLACE)
  void insertAll(PermissionEntity... entities);

  @Query("DELETE FROM PermissionEntity WHERE pkgName IS :pkgName")
  void deletePackage(String pkgName);

  @Query("DELETE FROM PermissionEntity WHERE pkgName IS :pkgName AND permName IS :permName")
  void deletePermission(String pkgName, String permName);
}
