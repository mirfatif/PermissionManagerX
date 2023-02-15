package com.mirfatif.permissionmanagerx.parser.permsdb;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import com.google.common.collect.Iterables;
import java.util.List;

@Dao
public interface PermissionDao {

  @Query("SELECT * FROM PermissionEntity")
  LiveData<List<PermissionEntity>> watchAll();

  @Query("SELECT * FROM PermissionEntity")
  List<PermissionEntity> getAll();

  @Query(
      "SELECT id FROM PermissionEntity "
          + "WHERE pkgName IS :pkgName "
          + "AND permName IS :permName "
          + "AND isAppOps IS :isAppOp "
          + "AND isPerUid IS :isPerUid "
          + "AND userId IS :userId")
  int getId(String pkgName, String permName, boolean isAppOp, boolean isPerUid, int userId);

  @Insert(onConflict = OnConflictStrategy.REPLACE)
  void insertAll(PermissionEntity... entities);

  @Query("DELETE FROM PermissionEntity WHERE pkgName IS :pkgName AND userId IS :userId")
  void deletePkg(String pkgName, int userId);

  @Query(
      "DELETE FROM PermissionEntity "
          + "WHERE pkgName IS :pkgName "
          + "AND permName IS :permName "
          + "AND isAppOps IS :isAppOp "
          + "AND isPerUid IS :isPerUid "
          + "AND userId IS :userId")
  void deletePerm(String pkgName, String permName, boolean isAppOp, boolean isPerUid, int userId);

  @Query("DELETE FROM PermissionEntity")
  void deleteAll();

  @Query("DELETE FROM PermissionEntity WHERE id IN (:ids)")
  void deletePerms(List<Integer> ids);

  static void deletePerms(PermissionDao dao, List<Integer> ids) {
    for (List<Integer> idList : Iterables.partition(ids, 999)) {
      dao.deletePerms(idList);
    }
  }

  @Query(
      "DELETE FROM PermissionEntity WHERE id NOT IN "
          + "(SELECT MIN(id) FROM PermissionEntity "
          + "GROUP BY pkgName, permName, isAppOps, isPerUid, userId)")
  void deleteDuplicates();

  @Query(
      "SELECT state FROM PermissionEntity "
          + "WHERE pkgName IS :pkgName "
          + "AND permName IS :permName "
          + "AND isAppOps IS :isAppOp "
          + "AND isPerUid IS :isPerUid "
          + "AND userId IS :userId")
  String getState(String pkgName, String permName, boolean isAppOp, boolean isPerUid, int userId);
}
