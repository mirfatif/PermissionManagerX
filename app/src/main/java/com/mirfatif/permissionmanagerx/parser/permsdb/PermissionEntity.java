package com.mirfatif.permissionmanagerx.parser.permsdb;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity
public class PermissionEntity {

  @PrimaryKey(autoGenerate = true)
  public int id;

  @ColumnInfo(name = "pkgName")
  public String pkgName;

  @ColumnInfo(name = "permName")
  public String permName;

  @ColumnInfo(name = "state")
  public String state;

  @ColumnInfo(name = "isAppOps")
  public boolean isAppOps;

  @ColumnInfo(name = "isPerUid")
  public boolean isPerUid;

  @ColumnInfo(name = "userId")
  public int userId;

  public PermissionEntity() {}

  public PermissionEntity(
      String pkgName, String permName, String state, boolean isAppOp, boolean isPerUid) {
    this.pkgName = pkgName;
    this.permName = permName;
    this.state = state;
    this.isAppOps = isAppOp;
    this.isPerUid = isPerUid;
  }
}
