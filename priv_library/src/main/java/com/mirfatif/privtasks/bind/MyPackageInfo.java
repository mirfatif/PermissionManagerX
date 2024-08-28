package com.mirfatif.privtasks.bind;

import android.os.Parcel;
import android.os.Parcelable;

public class MyPackageInfo implements Parcelable {

  public final String packageName;

  public final int[] requestedPermissionsFlags;

  public final int uid;

  public final boolean enabled;

  public MyPackageInfo(String pkgName, int[] reqPermsFlags, int uid, boolean enabled) {
    packageName = pkgName;
    requestedPermissionsFlags = reqPermsFlags;
    this.uid = uid;
    this.enabled = enabled;
  }

  protected MyPackageInfo(Parcel in) {
    packageName = in.readString();
    requestedPermissionsFlags = in.createIntArray();
    uid = in.readInt();
    enabled = in.readByte() != 0;
  }

  public static final Creator<MyPackageInfo> CREATOR =
      new Creator<>() {
        public MyPackageInfo createFromParcel(Parcel in) {
          return new MyPackageInfo(in);
        }

        public MyPackageInfo[] newArray(int size) {
          return new MyPackageInfo[size];
        }
      };

  public int describeContents() {
    return 0;
  }

  public void writeToParcel(Parcel dest, int flags) {
    dest.writeString(packageName);
    dest.writeIntArray(requestedPermissionsFlags);
    dest.writeInt(uid);
    dest.writeByte((byte) (enabled ? 1 : 0));
  }
}
