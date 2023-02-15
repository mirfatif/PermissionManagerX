package com.mirfatif.privtasks.bind;

import android.os.Parcel;
import android.os.Parcelable;
import java.util.List;

public class MyPackageOps implements Parcelable {

  public final String pkgName;
  public final List<MyOpEntry> opEntryList;

  public MyPackageOps(String pkgName, List<MyOpEntry> opEntryList) {
    this.pkgName = pkgName;
    this.opEntryList = opEntryList;
  }

  protected MyPackageOps(Parcel in) {
    pkgName = in.readString();
    opEntryList = in.createTypedArrayList(MyOpEntry.CREATOR);
  }

  public void writeToParcel(Parcel dest, int flags) {
    dest.writeString(pkgName);
    dest.writeTypedList(opEntryList);
  }

  public int describeContents() {
    return 0;
  }

  public static final Creator<MyPackageOps> CREATOR =
      new Creator<MyPackageOps>() {

        public MyPackageOps createFromParcel(Parcel in) {
          return new MyPackageOps(in);
        }

        public MyPackageOps[] newArray(int size) {
          return new MyPackageOps[size];
        }
      };

  public static class MyOpEntry implements Parcelable {

    public int op;
    public long lastAccessTime;
    public int opMode;

    public MyOpEntry() {}

    protected MyOpEntry(Parcel in) {
      op = in.readInt();
      lastAccessTime = in.readLong();
      opMode = in.readInt();
    }

    public void writeToParcel(Parcel dest, int flags) {
      dest.writeInt(op);
      dest.writeLong(lastAccessTime);
      dest.writeInt(opMode);
    }

    public int describeContents() {
      return 0;
    }

    public static final Creator<MyOpEntry> CREATOR =
        new Creator<MyOpEntry>() {

          public MyOpEntry createFromParcel(Parcel in) {
            return new MyOpEntry(in);
          }

          public MyOpEntry[] newArray(int size) {
            return new MyOpEntry[size];
          }
        };
  }
}
