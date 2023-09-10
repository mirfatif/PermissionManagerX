package com.mirfatif.privtasks.bind;

import android.os.Parcel;
import android.os.Parcelable;
import java.util.ArrayList;
import java.util.List;

public class PrivsStatus implements Parcelable {

  public PrivsStatus() {}

  public boolean opToDefModeWorks;

  public boolean opToSwitchWorks;

  public boolean opToNameWorks;

  public boolean getOpsWorks;

  public boolean opNumConsistent;

  public boolean opModeConsistent;

  public List<PermStatus> permStatusList = new ArrayList<>();

  protected PrivsStatus(Parcel in) {
    opToDefModeWorks = in.readInt() != 0;
    opToSwitchWorks = in.readInt() != 0;
    opToNameWorks = in.readInt() != 0;
    getOpsWorks = in.readInt() != 0;
    opNumConsistent = in.readInt() != 0;
    opModeConsistent = in.readInt() != 0;
    permStatusList = in.createTypedArrayList(PermStatus.CREATOR);
  }

  public void writeToParcel(Parcel dest, int flags) {
    dest.writeInt(opToDefModeWorks ? 1 : 0);
    dest.writeInt(opToSwitchWorks ? 1 : 0);
    dest.writeInt(opToNameWorks ? 1 : 0);
    dest.writeInt(getOpsWorks ? 1 : 0);
    dest.writeInt(opNumConsistent ? 1 : 0);
    dest.writeInt(opModeConsistent ? 1 : 0);
    dest.writeTypedList(permStatusList);
  }

  public int describeContents() {
    return 0;
  }

  public static final Creator<PrivsStatus> CREATOR =
      new Creator<>() {

        public PrivsStatus createFromParcel(Parcel in) {
          return new PrivsStatus(in);
        }

        public PrivsStatus[] newArray(int size) {
          return new PrivsStatus[size];
        }
      };

  public static class PermStatus implements Parcelable {

    public String name;
    public boolean granted;

    public PermStatus(String name, boolean granted) {
      this.name = name;
      this.granted = granted;
    }

    protected PermStatus(Parcel in) {
      name = in.readString();
      granted = in.readByte() != 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
      dest.writeString(name);
      dest.writeByte((byte) (granted ? 1 : 0));
    }

    public int describeContents() {
      return 0;
    }

    public static final Creator<PermStatus> CREATOR =
        new Creator<>() {

          public PermStatus createFromParcel(Parcel in) {
            return new PermStatus(in);
          }

          public PermStatus[] newArray(int size) {
            return new PermStatus[size];
          }
        };
  }
}
