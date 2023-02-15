package com.mirfatif.privtasks.bind;

import android.os.Parcel;
import android.os.Parcelable;

public class PermFixedFlags implements Parcelable {

  public final int systemFixed;

  public final int policyFixed;

  public PermFixedFlags(int systemFixed, int policyFixed) {
    this.systemFixed = systemFixed;
    this.policyFixed = policyFixed;
  }

  protected PermFixedFlags(Parcel in) {
    systemFixed = in.readInt();
    policyFixed = in.readInt();
  }

  public static final Creator<PermFixedFlags> CREATOR =
      new Creator<PermFixedFlags>() {

        public PermFixedFlags createFromParcel(Parcel in) {
          return new PermFixedFlags(in);
        }

        public PermFixedFlags[] newArray(int size) {
          return new PermFixedFlags[size];
        }
      };

  public int describeContents() {
    return 0;
  }

  public void writeToParcel(Parcel dest, int flags) {
    dest.writeInt(systemFixed);
    dest.writeInt(policyFixed);
  }
}
