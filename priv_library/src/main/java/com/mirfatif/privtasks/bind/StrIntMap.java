package com.mirfatif.privtasks.bind;

import android.os.Parcel;
import android.os.Parcelable;
import android.util.ArrayMap;

public class StrIntMap implements Parcelable {

  public final ArrayMap<String, Integer> map = new ArrayMap<>();

  public StrIntMap() {}

  protected StrIntMap(Parcel in) {
    int size = in.readInt();
    for (int i = 0; i < size; i++) {
      map.put(in.readString(), in.readInt());
    }
  }

  public void writeToParcel(Parcel dest, int flags) {
    int size = map.size();
    dest.writeInt(size);
    for (int i = 0; i < size; i++) {
      dest.writeString(map.keyAt(i));
      dest.writeInt(map.valueAt(i));
    }
  }

  public int describeContents() {
    return 0;
  }

  public static final Creator<StrIntMap> CREATOR =
      new Creator<>() {

        public StrIntMap createFromParcel(Parcel in) {
          return new StrIntMap(in);
        }

        public StrIntMap[] newArray(int size) {
          return new StrIntMap[size];
        }
      };
}
