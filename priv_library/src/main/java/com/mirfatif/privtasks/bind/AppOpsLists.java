package com.mirfatif.privtasks.bind;

import android.os.Parcel;
import android.os.Parcelable;
import com.mirfatif.privtasks.util.Util;
import java.util.List;
import java.util.Objects;

public class AppOpsLists implements Parcelable {

  public final List<String> appOpsNames;

  public final List<String> appOpsModes;

  public final List<Integer> opDefModeList;

  public final List<Integer> opSwitchList;

  public final StrIntMap permToOpMap;

  public AppOpsLists(
      List<String> appOpsNames,
      List<String> appOpsModes,
      List<Integer> opDefModeList,
      List<Integer> opSwitchList,
      StrIntMap permToOpMap) {
    this.appOpsNames = appOpsNames;
    this.appOpsModes = appOpsModes;
    this.opDefModeList = opDefModeList;
    this.opSwitchList = opSwitchList;
    this.permToOpMap = permToOpMap;
  }

  protected AppOpsLists(Parcel in) {
    appOpsNames = Objects.requireNonNull(in.createStringArrayList());
    appOpsModes = Objects.requireNonNull(in.createStringArrayList());
    opDefModeList = Util.getList(in.createIntArray());
    opSwitchList = Util.getList(in.createIntArray());
    permToOpMap = Objects.requireNonNull(in.readTypedObject(StrIntMap.CREATOR));
  }

  public static final Creator<AppOpsLists> CREATOR =
      new Creator<>() {
        public AppOpsLists createFromParcel(Parcel in) {
          return new AppOpsLists(in);
        }

        public AppOpsLists[] newArray(int size) {
          return new AppOpsLists[size];
        }
      };

  public int describeContents() {
    return 0;
  }

  public void writeToParcel(Parcel dest, int flags) {
    dest.writeStringList(appOpsNames);
    dest.writeStringList(appOpsModes);
    dest.writeIntArray(Util.getArray(opDefModeList));
    dest.writeIntArray(Util.getArray(opSwitchList));
    dest.writeTypedObject(permToOpMap, 0);
  }
}
