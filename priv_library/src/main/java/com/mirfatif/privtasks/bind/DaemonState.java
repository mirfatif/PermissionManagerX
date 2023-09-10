package com.mirfatif.privtasks.bind;

import android.os.IBinder;
import android.os.IBinder.DeathRecipient;
import android.os.Parcel;
import android.os.Parcelable;
import com.mirfatif.privtasks.iface.IPrivTasks;

public class DaemonState implements Parcelable {

  public final int pid;

  public final int uid;

  public final String context;

  public final int port;

  public final IBinder privTasksFlavor;

  public DaemonState(int pid, int uid, String context, int port, IBinder privTasksFlavor) {
    this.pid = pid;
    this.uid = uid;
    this.context = context;
    this.port = port;
    this.privTasksFlavor = privTasksFlavor;
  }

  public IPrivTasks privTasks;
  public DeathRecipient deathRecipient;

  protected DaemonState(Parcel in) {
    pid = in.readInt();
    uid = in.readInt();
    context = in.readString();
    port = in.readInt();
    privTasksFlavor = in.readStrongBinder();
  }

  public void writeToParcel(Parcel dest, int flags) {
    dest.writeInt(pid);
    dest.writeInt(uid);
    dest.writeString(context);
    dest.writeInt(port);
    dest.writeStrongBinder(privTasksFlavor);
  }

  public int describeContents() {
    return 0;
  }

  public static final Creator<DaemonState> CREATOR =
      new Creator<>() {

        public DaemonState createFromParcel(Parcel in) {
          return new DaemonState(in);
        }

        public DaemonState[] newArray(int size) {
          return new DaemonState[size];
        }
      };
}
