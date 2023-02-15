package com.mirfatif.privdaemon;

import android.os.IBinder;
import com.mirfatif.privtasks.AppPrivTasks;

public class IPrivTasksFlavorImpl {

  public IPrivTasksFlavorImpl(AppPrivTasks.AppPrivTasksCallback obj) {}

  public void onDaemonStopped() {}

  public IBinder asBinder() {
    return null;
  }
}
