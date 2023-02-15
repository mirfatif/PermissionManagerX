package com.mirfatif.privtasks.bind;

import com.mirfatif.privtasks.bind.DaemonState;

parcelable DaemonState;

interface IPrivTasksCallback {

  void hello(in DaemonState daemonState);

  void showError(int privTasksError);

  void saveLog(String stackTrace);
}
