package com.mirfatif.privdaemon;

import com.mirfatif.privtasks.PrivTasks;

public class DaemonFlavor {

  @SuppressWarnings("UnusedDeclaration")
  public DaemonFlavor(Daemon daemon, PrivTasks privTasks) {}

  @SuppressWarnings("UnusedDeclaration")
  public boolean handleCommand(String[] args) {
    return false;
  }

  public void onExit() {}
}
