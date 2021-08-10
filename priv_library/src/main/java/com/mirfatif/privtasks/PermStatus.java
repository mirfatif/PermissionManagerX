package com.mirfatif.privtasks;

import java.io.Serializable;

public class PermStatus implements Serializable {

  private static final long serialVersionUID = 1234567890L;

  public String name;
  public boolean granted;

  public PermStatus(String name, boolean granted) {
    this.name = name;
    this.granted = granted;
  }
}
