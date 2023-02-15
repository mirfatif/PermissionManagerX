package com.mirfatif.privtasks;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.SOURCE)
public @interface PrivTasksError {

  int OP_NUM_INCONSISTENCY = 0;

  int OP_MODE_INCONSISTENCY = 1;

  int APP_OPS_IMPL = 2;
}
