package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.util.AttributeSet;

public class ProgressLinearLayout extends MyLinearLayout {

  public ProgressLinearLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  private Runnable mVisibilityTask;
  private long mLastCall;

  public synchronized void setVisibility(int visibility) {
    removeCallbacks(mVisibilityTask);
    mVisibilityTask =
        () -> {
          mLastCall = System.currentTimeMillis();
          super.setVisibility(visibility);
        };
    long sleepTime = 1000 + mLastCall - System.currentTimeMillis();
    if (sleepTime > 10) {
      postDelayed(mVisibilityTask, sleepTime);
    } else {
      mVisibilityTask.run();
    }
  }
}
