package com.mirfatif.permissionmanagerx.main;

import android.content.Context;
import android.util.AttributeSet;
import com.mirfatif.permissionmanagerx.ui.base.MyLinearLayout;

public class ProgressLinearLayout extends MyLinearLayout {

  public ProgressLinearLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  private Runnable mVisibilityTask;
  private long mLastCall;

  @Override
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
