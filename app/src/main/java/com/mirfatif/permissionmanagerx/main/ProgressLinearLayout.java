package com.mirfatif.permissionmanagerx.main;

import android.content.Context;
import android.util.AttributeSet;
import com.mirfatif.permissionmanagerx.ui.base.MyLinearLayout;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class ProgressLinearLayout extends MyLinearLayout {

  public ProgressLinearLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  private final ExecutorService mVisibilityExecutor = Executors.newSingleThreadExecutor();
  private Future<?> mVisibilityFuture;

  @Override
  public void setVisibility(int visibility) {
    if (mVisibilityFuture != null && !mVisibilityFuture.isDone()) {
      mVisibilityFuture.cancel(true);
    }
    mVisibilityFuture = mVisibilityExecutor.submit(() -> setVisibilityInBg(visibility));
  }

  private long lastCall;

  private void setVisibilityInBg(int visibility) {
    long sleepTime = 1000 + lastCall - System.currentTimeMillis();
    if (sleepTime > 0) {
      try {
        synchronized (mVisibilityExecutor) {
          mVisibilityExecutor.wait(sleepTime);
        }
      } catch (InterruptedException e) {
        return; // We've got a new call
      }
    }

    lastCall = System.currentTimeMillis();
    Utils.runInFg(() -> super.setVisibility(visibility));
  }
}
