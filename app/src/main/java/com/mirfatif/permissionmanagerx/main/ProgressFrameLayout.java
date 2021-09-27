package com.mirfatif.permissionmanagerx.main;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;

public class ProgressFrameLayout extends FrameLayout {

  public ProgressFrameLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  private Runnable mVisibilityTask;
  private long mLastCall;

  // setVisibility() on RoundProgressBarContainer is called from multiple places.
  // Too quick calls cause progress bar to hang. Here we rate limit it.
  @Override
  public synchronized void setVisibility(int visibility) {
    if (mKeepVisible && visibility != VISIBLE) {
      if (mListener != null) {
        mListener.visibilityChanged(visibility);
      }
      return;
    }

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

  private boolean mKeepVisible = false;

  @SuppressWarnings("UnusedDeclaration")
  public void setKeepVisible(boolean keepVisible) {
    mKeepVisible = keepVisible;
    if (keepVisible) {
      setVisibility(VISIBLE);
    }
  }

  @Override
  protected synchronized void onVisibilityChanged(View changedView, int visibility) {
    super.onVisibilityChanged(changedView, visibility);
    if (mListener != null && changedView == this) {
      mListener.visibilityChanged(visibility);
    }
  }

  private VisibilityChangeListener mListener;

  @SuppressWarnings("UnusedDeclaration")
  public synchronized void setOnVisibilityChangeListener(VisibilityChangeListener listener) {
    mListener = listener;
  }

  public interface VisibilityChangeListener {
    void visibilityChanged(int visibility);
  }
}
