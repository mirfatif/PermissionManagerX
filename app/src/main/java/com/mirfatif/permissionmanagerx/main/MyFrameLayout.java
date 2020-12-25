package com.mirfatif.permissionmanagerx.main;

import android.content.Context;
import android.os.SystemClock;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;
import com.mirfatif.permissionmanagerx.Utils;

public class MyFrameLayout extends FrameLayout {

  public MyFrameLayout(Context context) {
    super(context);
  }

  public MyFrameLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  public MyFrameLayout(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
  }

  @SuppressWarnings("UnusedDeclaration")
  public MyFrameLayout(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
    super(context, attrs, defStyleAttr, defStyleRes);
  }

  // setVisibility() on RoundProgressBarContainer is called from multiple places.
  // Too quick calls cause progress bar to hang. Here we rate limit it.
  @Override
  public void setVisibility(int visibility) {
    long myId = mRefId = System.nanoTime();
    Utils.runInBg(() -> setVisibilityInBg(visibility, myId));
  }

  private long mRefId, lastCall;

  private synchronized void setVisibilityInBg(int visibility, long myId) {
    long sleepTime = 1000 + lastCall - System.currentTimeMillis();
    if (sleepTime > 0) SystemClock.sleep(sleepTime);

    // we have new call waiting
    if (myId != mRefId) return;

    lastCall = System.currentTimeMillis();
    Utils.runInFg(() -> super.setVisibility(visibility));
  }

  @Override
  protected void onVisibilityChanged(View changedView, int visibility) {
    super.onVisibilityChanged(changedView, visibility);
    if (mListener != null && changedView == this) {
      mListener.visibilityChanged(visibility);
    }
  }

  private VisibilityChangeListener mListener;

  @SuppressWarnings("UnusedDeclaration")
  public void setOnVisibilityChangeListener(VisibilityChangeListener listener) {
    mListener = listener;
  }

  public interface VisibilityChangeListener {
    void visibilityChanged(int visibility);
  }
}
