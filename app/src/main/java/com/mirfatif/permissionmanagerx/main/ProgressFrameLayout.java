package com.mirfatif.permissionmanagerx.main;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class ProgressFrameLayout extends FrameLayout {

  public ProgressFrameLayout(Context context) {
    super(context);
  }

  public ProgressFrameLayout(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  public ProgressFrameLayout(Context context, AttributeSet attrs, int defStyleAttr) {
    super(context, attrs, defStyleAttr);
  }

  @SuppressWarnings("UnusedDeclaration")
  public ProgressFrameLayout(
      Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
    super(context, attrs, defStyleAttr, defStyleRes);
  }

  private final ExecutorService mVisibilityExecutor = Executors.newSingleThreadExecutor();
  private Future<?> mVisibilityFuture;

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
