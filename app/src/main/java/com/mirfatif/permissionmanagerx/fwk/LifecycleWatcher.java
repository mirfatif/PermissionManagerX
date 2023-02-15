package com.mirfatif.permissionmanagerx.fwk;

import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.Lifecycle.Event;
import androidx.lifecycle.Lifecycle.State;
import androidx.lifecycle.LifecycleEventObserver;
import androidx.lifecycle.LifecycleOwner;
import com.mirfatif.permissionmanagerx.util.bg.LiveUiWaitTask;
import com.mirfatif.privtasks.util.bg.ThreadUtils;

public class LifecycleWatcher {

  public static LifecycleWatcher addOnDestroyed(LifecycleOwner owner, LifecycleCallback callback) {
    return new LifecycleWatcher(owner, callback);
  }

  private final LifecycleEventObserver mObserver = new Observer();
  private final LifecycleCallback mCallback;

  private LifecycleWatcher(LifecycleOwner owner, LifecycleCallback callback) {
    mCallback = callback;
    if (ThreadUtils.isMainThread()) {
      addObserver(owner.getLifecycle());
    } else {
      LiveUiWaitTask.post(() -> addObserver(owner.getLifecycle())).waitForMe();
    }
  }

  private void addObserver(Lifecycle lifecycle) {
    lifecycle.addObserver(mObserver);
    if (lifecycle.getCurrentState() == State.DESTROYED) {
      onDestroyed(lifecycle);
    }
  }

  private void onDestroyed(Lifecycle lifecycle) {
    mCallback.onDestroyed();
    lifecycle.removeObserver(mObserver);
  }

  private class Observer implements LifecycleEventObserver {

    public void onStateChanged(LifecycleOwner source, Event event) {
      if (event == Event.ON_DESTROY) {
        onDestroyed(source.getLifecycle());
      }
    }
  }

  public interface LifecycleCallback {

    void onDestroyed();
  }
}
