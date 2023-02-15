package com.mirfatif.permissionmanagerx.util.bg;

import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class LiveEvent<T> extends MutableLiveData<T> {

  private final boolean mOneshot;

  public LiveEvent(boolean oneshot) {
    mOneshot = oneshot;
  }

  private final Map<Integer, ObserverWrapper<? super T>> mObservers =
      Collections.synchronizedMap(new HashMap<>());

  public void observe(LifecycleOwner owner, Observer<? super T> observer) {
    if (isNewObserver(observer)) {
      ObserverWrapper<T> wrapper = new ObserverWrapper<>(observer, mOneshot);
      mObservers.put(observer.hashCode(), wrapper);
      super.observe(owner, wrapper);
    }
  }

  public void observeForever(Observer<? super T> observer) {
    if (isNewObserver(observer)) {
      ObserverWrapper<T> wrapper = new ObserverWrapper<>(observer, mOneshot);
      mObservers.put(observer.hashCode(), wrapper);
      super.observeForever(wrapper);
    }
  }

  private boolean isNewObserver(Observer<? super T> observer) {
    return !mObservers.containsKey(observer.hashCode());
  }

  public void removeObserver(Observer<? super T> observer) {
    for (Map.Entry<Integer, ObserverWrapper<? super T>> entry :
        new HashSet<>(mObservers.entrySet())) {
      ObserverWrapper<? super T> wrapper = entry.getValue();
      if (wrapper == observer || wrapper.observer == observer) {
        mObservers.remove(entry.getKey());
        super.removeObserver(wrapper);
      }
    }
  }

  public void setValue(T value) {
    setValue(value, false);
  }

  public void setValue(T value, boolean mustSend) {
    for (ObserverWrapper<? super T> wrapper : new HashSet<>(mObservers.values())) {
      if (mustSend) {
        wrapper.addMustValue(value);
      } else {
        wrapper.setPending();
      }
    }
    super.setValue(value);
  }

  public void postValue(T value, boolean mustSend) {
    if (mustSend) {
      new HashSet<>(mObservers.values()).forEach(wrapper -> wrapper.addMustValue(value));
    }
    super.postValue(value);
  }

  private static class ObserverWrapper<T> implements Observer<T> {

    private final boolean oneshot;
    private final Observer<? super T> observer;

    private ObserverWrapper(Observer<? super T> observer, boolean oneshot) {
      this.observer = observer;
      this.oneshot = oneshot;
    }

    private boolean pending = false;

    private void setPending() {
      synchronized (this) {
        pending = true;
      }
    }

    private final Set<T> mustValues = Collections.synchronizedSet(new LinkedHashSet<>());

    private void addMustValue(T value) {
      synchronized (this) {
        pending = true;
        mustValues.add(value);
      }
    }

    public void onChanged(T t) {
      synchronized (this) {
        if (!oneshot || pending) {
          pending = false;

          if (!mustValues.isEmpty()) {
            mustValues.remove(t);
            mustValues.forEach(observer::onChanged);
          }

          observer.onChanged(t);
        }
      }
    }
  }
}
