package com.mirfatif.permissionmanagerx.util;

import androidx.annotation.NonNull;
import androidx.collection.ArraySet;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;

/*
 https://github.com/hadilq/LiveEvent
 Send data just once to an Observer, not on subsequent calls i.e. behave
 like a fresh instance of LiveData e.g. after configuration changes.
 As a side-effect, new Observers will also not receive previously set/posted data.
*/
public class LiveEvent<T> extends MutableLiveData<T> {

  private final ArraySet<ObserverWrapper<? super T>> mObservers = new ArraySet<>();

  @Override
  public void observe(@NonNull LifecycleOwner owner, @NonNull Observer<? super T> observer) {
    if (isNewObserver(observer)) {
      ObserverWrapper<T> wrapper = new ObserverWrapper<>(observer);
      mObservers.add(wrapper);
      super.observe(owner, wrapper);
    }
  }

  @Override
  public void observeForever(@NonNull Observer<? super T> observer) {
    if (isNewObserver(observer)) {
      ObserverWrapper<T> wrapper = new ObserverWrapper<>(observer);
      mObservers.add(wrapper);
      super.observeForever(wrapper);
    }
  }

  private boolean isNewObserver(Observer<? super T> observer) {
    for (ObserverWrapper<? super T> wrapper : mObservers) {
      if (wrapper.observer == observer) {
        return false;
      }
    }
    return true;
  }

  /*
   No need to override the removeObservers(LifecycleOwner)
   method as it calls this method under the hood.

   wrapper == observer covers the situation when called from inside LiveData
   on LifecycleOwner destroyed. The latter condition covers the situation
   when called from outside.
  */
  @Override
  public void removeObserver(@NonNull Observer<? super T> observer) {
    for (ObserverWrapper<? super T> wrapper : new ArraySet<>(mObservers)) {
      if (wrapper == observer || wrapper.observer == observer) {
        mObservers.remove(wrapper);
        super.removeObserver(wrapper);
      }
    }
  }

  /*
   No need to override the postValue(T) method as it calls this method under the hood.
  */
  @Override
  public void setValue(T value) {
    mObservers.forEach(ObserverWrapper::setPending);
    super.setValue(value);
  }

  private static class ObserverWrapper<T> implements Observer<T> {

    private final Observer<? super T> observer;

    private ObserverWrapper(Observer<? super T> observer) {
      this.observer = observer;
    }

    private boolean pending = false;

    @Override
    public void onChanged(T t) {
      if (pending) {
        pending = false;
        observer.onChanged(t);
      }
    }

    private void setPending() {
      pending = true;
    }
  }
}
