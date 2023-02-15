package com.mirfatif.permissionmanagerx.base;

import androidx.lifecycle.LifecycleOwner;
import androidx.recyclerview.widget.DiffUtil.ItemCallback;
import androidx.recyclerview.widget.ListAdapter;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.util.bg.LiveSingleParamTask;
import com.mirfatif.permissionmanagerx.util.bg.LiveUiParamTask;
import com.mirfatif.privtasks.util.bg.NotifyWaiter;
import com.mirfatif.privtasks.util.bg.RateLimiter;
import java.util.List;
import java.util.concurrent.TimeUnit;

public abstract class MyListAdapter<T, VH extends RecyclerView.ViewHolder>
    extends ListAdapter<T, VH> {

  protected MyListAdapter(ItemCallback<T> diffCallback, LifecycleOwner owner, String tag) {
    super(diffCallback);
    mBgListSubmitter =
        new LiveSingleParamTask<>(owner, this::submitListAndWait, tag + "-ListSubmitter");
    mUiListSubmitter = new LiveUiParamTask<>(owner, this::submitListOnUi);
  }

  protected T getItem(int position) {
    try {
      return super.getItem(position);
    } catch (IndexOutOfBoundsException ignored) {
      return null;
    }
  }

  private final LiveSingleParamTask<List<T>> mBgListSubmitter;

  public synchronized void submitList(List<T> list) {
    mBgListSubmitter.cancelAndSubmit(list, false);
  }

  private final LiveUiParamTask<List<T>> mUiListSubmitter;

  private final NotifyWaiter mCommitWaiter = new NotifyWaiter(2, TimeUnit.SECONDS);

  private final RateLimiter mListSubmitLimiter = new RateLimiter(1, TimeUnit.SECONDS);

  private void submitListAndWait(List<T> list) {
    mUiListSubmitter.post(list, true);
    mCommitWaiter.waitForNotifyNoThrow();
    mListSubmitLimiter.waitUntilCanNoThrow(true);
  }

  private final Runnable mWaitEnder = () -> mCommitWaiter.notify(true);

  private void submitListOnUi(List<T> list) {
    super.submitList(list, mWaitEnder);
  }
}
