package com.mirfatif.permissionmanagerx.ui.base;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.DiffUtil.ItemCallback;
import androidx.recyclerview.widget.ListAdapter;
import androidx.recyclerview.widget.RecyclerView;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public abstract class MyListAdapter<T, VH extends RecyclerView.ViewHolder>
    extends ListAdapter<T, VH> {

  private final ListAdapterCallback mCallback;

  protected MyListAdapter(@NonNull ItemCallback<T> diffCallback, ListAdapterCallback callback) {
    super(diffCallback);
    mCallback = callback;
  }

  /*
    We rely on submitList() to keep track of UI changes in Package Objects because new Objects are
    not always created and hence we cannot compare two Package Objects to determine UI changes.
    Here we ensure that new list is not submitted to RecyclerView until the previous one is not
    committed. When being committed, the Package Objects in the list record their UI state which
    can later be compared in areContentsTheSame().
  */
  private final ExecutorService mSubmitListExecutor = Executors.newSingleThreadExecutor();
  private Future<?> mSubmitListFuture;

  @Override
  public synchronized void submitList(@Nullable List<T> list) {
    if (mSubmitListFuture != null && !mSubmitListFuture.isDone()) {
      // Cancel if execution not started yet
      mSubmitListFuture.cancel(false);
    }
    mSubmitListFuture = mSubmitListExecutor.submit(() -> submitListAndWait(list));
  }

  private boolean mIsCommittingList = false;

  private void submitListAndWait(List<T> list) {
    mIsCommittingList = true;
    mCallback.runInFg(() -> super.submitList(list, this::endWait));

    synchronized (mSubmitListExecutor) {
      while (mIsCommittingList) {
        try {
          mSubmitListExecutor.wait();
        } catch (InterruptedException ignored) {
        }
      }
    }
  }

  private void endWait() {
    synchronized (mSubmitListExecutor) {
      mIsCommittingList = false;
      mSubmitListExecutor.notifyAll();
    }
  }

  public interface ListAdapterCallback {

    void runInFg(Runnable task);
  }
}
