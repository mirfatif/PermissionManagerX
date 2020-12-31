package com.mirfatif.permissionmanagerx.main;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.coordinatorlayout.widget.CoordinatorLayout;
import com.google.android.material.snackbar.Snackbar;
import com.mirfatif.permissionmanagerx.R;

// https://stackoverflow.com/questions/33217241
@Keep
public class MoveUpwardBehavior extends CoordinatorLayout.Behavior<View> {

  @SuppressWarnings("UnusedDeclaration")
  public MoveUpwardBehavior() {
    super();
  }

  @SuppressWarnings("UnusedDeclaration")
  public MoveUpwardBehavior(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  @Override
  public boolean layoutDependsOn(
      @NonNull CoordinatorLayout parent, @NonNull View child, @NonNull View dependency) {
    return dependency instanceof Snackbar.SnackbarLayout;
  }

  @Override
  public boolean onDependentViewChanged(
      @NonNull CoordinatorLayout parent, @NonNull View child, @NonNull View dependency) {

    float translationY = Math.min(0, dependency.getTranslationY() - dependency.getHeight());

    /**
     * Cancel if animation from {@link #onDependentViewRemoved(CoordinatorLayout, View, View)} is in
     * progress
     */
    child.findViewById(R.id.moving_container).animate().cancel();

    // Move the container up.
    child.findViewById(R.id.moving_container).setTranslationY(translationY);
    return true;
  }

  // Swipe the Snackbar.
  @Override
  public void onDependentViewRemoved(
      @NonNull CoordinatorLayout parent, @NonNull View child, @NonNull View dependency) {
    super.onDependentViewRemoved(parent, child, dependency);

    // Slowly move container down.
    child.findViewById(R.id.moving_container).animate().translationY(0).start();
  }
}
