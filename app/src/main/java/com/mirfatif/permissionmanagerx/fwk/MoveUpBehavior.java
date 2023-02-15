package com.mirfatif.permissionmanagerx.fwk;

import android.view.View;
import androidx.coordinatorlayout.widget.CoordinatorLayout;

public class MoveUpBehavior extends CoordinatorLayout.Behavior<View> {

  private final Class<?> mDependency;
  private final View mChild;

  public MoveUpBehavior(Class<?> dependency, View child) {
    mDependency = dependency;
    mChild = child;
  }

  public boolean layoutDependsOn(CoordinatorLayout parent, View child, View dependency) {
    return mDependency.isInstance(dependency);
  }

  public boolean onDependentViewChanged(CoordinatorLayout parent, View child, View dependency) {

    float translationY = Math.min(0, dependency.getTranslationY() - dependency.getHeight());

    mChild.animate().cancel();

    mChild.setTranslationY(translationY);
    return true;
  }

  public void onDependentViewRemoved(CoordinatorLayout parent, View child, View dependency) {
    super.onDependentViewRemoved(parent, child, dependency);

    mChild.animate().translationY(0).start();
  }
}
