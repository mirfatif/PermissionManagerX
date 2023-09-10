package com.mirfatif.permissionmanagerx.main;

import android.animation.Animator;
import android.animation.ValueAnimator;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.DecelerateInterpolator;
import android.widget.FrameLayout;
import androidx.core.view.GravityCompat;
import androidx.drawerlayout.widget.DrawerLayout;
import androidx.drawerlayout.widget.DrawerLayout.SimpleDrawerListener;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.fwk.LifecycleWatcher;
import com.mirfatif.permissionmanagerx.util.UiUtilsFlavor;
import com.mirfatif.permissionmanagerx.util.bg.LiveSchedTask;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class PrivsCheckBoxFocus {

  private static final String TAG = "PrivsCheckBoxFocus";

  private final MainActivity mA;

  PrivsCheckBoxFocus(MainActivity act) {
    mA = act;
  }

  private final AtomicReference<AnimationFocus> mFocus = new AtomicReference<>();

  void doFocus() {
    synchronized (mFocus) {
      endFocusUnlocked();
      mFocus.set(new AnimationFocus());
    }
  }

  void endFocus() {
    synchronized (mFocus) {
      endFocusUnlocked();
    }
  }

  private void endFocusUnlocked() {
    AnimationFocus focus = mFocus.getAndSet(null);
    if (focus != null) {
      focus.end();
    }
  }

  private class AnimationFocus {

    private final DrawerLayout mDrawerLayout = mA.mB.getRoot();
    private final ViewGroup mDecorView = (ViewGroup) mA.mA.getWindow().getDecorView();

    private final DrawerListener mDrawerListener = new DrawerListener();
    private final Overlay mOverlay = new Overlay();
    private final ValueAnimator mAnim = ValueAnimator.ofFloat(0, 0.8f);

    private AnimationFocus() {
      if (mDrawerLayout.isOpen()) {
        mDrawerListener.onDrawerOpened(null);
      } else {
        mDrawerLayout.addDrawerListener(mDrawerListener);
        mDrawerLayout.openDrawer(GravityCompat.START);
      }
    }

    private void end() {
      mDrawerLayout.removeDrawerListener(mDrawerListener);
      mAnim.cancel();
      mDecorView.removeView(mOverlay);
    }

    private class DrawerListener extends SimpleDrawerListener {

      private boolean drawerOpened = false;

      public synchronized void onDrawerOpened(View drawerView) {
        if (drawerOpened) {
          return;
        }

        drawerOpened = true;

        scrollToCheckBoxes();
        showOverlayDelayed();
      }
    }

    private void scrollToCheckBoxes() {
      View v = mA.mB.navV.getChildAt(0);

      if (!(v instanceof RecyclerView rv)) {
        return;
      }

      RecyclerView.LayoutManager lm = rv.getLayoutManager();

      if (!(lm instanceof LinearLayoutManager llm)) {
        return;
      }

      int first = llm.findFirstCompletelyVisibleItemPosition();
      int last = llm.findLastCompletelyVisibleItemPosition();

      if (first == RecyclerView.NO_POSITION || last == RecyclerView.NO_POSITION) {
        return;
      }

      int rootPos = mA.mB.navV.getMenu().findItem(R.id.action_root).getOrder();
      int adbPos = mA.mB.navV.getMenu().findItem(R.id.action_adb).getOrder();

      if (adbPos > last) {
        rv.scrollToPosition(adbPos);
      } else if (rootPos < first) {
        rv.scrollToPosition(rootPos);
      }
    }

    private void showOverlayDelayed() {
      LiveSchedTask.schedule(
          mA.mA, this::showOverlay, 500, TimeUnit.MILLISECONDS, true, TAG + "-ShowOverlay");
    }

    private float mX, mY, mW, mH;

    private void showOverlay() {
      View view = mA.mB.navV.getMenu().findItem(R.id.action_root).getActionView();
      int[] loc = new int[2];

      Objects.requireNonNull(view).getLocationInWindow(loc);
      mX = loc[0];
      mY = loc[1];

      mW = view.getWidth();

      view = mA.mB.navV.getMenu().findItem(R.id.action_adb).getActionView();
      Objects.requireNonNull(view).getLocationInWindow(loc);

      mH = loc[1] + view.getHeight() - mY;

      mAnim.setDuration(1000);
      mAnim.setInterpolator(new DecelerateInterpolator(1.5f));
      mAnim.setRepeatCount(1);
      mAnim.setRepeatMode(ValueAnimator.REVERSE);

      mAnim.addUpdateListener(
          animation -> {
            if (!mDrawerLayout.isOpen()) {
              endFocus();
            } else {
              mOverlay.alpha = (float) animation.getAnimatedValue();
              mOverlay.invalidate();
            }
          });

      mAnim.addListener(
          new Animator.AnimatorListener() {

            public void onAnimationStart(Animator animation) {}

            public void onAnimationEnd(Animator animation) {
              endFocus();
            }

            public void onAnimationCancel(Animator animation) {}

            public void onAnimationRepeat(Animator animation) {}
          });

      LifecycleWatcher.addOnDestroyed(mA.mA, PrivsCheckBoxFocus.this::endFocus);
      mOverlay.setOnClickListener(v -> endFocus());

      if (!mDrawerLayout.isOpen()) {

        endFocus();
      } else {
        mDecorView.addView(mOverlay);
        mAnim.start();
      }
    }

    private class Overlay extends FrameLayout {

      public Overlay() {
        super(mA.mA);

        setLayoutParams(
            new ViewGroup.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));

        setWillNotDraw(false);
      }

      private float alpha = 0;
      private Bitmap b;
      private Canvas c;

      private final Paint accent = new Paint(Paint.ANTI_ALIAS_FLAG);
      private final Paint clear = new Paint(Paint.ANTI_ALIAS_FLAG);

      {
        accent.setStyle(Paint.Style.FILL);
        accent.setColor(UiUtilsFlavor.getAccentColor());

        clear.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.CLEAR));
      }

      protected void onDraw(Canvas canvas) {
        if (b == null) {
          int wid = getWidth();
          int ht = getHeight();

          if (wid <= 0 || ht <= 0) {
            return;
          }

          b = Bitmap.createBitmap(wid, ht, Bitmap.Config.ARGB_8888);
          c = new Canvas(b);
        }

        c.drawColor(Color.BLACK);

        float f = (1 - alpha) * mW * 2;

        float x = mX - f;
        float y = mY - f;
        float w = mW + 2 * f;
        float h = mH + 2 * f;
        float r = w / 4;

        c.drawRoundRect(x, y, x + w, y + h, r, r, accent);

        float xx = x;

        x = mX - f * 4 / 9;
        y = mY - f * 4 / 9;
        w = mW + f * 8 / 9;
        h = mH + f * 8 / 9;

        r = r - (x - xx) * 2 / 3;

        c.drawRoundRect(x, y, x + w, y + h, r, r, clear);

        canvas.drawBitmap(b, 0, 0, null);

        setAlpha(alpha);
      }
    }
  }
}
