package com.mirfatif.permissionmanagerx.fwk;

import android.content.Context;
import android.graphics.Color;
import android.util.AttributeSet;
import androidx.activity.ComponentActivity;
import androidx.appcompat.widget.AppCompatImageView;
import com.mirfatif.permissionmanagerx.util.bg.LiveTasksQueue;

public class DialogHelpIcon extends AppCompatImageView {

  public DialogHelpIcon(Context context, AttributeSet attrs) {
    super(context, attrs);

    post(
        () ->
            new LiveTasksQueue((ComponentActivity) context, 3000)
                .onUi(() -> setColorFilter(Color.RED))
                .delay(250)
                .onUi(() -> setColorFilter(Color.TRANSPARENT))
                .delay(250)
                .onUi(this::clearColorFilter)
                .delay(250)
                .onUi(() -> setColorFilter(Color.RED))
                .delay(250)
                .onUi(() -> setColorFilter(Color.TRANSPARENT))
                .delay(250)
                .onUi(this::clearColorFilter)
                .start());
  }
}
