package com.mirfatif.permissionmanagerx.ui;

import static com.mirfatif.permissionmanagerx.parser.AppOpsParser.APP_OPS_PARSER;
import static com.mirfatif.permissionmanagerx.util.Utils.getString;

import android.content.Context;
import android.graphics.Color;
import android.text.style.TextAppearanceSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.TextView;
import androidx.annotation.ColorInt;
import androidx.annotation.NonNull;
import androidx.core.graphics.ColorUtils;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.RvItemPermBinding;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.ui.PermissionAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.ui.base.MyListAdapter;
import com.mirfatif.permissionmanagerx.util.Utils;
import com.mirfatif.privtasks.Commands;
import java.util.ArrayList;
import java.util.List;

public class PermissionAdapter extends MyListAdapter<Permission, ItemViewHolder> {

  private final PermAdapterCallback mCallback;
  private final ArrayAdapter<String> mAppOpModesAdapter;
  private final ArrayAdapter<String> mAppOpModesBgAdapter;

  PermissionAdapter(Context context, PermAdapterCallback callback) {
    super(new DiffUtilItemCallBack(), callback::runInFg);
    mCallback = callback;

    mAppOpModesAdapter = new AppOpModesAdapter(context, false);
    mAppOpModesBgAdapter = new AppOpModesAdapter(context, true);
  }

  // Override Adapter method
  // Inflate a View, create and return a ViewHolder
  @NonNull
  @Override
  public ItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
    LayoutInflater inflater = LayoutInflater.from(parent.getContext());
    RvItemPermBinding b = RvItemPermBinding.inflate(inflater, parent, false);
    return new ItemViewHolder(b);
  }

  // Override RecyclerView.Adapter method
  // set contents in Views
  @Override
  public void onBindViewHolder(@NonNull ItemViewHolder holder, int position) {
    holder.bind(position);
  }

  // Store and recycle items as they are scrolled off screen
  class ItemViewHolder extends RecyclerView.ViewHolder
      implements OnClickListener, OnLongClickListener {

    private final RvItemPermBinding mB;

    public ItemViewHolder(RvItemPermBinding binding) {
      super(binding.getRoot());
      mB = binding;
      binding.getRoot().setOnClickListener(this);
      binding.getRoot().setOnLongClickListener(this);
    }

    public void bind(int position) {
      Permission perm = getItem(position);

      mB.appOpsRefStateV.setVisibility(View.GONE);
      if (perm.isReferenced() == null) {
        mB.refIndicationV.setBackgroundColor(PackageAdapter.ORANGE);
      } else if (!perm.isReferenced()) {
        mB.refIndicationV.setBackgroundColor(Color.RED);
        if (perm.isAppOps()) {
          String state = getLocalizedMode(perm.getReference());
          if (state == null) {
            state = perm.getReference();
          }
          mB.appOpsRefStateV.setText(Utils.htmlToString(getString(R.string.should_be, state)));
          mB.appOpsRefStateV.setVisibility(View.VISIBLE);
        }
      } else {
        mB.refIndicationV.setBackgroundColor(Color.GREEN);
      }

      if (perm.getIconResId() != null) {
        mB.iconV.setImageResource(perm.getIconResId());
      }

      mB.permStateSpinnerCont.setOnClickListener(null);

      if (perm.isAppOps()) {
        if (perm.dependsOn() == null) {
          mB.permStateSpinnerCont.setVisibility(View.VISIBLE);
          mB.permStateSpinnerCont.setOnClickListener(v -> mB.permStateSpinner.performClick());
        } else {
          mB.permStateSpinnerCont.setVisibility(View.INVISIBLE);
        }
      }

      mB.permNameV.setText(perm.getPermNameString());
      mB.appOpsTimeV.setVisibility(View.GONE);

      boolean isChangeable = perm.isChangeable();

      if (perm.isCritical() && isChangeable) {
        mB.protLevelV.setText(
            Utils.getHighlightString(
                perm.getProtLevelString(),
                getHighlightSpan(mB.protLevelV.getCurrentTextColor()),
                true,
                getString(R.string.prot_lvl_fixed),
                getString(R.string.prot_lvl_privileged)));
      } else {
        mB.protLevelV.setText(perm.getProtLevelString());
      }

      if (perm.isAppOps()) {
        String time = perm.getAppOpsAccessTime();
        if (time != null) {
          mB.appOpsTimeV.setText(time);
          mB.appOpsTimeV.setTextColor(getRedBlend(mB.appOpsTimeV.getCurrentTextColor()));
          mB.appOpsTimeV.setVisibility(View.VISIBLE);
        }

        if (perm.getName().equals("RUN_IN_BACKGROUND")
            || perm.getName().equals("RUN_ANY_IN_BACKGROUND")) {
          mB.permStateSpinner.setAdapter(getAppOpModesAdapter(true));
        } else {
          mB.permStateSpinner.setAdapter(getAppOpModesAdapter(false));
        }

        mB.permStateSpinner.setSelection(perm.getAppOpsMode());
        mB.permStateSpinner.setOnItemSelectedListener(new AppOpsModeSelectListener(perm));
        mB.permStateSwitch.setVisibility(View.GONE);
        mB.permStateSpinner.setEnabled(isChangeable);
        mB.appOpsDefaultV.setVisibility(perm.isAppOpsSet() ? View.GONE : View.VISIBLE);

      } else {
        if (perm.isProviderMissing()) {
          mB.permStateSwitch.setVisibility(View.INVISIBLE);
        } else {
          mB.permStateSwitch.setChecked(perm.isGranted());
          mB.permStateSwitch.setEnabled(isChangeable);
          mB.permStateSwitch.setOnClickListener(
              v -> {
                mB.permStateSwitch.setChecked(perm.isGranted()); // Do not change the state here
                mCallback.onPermSwitchClick(perm);
              });
          mB.permStateSwitch.setVisibility(View.VISIBLE);
        }
        mB.permStateSpinnerCont.setVisibility(View.GONE);
      }
    }

    @Override
    public void onClick(View v) {
      int pos = getBindingAdapterPosition();
      if (pos != RecyclerView.NO_POSITION) {
        int[] location = new int[2];
        v.getLocationInWindow(location);
        mCallback.onPermClick(getItem(pos), location[1] - 2 * v.getHeight());
      }
    }

    @Override
    public boolean onLongClick(View v) {
      int pos = getBindingAdapterPosition();
      if (pos != RecyclerView.NO_POSITION) {
        mCallback.onPermLongClick(getItem(pos));
      }
      return true;
    }
  }

  private TextAppearanceSpan HIGHLIGHT;

  private TextAppearanceSpan getHighlightSpan(@ColorInt int currentColor) {
    if (HIGHLIGHT == null) {
      HIGHLIGHT = Utils.getHighlight(getRedBlend(currentColor));
    }
    return HIGHLIGHT;
  }

  private @ColorInt Integer mOrigTextColor;

  private @ColorInt int getRedBlend(@ColorInt int currentColor) {
    if (mOrigTextColor == null) {
      mOrigTextColor = currentColor;
    }
    return ColorUtils.blendARGB(mOrigTextColor, Color.RED, 0.75f);
  }

  private String getLocalizedMode(String appOpMode) {
    switch (appOpMode.toLowerCase()) {
      case Commands.APP_OP_MODE_ALLOW:
        return getString(R.string.app_op_mode_allow);
      case Commands.APP_OP_MODE_IGNORE:
        return getString(R.string.app_op_mode_ignore);
      case Commands.APP_OP_MODE_DENY:
        return getString(R.string.app_op_mode_deny);
      case Commands.APP_OP_MODE_DEFAULT:
        return getString(R.string.app_op_mode_default);
      case Commands.APP_OP_MODE_FG:
        return getString(R.string.app_op_mode_foreground);
    }
    return null;
  }

  private static final Object ADAPTER_BUILD_LOCK = new Object();

  private ArrayAdapter<String> getAppOpModesAdapter(boolean forBg) {
    synchronized (ADAPTER_BUILD_LOCK) {
      if (mAppOpModesAdapter.isEmpty() || mAppOpModesBgAdapter.isEmpty()) {
        for (String mode : APP_OPS_PARSER.getAppOpsModes()) {
          String localizedMode = getLocalizedMode(mode);
          if (localizedMode != null) {
            mode = localizedMode;
            if (mode.equalsIgnoreCase(Commands.APP_OP_MODE_FG)) {
              mode = Utils.ellipsize(mode, 8);
            }
          } else {
            mode = Utils.ellipsize(mode, 8);
          }
          mAppOpModesAdapter.add(mode);
          mAppOpModesBgAdapter.add(mode);
        }
      }
    }
    return forBg ? mAppOpModesBgAdapter : mAppOpModesAdapter;
  }

  private static class AppOpModesAdapter extends ArrayAdapter<String> {

    private final List<String> appOpsModes = APP_OPS_PARSER.getAppOpsModes();
    private final boolean mForBg;

    public AppOpModesAdapter(Context context, boolean forBg) {
      super(context, android.R.layout.simple_spinner_item, new ArrayList<>());
      setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
      mForBg = forBg;
    }

    @Override
    public boolean areAllItemsEnabled() {
      return !mForBg;
    }

    @Override
    public boolean isEnabled(int position) {
      if (mForBg) {
        if (appOpsModes.get(position).equals("Foreground")) {
          return false;
        }
      }
      return super.isEnabled(position);
    }

    // Cannot use color selector here for TextView. It only honors states: "normal",
    // "selected" and "focused", not "disabled".
    @Override
    public View getDropDownView(int position, View convertView, @NonNull ViewGroup parent) {
      View view = super.getDropDownView(position, convertView, parent);

      if (mForBg) {
        if (appOpsModes.get(position).equals("Foreground")) {
          // Find TextView from android.R.layout.simple_spinner_dropdown_item.
          TextView tView = view.findViewById(android.R.id.text1);
          // Check null to avoid broken behavior on different Android versions.
          if (tView != null) {
            tView.setTextColor(App.getContext().getColor(R.color.disabledStateColor));
          }
        }
      }

      return view;
    }
  }

  private class AppOpsModeSelectListener implements AdapterView.OnItemSelectedListener {

    private final Permission permission;

    AppOpsModeSelectListener(Permission permission) {
      this.permission = permission;
    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
      // "position" is the AppOps mode int value here
      if (permission.getAppOpsMode() != position) {
        permission.setAppOpsMode(position);
        mCallback.onSpinnerItemSelect(permission, position);
      }
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {}
  }

  private static class DiffUtilItemCallBack extends DiffUtil.ItemCallback<Permission> {
    @Override
    public boolean areItemsTheSame(@NonNull Permission oldItem, @NonNull Permission newItem) {
      return oldItem.getName().equals(newItem.getName());
    }

    @Override
    public boolean areContentsTheSame(@NonNull Permission oldItem, @NonNull Permission newItem) {
      return oldItem.areContentsTheSame(newItem);
    }
  }

  public interface PermAdapterCallback {

    void onPermClick(Permission perm, Integer yLocation);

    void onPermLongClick(Permission perm);

    void onPermSwitchClick(Permission perm);

    void onSpinnerItemSelect(Permission perm, int selectedValue);

    void runInFg(Runnable task);
  }
}
