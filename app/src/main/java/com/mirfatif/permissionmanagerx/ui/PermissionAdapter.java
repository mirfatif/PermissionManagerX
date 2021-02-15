package com.mirfatif.permissionmanagerx.ui;

import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatSpinner;
import androidx.appcompat.widget.SwitchCompat;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.AppOpsParser;
import com.mirfatif.permissionmanagerx.parser.Permission;
import com.mirfatif.permissionmanagerx.ui.PermissionAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.ui.base.MyListAdapter;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.util.ArrayList;
import java.util.List;

public class PermissionAdapter extends MyListAdapter<Permission, ItemViewHolder> {

  private final PermClickListener mSwitchToggleListener;
  private final PermSpinnerSelectListener mSpinnerSelectListener;
  private final PermClickListenerWithLoc mPermClickListener;
  private final PermLongClickListener mPermLongClickListener;
  private final ArrayAdapter<String> mAppOpModesAdapter;
  private final ArrayAdapter<String> mAppOpModesBgAdapter;

  PermissionAdapter(
      Context context,
      PermClickListener switchToggleListener,
      PermSpinnerSelectListener spinnerSelectListener,
      PermClickListenerWithLoc permClickListener,
      PermLongClickListener permLongClickListener) {
    super(new DiffUtilItemCallBack());
    mSwitchToggleListener = switchToggleListener;
    mSpinnerSelectListener = spinnerSelectListener;
    mPermClickListener = permClickListener;
    mPermLongClickListener = permLongClickListener;

    mAppOpModesAdapter = new AppOpModesAdapter(context, false);
    mAppOpModesBgAdapter = new AppOpModesAdapter(context, true);
  }

  // Override Adapter method
  // Inflate a View, create and return a ViewHolder
  @NonNull
  @Override
  public ItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
    LayoutInflater inflater = LayoutInflater.from(parent.getContext());
    View itemView = inflater.inflate(R.layout.recycler_view_item_permission, parent, false);
    return new ItemViewHolder(itemView);
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
    View referenceView;
    ImageView groupIconView;
    TextView permissionNameView;
    TextView protectionLevelView;
    TextView appOpsTimeView;
    TextView appOpsRefStateView;
    SwitchCompat stateSwitch;
    AppCompatSpinner spinner;
    LinearLayout spinnerContainer;
    TextView appOpsDefaultView;

    public ItemViewHolder(@NonNull View itemView) {
      super(itemView);

      // Find Views inside the itemView in XML layout with the given IDs
      referenceView = itemView.findViewById(R.id.reference_indication_view);
      groupIconView = itemView.findViewById(R.id.icon_view);
      permissionNameView = itemView.findViewById(R.id.permission_name_view);
      protectionLevelView = itemView.findViewById(R.id.protection_level_view);
      appOpsTimeView = itemView.findViewById(R.id.app_ops_time_view);
      appOpsRefStateView = itemView.findViewById(R.id.app_ops_ref_state_view);
      stateSwitch = itemView.findViewById(R.id.permission_state_switch);
      spinner = itemView.findViewById(R.id.permission_state_spinner);
      spinnerContainer = itemView.findViewById(R.id.permission_state_spinner_container);
      appOpsDefaultView = itemView.findViewById(R.id.app_ops_default_view);

      itemView.setOnClickListener(this);
      itemView.setOnLongClickListener(this);
    }

    public void bind(int position) {
      Permission permission = getItem(position);

      appOpsRefStateView.setVisibility(View.GONE);
      if (permission.isReferenced() == null) {
        referenceView.setBackgroundColor(PackageAdapter.ORANGE);
      } else if (!permission.isReferenced()) {
        referenceView.setBackgroundColor(Color.RED);
        if (permission.isAppOps()) {
          appOpsRefStateView.setText(
              Utils.htmlToString(
                  App.getContext().getString(R.string.should_be, permission.getReference())));
          appOpsRefStateView.setVisibility(View.VISIBLE);
        }
      } else {
        referenceView.setBackgroundColor(Color.GREEN);
      }

      if (permission.getIconResId() != null) {
        groupIconView.setImageResource(permission.getIconResId());
      }

      spinnerContainer.setOnClickListener(null);

      if (permission.isAppOps()) {
        if (permission.dependsOn() == null) {
          spinnerContainer.setVisibility(View.VISIBLE);
          spinnerContainer.setOnClickListener(v -> spinner.performClick());
        } else {
          spinnerContainer.setVisibility(View.INVISIBLE);
        }
      }

      permissionNameView.setText(permission.createPermNameString());
      protectionLevelView.setText(permission.createProtectLevelString());

      appOpsTimeView.setVisibility(View.GONE);

      if (permission.isAppOps()) {
        String time = permission.getAppOpsAccessTime();
        if (time != null) {
          appOpsTimeView.setText(time);
          appOpsTimeView.setVisibility(View.VISIBLE);
        }

        if (permission.getName().equals("RUN_IN_BACKGROUND")
            || permission.getName().equals("RUN_ANY_IN_BACKGROUND")) {
          spinner.setAdapter(getAppOpModesAdapter(true));
        } else {
          spinner.setAdapter(getAppOpModesAdapter(false));
        }

        spinner.setSelection(permission.getAppOpsMode());
        spinner.setOnItemSelectedListener(new AppOpsModeSelectListener(permission));
        stateSwitch.setVisibility(View.GONE);
        spinner.setEnabled(permission.isChangeable());
        appOpsDefaultView.setVisibility(permission.isAppOpsSet() ? View.GONE : View.VISIBLE);

      } else {
        if (permission.isProviderMissing()) {
          stateSwitch.setVisibility(View.INVISIBLE);
        } else {
          stateSwitch.setChecked(permission.isGranted());
          stateSwitch.setEnabled(permission.isChangeable());
          stateSwitch.setOnClickListener(
              v -> {
                stateSwitch.setChecked(permission.isGranted()); // do not change the state here
                mSwitchToggleListener.onClick(permission);
              });
          stateSwitch.setVisibility(View.VISIBLE);
        }
        spinnerContainer.setVisibility(View.GONE);
      }
    }

    @Override
    public void onClick(View v) {
      int[] location = new int[2];
      v.getLocationInWindow(location);
      mPermClickListener.onClick(
          getItem(getBindingAdapterPosition()), location[1] - 2 * v.getHeight());
    }

    @Override
    public boolean onLongClick(View v) {
      mPermLongClickListener.onLongClick(getItem(getBindingAdapterPosition()));
      return true;
    }
  }

  private static final Object ADAPTER_BUILD_LOCK = new Object();

  private ArrayAdapter<String> getAppOpModesAdapter(boolean forBg) {
    synchronized (ADAPTER_BUILD_LOCK) {
      if (mAppOpModesAdapter.isEmpty() || mAppOpModesBgAdapter.isEmpty()) {
        for (String mode : AppOpsParser.getInstance().getAppOpsModes()) {
          mAppOpModesAdapter.add(Utils.ellipsize(mode, 8));
          mAppOpModesBgAdapter.add(Utils.ellipsize(mode, 8));
        }
      }
    }
    return forBg ? mAppOpModesBgAdapter : mAppOpModesAdapter;
  }

  private static class AppOpModesAdapter extends ArrayAdapter<String> {

    private final List<String> appOpsModes = AppOpsParser.getInstance().getAppOpsModes();
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
        mSpinnerSelectListener.onSelect(permission, position);
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

  interface PermClickListener {
    void onClick(Permission permission);
  }

  interface PermClickListenerWithLoc {
    void onClick(Permission permission, int yLocation);
  }

  interface PermSpinnerSelectListener {
    void onSelect(Permission permission, int selectedValue);
  }

  interface PermLongClickListener {
    void onLongClick(Permission permission);
  }
}
