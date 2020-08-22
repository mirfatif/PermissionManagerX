package com.mirfatif.permissionmanagerx;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.ListAdapter;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.PackageAdapter.ItemViewHolder;

public class PackageAdapter extends ListAdapter<Package, ItemViewHolder> {

  private final PkgClickListener mPkgClickListener;
  private final PkgLongClickListener mPkgLongClickListener;

  // orange state color
  static final int ORANGE = 0xFFFFC107;

  PackageAdapter(PkgClickListener pkgClickListener, PkgLongClickListener pkgLongClickListener) {
    super(new DiffUtilItemCallBack());
    mPkgClickListener = pkgClickListener;
    mPkgLongClickListener = pkgLongClickListener;
  }

  // Override Adapter method
  // Inflate a View, create and return a ViewHolder
  @NonNull
  @Override
  public ItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
    View itemView =
        LayoutInflater.from(parent.getContext())
            .inflate(R.layout.recycler_view_item_package, parent, false);
    return new ItemViewHolder(itemView);
  }

  // Override Adapter method
  // set contents in Views
  @Override
  public void onBindViewHolder(@NonNull ItemViewHolder holder, int position) {
    holder.bind(position);
  }

  // Store and recycle items as they are scrolled off screen
  class ItemViewHolder extends RecyclerView.ViewHolder
      implements OnClickListener, OnLongClickListener {

    View referenceView;
    ImageView iconView;
    TextView packageLabelView;
    TextView packageNameView;
    TextView packageStateView;
    TextView permCountView;

    public ItemViewHolder(@NonNull View itemView) {
      super(itemView);

      // Find Views inside the itemView in XML layout with the given IDs
      referenceView = itemView.findViewById(R.id.reference_indication_view);
      iconView = itemView.findViewById(R.id.icon_view);
      packageLabelView = itemView.findViewById(R.id.package_label_view);
      packageNameView = itemView.findViewById(R.id.package_name_view);
      packageStateView = itemView.findViewById(R.id.package_state_view);
      permCountView = itemView.findViewById(R.id.package_perm_count_view);

      // Set click listener on whole item
      itemView.setOnClickListener(this);
      itemView.setOnLongClickListener(this);
    }

    public void bind(int position) {
      Package pkg = getItem(position);

      if (pkg.isReferenced() == null) {
        referenceView.setBackgroundColor(ORANGE);
      } else if (!pkg.isReferenced()) {
        referenceView.setBackgroundColor(Color.RED);
      } else {
        referenceView.setBackgroundColor(Color.GREEN);
      }

      iconView.setImageDrawable(pkg.getIcon());
      packageLabelView.setText(pkg.getLabel());
      packageNameView.setText(pkg.getName() + " (" + pkg.getUid() + ")");

      String packageState = null;
      if (pkg.isCriticalApp()) {
        packageState = "Critical";
      } else if (pkg.isFrameworkApp()) {
        packageState = "Framework";
      } else if (pkg.isSystemApp()) {
        packageState = "System";
      }
      if (!pkg.isEnabled()) {
        if (packageState == null) packageState = "Disabled";
        else packageState = packageState + ", Disabled";
      }

      if (packageState == null) {
        packageStateView.setVisibility(View.GONE);
      } else {
        packageStateView.setText(packageState);
        packageStateView.setVisibility(View.VISIBLE);
      }

      String count =
          pkg.getPermCount()
              + "/"
              + pkg.getTotalPermCount()
              + (MySettings.getInstance().excludeAppOpsPerms()
                  ? ""
                  : " | " + pkg.getAppOpsCount() + "/" + pkg.getTotalAppOpsCount());
      permCountView.setText(count);
    }

    @Override
    public void onClick(View v) {
      mPkgClickListener.onClick(getItem(getBindingAdapterPosition()));
    }

    @Override
    public boolean onLongClick(View v) {
      mPkgLongClickListener.onLongClick(getItem(getBindingAdapterPosition()));
      return true;
    }
  }

  static class DiffUtilItemCallBack extends DiffUtil.ItemCallback<Package> {
    @Override
    public boolean areItemsTheSame(@NonNull Package oldItem, @NonNull Package newItem) {
      return oldItem.getName().equals(newItem.getName());
    }

    @Override
    public boolean areContentsTheSame(@NonNull Package oldItem, @NonNull Package newItem) {
      return oldItem.areContentsTheSame(newItem);
    }
  }
}
