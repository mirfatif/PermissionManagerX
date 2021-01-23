package com.mirfatif.permissionmanagerx.ui;

import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
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
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.util.Utils;

public class PackageAdapter extends ListAdapter<Package, ItemViewHolder> {

  private final PkgClickListener mPkgClickListener;
  private final PkgLongClickListener mPkgLongClickListener;
  private final PackageManager mPackageManager;

  // orange state color
  static final int ORANGE = 0xFFFFC107;

  public PackageAdapter(
      PkgClickListener pkgClickListener, PkgLongClickListener pkgLongClickListener) {
    super(new DiffUtilItemCallBack());
    mPkgClickListener = pkgClickListener;
    mPkgLongClickListener = pkgLongClickListener;
    mPackageManager = App.getContext().getPackageManager();
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

      // Rarely pkg comes null, don't know ATM why
      if (pkg == null) {
        return;
      }

      if (pkg.isNotQuicklyScanned()) {
        if (pkg.isReferenced() == null) {
          referenceView.setBackgroundColor(ORANGE);
        } else if (!pkg.isReferenced()) {
          referenceView.setBackgroundColor(Color.RED);
        } else {
          referenceView.setBackgroundColor(Color.GREEN);
        }
        referenceView.setVisibility(View.VISIBLE);
      } else {
        referenceView.setVisibility(View.GONE);
      }

      Utils.runInBg(
          () -> {
            try {
              Drawable icon = mPackageManager.getApplicationIcon(pkg.getName());
              Utils.runInFg(() -> iconView.setImageDrawable(icon));
            } catch (NameNotFoundException ignored) {
            }
          });

      packageLabelView.setText(pkg.getLabel());
      String pkgName = pkg.getName() + " (" + pkg.getUid() + ")";
      packageNameView.setText(pkgName);

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

      if (pkg.isNotQuicklyScanned()) {
        String count =
            pkg.getPermCount()
                + "/"
                + pkg.getTotalPermCount()
                + (MySettings.getInstance().excludeAppOpsPerms()
                    ? ""
                    : " | " + pkg.getAppOpsCount() + "/" + pkg.getTotalAppOpsCount());
        permCountView.setText(count);
        permCountView.setVisibility(View.VISIBLE);
      } else {
        permCountView.setVisibility(View.GONE);
      }
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

  private static class DiffUtilItemCallBack extends DiffUtil.ItemCallback<Package> {
    @Override
    public boolean areItemsTheSame(@NonNull Package oldItem, @NonNull Package newItem) {
      return oldItem.getName().equals(newItem.getName());
    }

    @Override
    public boolean areContentsTheSame(@NonNull Package oldItem, @NonNull Package newItem) {
      return oldItem.areContentsTheSame(newItem);
    }
  }

  public interface PkgClickListener {
    void onClick(Package pkg);
  }

  public interface PkgLongClickListener {
    void onLongClick(Package pkg);
  }
}
