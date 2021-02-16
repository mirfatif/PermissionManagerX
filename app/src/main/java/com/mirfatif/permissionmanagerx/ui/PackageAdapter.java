package com.mirfatif.permissionmanagerx.ui;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.text.style.TextAppearanceSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.core.graphics.ColorUtils;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.ui.PackageAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.ui.base.MyListAdapter;
import com.mirfatif.permissionmanagerx.util.Utils;

public class PackageAdapter extends MyListAdapter<Package, ItemViewHolder> {

  private final PkgClickListener mPkgClickListener;
  private final PkgLongClickListener mPkgLongClickListener;
  private final PackageManager mPackageManager;

  // Orange state color
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
    TextView dateView;

    public ItemViewHolder(@NonNull View itemView) {
      super(itemView);

      // Find Views inside the itemView in XML layout with the given IDs
      referenceView = itemView.findViewById(R.id.reference_indication_view);
      iconView = itemView.findViewById(R.id.icon_view);
      packageLabelView = itemView.findViewById(R.id.package_label_view);
      packageNameView = itemView.findViewById(R.id.package_name_view);
      packageStateView = itemView.findViewById(R.id.package_state_view);
      permCountView = itemView.findViewById(R.id.package_perm_count_view);
      dateView = itemView.findViewById(R.id.date_view);

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

      if (pkg.shouldShowRefs()) {
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
              int flags = PackageManager.MATCH_UNINSTALLED_PACKAGES;
              ApplicationInfo appInfo = mPackageManager.getApplicationInfo(pkg.getName(), flags);
              Drawable icon = mPackageManager.getApplicationIcon(appInfo);
              Utils.runInFg(() -> iconView.setImageDrawable(icon));
            } catch (NameNotFoundException ignored) {
            }
          });

      packageLabelView.setText(pkg.getLabel());
      packageNameView.setText(pkg.getFormattedName());
      permCountView.setText(pkg.getPermCount());

      String packageState = null;
      if (pkg.isCriticalApp()) {
        packageState = "Critical";
      } else if (pkg.isFrameworkApp()) {
        packageState = Package.FRAMEWORK;
      } else if (pkg.isSystemApp()) {
        packageState = "System";
      }
      if (!pkg.isEnabled()) {
        packageState = packageState == null ? "Disabled" : packageState + ", Disabled";
      }

      if (packageState == null) {
        packageStateView.setVisibility(View.GONE);
      } else {
        if (pkg.isFrameworkApp() && pkg.isChangeable()) {
          packageStateView.setText(
              Utils.getHighlightString(
                  packageState,
                  getHighlightSpan(packageStateView.getCurrentTextColor()),
                  true,
                  Package.FRAMEWORK));
        } else {
          packageStateView.setText(packageState);
        }
        packageStateView.setVisibility(View.VISIBLE);
      }

      dateView.setText(pkg.getDate());
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

  private TextAppearanceSpan HIGHLIGHT;

  private TextAppearanceSpan getHighlightSpan(int currentColor) {
    if (HIGHLIGHT == null) {
      HIGHLIGHT = Utils.getHighlight(ColorUtils.blendARGB(currentColor, Color.RED, 0.75f));
    }
    return HIGHLIGHT;
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
