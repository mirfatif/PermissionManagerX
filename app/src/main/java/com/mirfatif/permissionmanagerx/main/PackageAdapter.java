package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.util.Utils.getString;

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
import androidx.annotation.NonNull;
import androidx.core.graphics.ColorUtils;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.databinding.RvItemPkgBinding;
import com.mirfatif.permissionmanagerx.main.PackageAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.ui.base.MyListAdapter;
import com.mirfatif.permissionmanagerx.util.Utils;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class PackageAdapter extends MyListAdapter<Package, ItemViewHolder> {

  private final PkgAdapterCallback mCallback;
  private final PackageManager mPackageManager;

  // Orange state color
  public static final int ORANGE = App.getContext().getColor(R.color.orangeState);

  public PackageAdapter(PkgAdapterCallback callback) {
    super(new DiffUtilItemCallBack(), callback::runInFg);
    mCallback = callback;
    mPackageManager = App.getContext().getPackageManager();
  }

  // Override Adapter method
  // Inflate a View, create and return a ViewHolder
  @NonNull
  @Override
  public ItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
    LayoutInflater inflater = LayoutInflater.from(parent.getContext());
    RvItemPkgBinding binding = RvItemPkgBinding.inflate(inflater, parent, false);
    return new ItemViewHolder(binding);
  }

  // Override Adapter method
  // set contents in Views
  @Override
  public void onBindViewHolder(@NonNull ItemViewHolder holder, int position) {
    holder.bind(position);
  }

  private final ExecutorService ICON_SETTER = Executors.newCachedThreadPool();

  // Store and recycle items as they are scrolled off screen
  class ItemViewHolder extends RecyclerView.ViewHolder
      implements OnClickListener, OnLongClickListener {

    private final RvItemPkgBinding mB;

    public ItemViewHolder(RvItemPkgBinding binding) {
      super(binding.getRoot());
      mB = binding;
      binding.getRoot().setOnClickListener(this);
      binding.getRoot().setOnLongClickListener(this);
    }

    public void bind(int position) {
      Package pkg = getItem(position);

      // Rarely pkg comes null, don't know ATM why
      if (pkg == null) {
        return;
      }

      if (pkg.shouldShowRefs()) {
        if (pkg.isReferenced() == null) {
          mB.refIndicationV.setBackgroundColor(ORANGE);
        } else if (!pkg.isReferenced()) {
          mB.refIndicationV.setBackgroundColor(Color.RED);
        } else {
          mB.refIndicationV.setBackgroundColor(Color.GREEN);
        }
        mB.refIndicationV.setVisibility(View.VISIBLE);
      } else {
        mB.refIndicationV.setVisibility(View.GONE);
      }

      ICON_SETTER.submit(() -> setIcon(pkg, mB.iconV));

      mB.pkgLabelV.setText(pkg.getLabel());
      mB.pkgNameV.setText(pkg.getFormattedName());
      mB.pkgPermCountV.setText(pkg.getPermCount());

      String pkgState = null;
      if (pkg.isCriticalApp()) {
        pkgState = getString(R.string.pkg_state_critical);
      } else if (pkg.isFrameworkApp()) {
        pkgState = getString(R.string.pkg_state_framework);
      } else if (pkg.isSystemApp()) {
        pkgState = getString(R.string.pkg_state_system);
      }
      if (!pkg.isEnabled()) {
        pkgState =
            pkgState == null
                ? getString(R.string.pkg_state_disabled)
                : getString(R.string.pkg_state_disabled2, pkgState);
      }

      if (pkgState == null) {
        mB.pkgStateV.setVisibility(View.GONE);
      } else {
        if (pkg.isFrameworkApp() && pkg.isChangeable()) {
          mB.pkgStateV.setText(
              Utils.getHighlightString(
                  pkgState,
                  getHighlightSpan(mB.pkgStateV.getCurrentTextColor()),
                  true,
                  getString(R.string.pkg_state_framework)));
        } else {
          mB.pkgStateV.setText(pkgState);
        }
        mB.pkgStateV.setVisibility(View.VISIBLE);
      }

      mB.dateV.setText(pkg.getDate());
    }

    @Override
    public void onClick(View v) {
      int pos = getBindingAdapterPosition();
      if (pos != RecyclerView.NO_POSITION) {
        mCallback.onClick(getItem(pos));
      }
    }

    @Override
    public boolean onLongClick(View v) {
      int pos = getBindingAdapterPosition();
      if (pos != RecyclerView.NO_POSITION) {
        mCallback.onLongClick(getItem(pos));
      }
      return true;
    }
  }

  private void setIcon(Package pkg, ImageView view) {
    try {
      int flags = PackageManager.MATCH_UNINSTALLED_PACKAGES;
      ApplicationInfo appInfo = mPackageManager.getApplicationInfo(pkg.getName(), flags);
      Drawable icon = mPackageManager.getApplicationIcon(appInfo);
      mCallback.runInFg(() -> view.setImageDrawable(icon));
    } catch (NameNotFoundException ignored) {
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

  public interface PkgAdapterCallback {

    void onClick(Package pkg);

    void onLongClick(Package pkg);

    void runInFg(Runnable task);
  }
}
