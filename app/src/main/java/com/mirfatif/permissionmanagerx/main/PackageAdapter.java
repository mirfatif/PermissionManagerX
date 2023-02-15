package com.mirfatif.permissionmanagerx.main;

import static com.mirfatif.permissionmanagerx.util.ApiUtils.getString;

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
import androidx.core.graphics.ColorUtils;
import androidx.lifecycle.LifecycleOwner;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.app.App;
import com.mirfatif.permissionmanagerx.base.MyListAdapter;
import com.mirfatif.permissionmanagerx.databinding.RvItemPkgBinding;
import com.mirfatif.permissionmanagerx.main.PackageAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.util.ApiUtils;
import com.mirfatif.permissionmanagerx.util.StringUtils;
import com.mirfatif.permissionmanagerx.util.UiUtils;
import com.mirfatif.permissionmanagerx.util.bg.LiveSingleParamTask;
import com.mirfatif.permissionmanagerx.util.bg.UiRunner;

public class PackageAdapter extends MyListAdapter<Package, ItemViewHolder> {

  private static final String TAG = "PackageAdapter";

  private final LifecycleOwner mLifecycleOwner;
  private final PkgAdapterCallback mCallback;

  public static final int ORANGE = App.getCxt().getColor(R.color.orangeState);

  public PackageAdapter(LifecycleOwner owner, PkgAdapterCallback callback) {
    super(new DiffUtilItemCallBack(), owner, TAG);
    mLifecycleOwner = owner;
    mCallback = callback;
  }

  private Package getPkg(int pos) {
    return pos == RecyclerView.NO_POSITION ? null : getItem(pos);
  }

  public ItemViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
    LayoutInflater inflater = LayoutInflater.from(parent.getContext());
    RvItemPkgBinding binding = RvItemPkgBinding.inflate(inflater, parent, false);
    return new ItemViewHolder(binding);
  }

  public void onBindViewHolder(ItemViewHolder holder, int position) {
    holder.bind(position);
  }

  class ItemViewHolder extends RecyclerView.ViewHolder
      implements OnClickListener, OnLongClickListener {

    private final RvItemPkgBinding mB;
    private final LiveSingleParamTask<Package> mIconSetter =
        new LiveSingleParamTask<>(mLifecycleOwner, this::setIcon, TAG + "-IconSetter");

    public ItemViewHolder(RvItemPkgBinding binding) {
      super(binding.getRoot());
      mB = binding;
      binding.getRoot().setOnClickListener(this);
      binding.getRoot().setOnLongClickListener(this);
    }

    public void bind(int pos) {
      Package pkg = getPkg(pos);
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

      mIconSetter.cancelAndSubmit(pkg, true);

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
              StringUtils.getHighlightString(
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

    public void onClick(View v) {
      int pos = getBindingAdapterPosition();
      Package pkg = getPkg(pos);
      if (pkg != null) {
        mCallback.onClick(pkg);
      }
    }

    public boolean onLongClick(View v) {
      int pos = getBindingAdapterPosition();
      Package pkg = getPkg(pos);
      if (pkg != null) {
        mCallback.onLongClick(pkg);
      }
      return true;
    }

    private void setIcon(Package pkg) {
      try {
        int flags = PackageManager.MATCH_UNINSTALLED_PACKAGES;
        Drawable icon = App.getPm().getApplicationIcon(ApiUtils.getAppInfo(pkg.getName(), flags));
        if (!Thread.interrupted()) {
          UiRunner.post(mLifecycleOwner, () -> mB.iconV.setImageDrawable(icon));
        }
      } catch (NameNotFoundException ignored) {
      }
    }
  }

  private TextAppearanceSpan HIGHLIGHT;

  private TextAppearanceSpan getHighlightSpan(int currentColor) {
    if (HIGHLIGHT == null) {
      HIGHLIGHT =
          UiUtils.getTextHighlightSpan(ColorUtils.blendARGB(currentColor, Color.RED, 0.75f));
    }
    return HIGHLIGHT;
  }

  private static class DiffUtilItemCallBack extends DiffUtil.ItemCallback<Package> {

    public boolean areItemsTheSame(Package oldItem, Package newItem) {
      return oldItem.getName().equals(newItem.getName());
    }

    public boolean areContentsTheSame(Package oldItem, Package newItem) {
      return oldItem.areContentsTheSame(newItem);
    }
  }

  public interface PkgAdapterCallback {

    void onClick(Package pkg);

    void onLongClick(Package pkg);
  }
}
