package com.mirfatif.permissionmanagerx.about;

import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.recyclerview.widget.RecyclerView;
import androidx.recyclerview.widget.RecyclerView.ViewHolder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.about.PrivilegesAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.databinding.AboutPrivilegesItemBinding;
import com.mirfatif.privtasks.bind.PrivsStatus;
import java.util.ArrayList;
import java.util.List;

public class PrivilegesAdapter extends RecyclerView.Adapter<ItemViewHolder> {

  private final List<PrivsStatus.PermStatus> mPermStatusList = new ArrayList<>();

  void submitList(List<PrivsStatus.PermStatus> permStatusList) {
    synchronized (mPermStatusList) {
      mPermStatusList.clear();
      mPermStatusList.addAll(permStatusList);
      notifyDataSetChanged();
    }
  }

  public ItemViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
    LayoutInflater inflater = LayoutInflater.from(parent.getContext());
    AboutPrivilegesItemBinding b = AboutPrivilegesItemBinding.inflate(inflater, parent, false);
    return new ItemViewHolder(b);
  }

  public void onBindViewHolder(ItemViewHolder holder, int position) {
    holder.bind(position);
  }

  public long getItemId(int position) {
    return position;
  }

  public int getItemCount() {
    return mPermStatusList.size();
  }

  class ItemViewHolder extends ViewHolder {

    private final AboutPrivilegesItemBinding mB;

    public ItemViewHolder(AboutPrivilegesItemBinding binding) {
      super(binding.getRoot());
      mB = binding;
    }

    void bind(int pos) {
      PrivsStatus.PermStatus item = mPermStatusList.get(pos);
      if (item != null) {
        mB.permV.setSelected(true);
        mB.permV.setText(item.name.replaceFirst("^android.permission.", ""));
        mB.statusV.setImageResource(item.granted ? R.drawable.tick : R.drawable.cross_red);
      }
    }
  }
}
