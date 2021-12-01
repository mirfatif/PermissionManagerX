package com.mirfatif.permissionmanagerx.about;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import androidx.recyclerview.widget.RecyclerView.ViewHolder;
import com.mirfatif.permissionmanagerx.R;
import com.mirfatif.permissionmanagerx.about.AboutPrivilegesAdapter.ItemViewHolder;
import com.mirfatif.permissionmanagerx.databinding.AboutPrivilegesItemBinding;
import com.mirfatif.privtasks.ser.PermStatus;
import java.util.ArrayList;
import java.util.List;

public class AboutPrivilegesAdapter extends RecyclerView.Adapter<ItemViewHolder> {

  private final List<PermStatus> mPermStatusList = new ArrayList<>();

  @SuppressLint("NotifyDataSetChanged")
  void submitList(List<PermStatus> permStatusList) {
    synchronized (mPermStatusList) {
      mPermStatusList.clear();
      mPermStatusList.addAll(permStatusList);
      notifyDataSetChanged();
    }
  }

  @NonNull
  @Override
  public ItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
    LayoutInflater inflater = LayoutInflater.from(parent.getContext());
    AboutPrivilegesItemBinding b = AboutPrivilegesItemBinding.inflate(inflater, parent, false);
    return new ItemViewHolder(b);
  }

  @Override
  public void onBindViewHolder(@NonNull ItemViewHolder holder, int position) {
    holder.bind(position);
  }

  @Override
  public long getItemId(int position) {
    return position;
  }

  @Override
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
      PermStatus item = mPermStatusList.get(pos);
      if (item != null) {
        mB.permV.setSelected(true);
        mB.permV.setText(item.name.replaceFirst("^android.permission.", ""));
        mB.statusV.setImageResource(item.granted ? R.drawable.tick : R.drawable.cross);
      }
    }
  }
}
