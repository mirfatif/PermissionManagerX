package com.mirfatif.permissionmanagerx.ui;

import android.app.Application;
import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import com.mirfatif.permissionmanagerx.parser.Package;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import com.mirfatif.permissionmanagerx.prefs.MySettings;
import com.mirfatif.permissionmanagerx.privs.PrivDaemonHandler;
import java.util.List;

public class MyViewModel extends AndroidViewModel {

  private final PackageParser mPackageParser;
  final MySettings mMySettings;
  final PrivDaemonHandler mDaemonHandler;

  public MyViewModel(@NonNull Application application) {
    super(application);

    // create and hold global instances
    mPackageParser = PackageParser.getInstance();
    mMySettings = MySettings.getInstance();
    mDaemonHandler = PrivDaemonHandler.getInstance();
  }

  public LiveData<List<Package>> getPackagesListLive() {
    return mPackageParser.getPackagesListLive();
  }

  public LiveData<Package> getChangedPackage() {
    return mPackageParser.getChangedPackage();
  }

  public LiveData<Integer> getProgressMax() {
    return mPackageParser.getProgressMax();
  }

  public LiveData<Integer> getProgressNow() {
    return mPackageParser.getProgressNow();
  }

  public LiveData<Void> getDrawerChanged() {
    return mDrawerChanged;
  }

  private static final MutableLiveData<Void> mDrawerChanged = new MutableLiveData<>();

  public static void updateDrawer() {
    mDrawerChanged.postValue(null);
  }
}
