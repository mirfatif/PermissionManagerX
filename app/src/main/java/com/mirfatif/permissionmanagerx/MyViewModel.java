package com.mirfatif.permissionmanagerx;

import android.app.Application;
import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import com.mirfatif.permissionmanagerx.parser.PackageParser;
import java.util.List;

public class MyViewModel extends AndroidViewModel {

  final PackageParser mPackageParser;
  final MySettings mMySettings;
  final PrivDaemonHandler mDaemonHandler;

  public MyViewModel(@NonNull Application application) {
    super(application);

    // create and hold global instances
    mPackageParser = PackageParser.getInstance();
    mMySettings = MySettings.getInstance();
    mDaemonHandler = PrivDaemonHandler.getInstance();
  }

  LiveData<List<Package>> getPackagesListLive() {
    return mPackageParser.getPackagesListLive();
  }

  LiveData<Package> getChangedPackage() {
    return mPackageParser.getChangedPackage();
  }

  LiveData<Integer> getProgressMax() {
    return mPackageParser.getProgressMax();
  }

  LiveData<Integer> getProgressNow() {
    return mPackageParser.getProgressNow();
  }

  LiveData<Boolean> getHiddenAPIsNotWorking() {
    return Utils.mHiddenAPIsNotWorking;
  }
}
