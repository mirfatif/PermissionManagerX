package com.mirfatif.permissionmanagerx;

import android.app.Application;
import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import java.util.List;

public class MyViewModel extends AndroidViewModel {

  private final PackageParser mPackageParser;

  public MyViewModel(@NonNull Application application) {
    super(application);

    // create instances
    mPackageParser = PackageParser.getInstance();
    mPackageParser.initiateVariables();

    MySettings.getInstance().initializeVariables();
    PrivDaemonHandler.getInstance();
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
