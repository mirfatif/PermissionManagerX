package android.content.pm;

import com.mirfatif.privtasks.hiddenapis.HiddenAPIs.NonSDK;
import java.util.List;

@NonSDK
public abstract class ParceledListSlice<T> {

  public abstract List<T> getList();
}
