package com.mirfatif.privtasks.iface;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import com.mirfatif.err.HiddenAPIsException;
import com.mirfatif.privtasks.bind.AppOpsLists;
import com.mirfatif.privtasks.bind.MyPackageOps;
import com.mirfatif.privtasks.bind.PermFixedFlags;
import com.mirfatif.privtasks.bind.PrivsStatus;
import com.mirfatif.privtasks.util.Util;
import java.util.List;

public interface IPrivTasks extends IInterface {

  void sendStdErr(int port, String jniLibPath) throws RemoteException;

  void hello(IBinder privTasksCb, String crashLogFile) throws RemoteException;

  void setExitOnAppDeath(boolean exitOnAppDeath) throws RemoteException;

  PrivsStatus getPrivsStatus() throws RemoteException;

  boolean setDebug(IBinder logCb) throws RemoteException;

  void dumpHeap(String dir) throws RemoteException;

  void grantAppPrivileges(String pkgName, int uid) throws RemoteException;

  void stopDaemon() throws RemoteException;

  AppOpsLists getAppOpsLists() throws RemoteException;

  PermFixedFlags getPermFixedFlags() throws RemoteException;

  int getPkgCountForUid(int uid) throws RemoteException;

  List<MyPackageOps> getOpsForPackage(int uid, String pkgName, int[] ops) throws RemoteException;

  int getPermFlags(String permName, String pkgName, int userId) throws RemoteException;

  void setPermState(boolean grant, String pkgName, String permName, int userId)
      throws RemoteException;

  void setAppOpMode(int uid, String pkgName, int op, int mode) throws RemoteException;

  void resetAppOps(int userId, String pkgName) throws RemoteException;

  void setPkgState(boolean enable, String pkgName, int userId) throws RemoteException;

  void openAppInfo(String pkgName, int userId) throws RemoteException;

  abstract class Stub extends Binder implements IPrivTasks {

    private static final String DESCRIPTOR = "com.mirfatif.privtasks.iface.IPrivTasks";

    public Stub() {
      attachInterface(this, DESCRIPTOR);
    }

    public IBinder asBinder() {
      return this;
    }

    public boolean onTransact(int code, Parcel data, Parcel reply, int flags)
        throws RemoteException {
      if (code == INTERFACE_TRANSACTION) {
        reply.writeString(DESCRIPTOR);
      } else if (code == TRANSACTION_sendStdErr) {
        sendStdErr(data, reply);
      } else if (code == TRANSACTION_hello) {
        hello(data, reply);
      } else if (code == TRANSACTION_setExitOnAppDeath) {
        setExitOnAppDeath(data, reply);
      } else if (code == TRANSACTION_getPrivsStatus) {
        getPrivsStatus(data, reply);
      } else if (code == TRANSACTION_setDebug) {
        setDebug(data, reply);
      } else if (code == TRANSACTION_dumpHeap) {
        dumpHeap(data, reply);
      } else if (code == TRANSACTION_grantPrivileges) {
        grantPrivileges(data, reply);
      } else if (code == TRANSACTION_stopDaemon) {
        stopDaemon(data, reply);
      } else if (code == TRANSACTION_getAppOpsLists) {
        getAppOpsLists(data, reply);
      } else if (code == TRANSACTION_getPermFixedFlags) {
        getPermFixedFlags(data, reply);
      } else if (code == TRANSACTION_getPkgCountForUid) {
        getPkgCountForUid(data, reply);
      } else if (code == TRANSACTION_getOpsForPackage) {
        getOpsForPackage(data, reply);
      } else if (code == TRANSACTION_getPermFlags) {
        getPermFlags(data, reply);
      } else if (code == TRANSACTION_setPermState) {
        setPermState(data, reply);
      } else if (code == TRANSACTION_setAppOpMode) {
        setAppOpMode(data, reply);
      } else if (code == TRANSACTION_resetAppOps) {
        resetAppOps(data, reply);
      } else if (code == TRANSACTION_setPkgState) {
        setPkgState(data, reply);
      } else if (code == TRANSACTION_openAppInfo) {
        openAppInfo(data, reply);
      } else {
        return super.onTransact(code, data, reply, flags);
      }
      return true;
    }

    public static IPrivTasks asInterface(IBinder iBinder) {
      IInterface iin = iBinder.queryLocalInterface(DESCRIPTOR);
      if (iin instanceof IPrivTasks) {
        return (IPrivTasks) iin;
      } else {
        return new Proxy(iBinder);
      }
    }

    private static final int TRANSACTION_sendStdErr = createTransactionCode();

    private static void sendStdErr(int port, String jniLibPath, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(port);
      data.writeString(jniLibPath);

      try {
        remote.transact(TRANSACTION_sendStdErr, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void sendStdErr(Parcel data, Parcel reply) {
      initReply(data, reply);

      int port = data.readInt();
      String jniLibPath = data.readString();

      try {
        sendStdErr(port, jniLibPath);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_hello = createTransactionCode();

    private static void hello(IBinder privTasksCb, String crashLogFile, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeStrongBinder(privTasksCb);
      data.writeString(crashLogFile);

      try {
        remote.transact(TRANSACTION_hello, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void hello(Parcel data, Parcel reply) {
      initReply(data, reply);

      IBinder privTaskCb = data.readStrongBinder();
      String crashLogFile = data.readString();

      try {
        hello(privTaskCb, crashLogFile);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_setExitOnAppDeath = createTransactionCode();

    private static void setExitOnAppDeath(boolean exitOnAppDeath, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(exitOnAppDeath ? 1 : 0);

      try {
        remote.transact(TRANSACTION_setExitOnAppDeath, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void setExitOnAppDeath(Parcel data, Parcel reply) {
      initReply(data, reply);

      boolean exitOnAppDeath = data.readInt() == 1;

      try {
        setExitOnAppDeath(exitOnAppDeath);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_getPrivsStatus = createTransactionCode();

    private static PrivsStatus getPrivsStatus(IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      try {
        remote.transact(TRANSACTION_getPrivsStatus, data, reply, 0);
        readException(reply);
        return reply.readTypedObject(PrivsStatus.CREATOR);
      } finally {
        recycle(data, reply);
      }
    }

    private void getPrivsStatus(Parcel data, Parcel reply) {
      initReply(data, reply);

      try {
        reply.writeTypedObject(getPrivsStatus(), 0);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_setDebug = createTransactionCode();

    private static boolean setDebug(IBinder logCallback, IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeStrongBinder(logCallback);

      try {
        remote.transact(TRANSACTION_setDebug, data, reply, 0);
        readException(reply);
        return (reply.readInt() != 0);
      } finally {
        recycle(data, reply);
      }
    }

    private void setDebug(Parcel data, Parcel reply) {
      initReply(data, reply);

      IBinder logCallback = data.readStrongBinder();

      try {
        boolean result = setDebug(logCallback);
        reply.writeInt(result ? 1 : 0);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_dumpHeap = createTransactionCode();

    private static void dumpHeap(String dir, IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeString(dir);

      try {
        remote.transact(TRANSACTION_dumpHeap, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void dumpHeap(Parcel data, Parcel reply) {
      initReply(data, reply);

      String dir = data.readString();

      try {
        dumpHeap(dir);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_grantPrivileges = createTransactionCode();

    private static void grantPrivileges(String pkgName, int uid, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeString(pkgName);
      data.writeInt(uid);

      try {
        remote.transact(TRANSACTION_grantPrivileges, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void grantPrivileges(Parcel data, Parcel reply) {
      initReply(data, reply);

      String pkgName = data.readString();
      int uid = data.readInt();

      try {
        grantAppPrivileges(pkgName, uid);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_stopDaemon = createTransactionCode();

    private static void stopDaemon(IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      try {
        remote.transact(TRANSACTION_stopDaemon, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void stopDaemon(Parcel data, Parcel reply) {
      initReply(data, reply);

      try {
        stopDaemon();
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_getAppOpsLists = createTransactionCode();

    private static AppOpsLists getAppOpsLists(IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      try {
        remote.transact(TRANSACTION_getAppOpsLists, data, reply, 0);
        readException(reply);
        return reply.readTypedObject(AppOpsLists.CREATOR);
      } finally {
        recycle(data, reply);
      }
    }

    private void getAppOpsLists(Parcel data, Parcel reply) {
      initReply(data, reply);

      try {
        reply.writeTypedObject(getAppOpsLists(), 0);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_getPermFixedFlags = createTransactionCode();

    private static PermFixedFlags getPermFixedFlags(IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      try {
        remote.transact(TRANSACTION_getPermFixedFlags, data, reply, 0);
        readException(reply);
        return reply.readTypedObject(PermFixedFlags.CREATOR);
      } finally {
        recycle(data, reply);
      }
    }

    private void getPermFixedFlags(Parcel data, Parcel reply) {
      initReply(data, reply);

      try {
        reply.writeTypedObject(getPermFixedFlags(), 0);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_getPkgCountForUid = createTransactionCode();

    private static int getPkgCountForUid(int uid, IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(uid);

      try {
        remote.transact(TRANSACTION_getPkgCountForUid, data, reply, 0);
        readException(reply);
        return reply.readInt();
      } finally {
        recycle(data, reply);
      }
    }

    private void getPkgCountForUid(Parcel data, Parcel reply) {
      initReply(data, reply);

      int uid = data.readInt();

      try {
        reply.writeInt(getPkgCountForUid(uid));
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_getOpsForPackage = createTransactionCode();

    private static List<MyPackageOps> getOpsForPackage(
        int uid, String pkgName, int[] ops, IBinder remote) throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(uid);
      data.writeString(pkgName);
      data.writeIntArray(ops);

      try {
        remote.transact(TRANSACTION_getOpsForPackage, data, reply, 0);
        readException(reply);
        return reply.createTypedArrayList(MyPackageOps.CREATOR);
      } finally {
        recycle(data, reply);
      }
    }

    private void getOpsForPackage(Parcel data, Parcel reply) {
      initReply(data, reply);

      int uid = data.readInt();
      String pkgName = data.readString();
      int[] ops = data.createIntArray();

      try {
        reply.writeTypedList(getOpsForPackage(uid, pkgName, ops));
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_getPermFlags = createTransactionCode();

    private static int getPermFlags(String permName, String pkgName, int userId, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeString(permName);
      data.writeString(pkgName);
      data.writeInt(userId);

      try {
        remote.transact(TRANSACTION_getPermFlags, data, reply, 0);
        readException(reply);
        return reply.readInt();
      } finally {
        recycle(data, reply);
      }
    }

    private void getPermFlags(Parcel data, Parcel reply) {
      initReply(data, reply);

      String permName = data.readString();
      String pkgName = data.readString();
      int userId = data.readInt();

      try {
        reply.writeInt(getPermFlags(permName, pkgName, userId));
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_setPermState = createTransactionCode();

    private static void setPermState(
        boolean grant, String pkgName, String permName, int userId, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(grant ? 1 : 0);
      data.writeString(pkgName);
      data.writeString(permName);
      data.writeInt(userId);

      try {
        remote.transact(TRANSACTION_setPermState, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void setPermState(Parcel data, Parcel reply) {
      initReply(data, reply);

      boolean grant = data.readInt() != 0;
      String pkgName = data.readString();
      String permName = data.readString();
      int userId = data.readInt();

      try {
        setPermState(grant, pkgName, permName, userId);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_setAppOpMode = createTransactionCode();

    private static void setAppOpMode(int uid, String pkgName, int op, int mode, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(uid);
      data.writeString(pkgName);
      data.writeInt(op);
      data.writeInt(mode);

      try {
        remote.transact(TRANSACTION_setAppOpMode, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void setAppOpMode(Parcel data, Parcel reply) {
      initReply(data, reply);

      int uid = data.readInt();
      String pkgName = data.readString();
      int op = data.readInt();
      int mode = data.readInt();

      try {
        setAppOpMode(uid, pkgName, op, mode);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_resetAppOps = createTransactionCode();

    private static void resetAppOps(int userId, String pkgName, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(userId);
      data.writeString(pkgName);

      try {
        remote.transact(TRANSACTION_resetAppOps, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void resetAppOps(Parcel data, Parcel reply) {
      initReply(data, reply);

      int userId = data.readInt();
      String pkgName = data.readString();

      try {
        resetAppOps(userId, pkgName);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_setPkgState = createTransactionCode();

    private static void setPkgState(boolean enable, String pkgName, int userId, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeInt(enable ? 1 : 0);
      data.writeString(pkgName);
      data.writeInt(userId);

      try {
        remote.transact(TRANSACTION_setPkgState, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void setPkgState(Parcel data, Parcel reply) {
      initReply(data, reply);

      boolean enable = data.readInt() != 0;
      String pkgName = data.readString();
      int userId = data.readInt();

      try {
        setPkgState(enable, pkgName, userId);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static final int TRANSACTION_openAppInfo = createTransactionCode();

    private static void openAppInfo(String pkgName, int userId, IBinder remote)
        throws RemoteException {
      Parcel data = obtainData(), reply = Parcel.obtain();

      data.writeString(pkgName);
      data.writeInt(userId);

      try {
        remote.transact(TRANSACTION_openAppInfo, data, reply, 0);
        readException(reply);
      } finally {
        recycle(data, reply);
      }
    }

    private void openAppInfo(Parcel data, Parcel reply) {
      initReply(data, reply);

      String pkgName = data.readString();
      int userId = data.readInt();

      try {
        openAppInfo(pkgName, userId);
      } catch (Throwable t) {
        writeException(reply, t);
      }
    }

    private static int CODE = 0;

    private static int createTransactionCode() {
      return FIRST_CALL_TRANSACTION + CODE++;
    }

    private static Parcel obtainData() {
      Parcel data = Parcel.obtain();
      data.writeInterfaceToken(DESCRIPTOR);
      return data;
    }

    private static void recycle(Parcel data, Parcel reply) {
      data.recycle();
      reply.recycle();
    }

    private static void initReply(Parcel data, Parcel reply) {
      data.enforceInterface(DESCRIPTOR);
      reply.writeInt(NO_ERROR);
    }

    private static final int NO_ERROR = 0;
    private static final int ERROR = 1;

    private static void writeException(Parcel reply, Throwable t) {
      reply.setDataSize(0);
      reply.setDataPosition(0);

      reply.writeInt(ERROR);
      reply.writeSerializable(t);
    }

    private static void readException(Parcel reply) throws RemoteException {
      if (reply.readInt() == ERROR) {
        Throwable t = Util.readSerializable(reply, Throwable.class);
        if (t instanceof HiddenAPIsException) {
          throw (HiddenAPIsException) t;
        } else if (t instanceof RemoteException) {
          throw (RemoteException) t;
        } else {
          RemoteException re = new RemoteException();
          re.initCause(t);
          throw re;
        }
      }
    }

    private static class Proxy implements IPrivTasks {

      private final IBinder mRemote;

      private Proxy(IBinder remote) {
        mRemote = remote;
      }

      public IBinder asBinder() {
        return mRemote;
      }

      public void sendStdErr(int port, String jniLibPath) throws RemoteException {
        Stub.sendStdErr(port, jniLibPath, mRemote);
      }

      public void hello(IBinder privTasksCb, String crashLogFile) throws RemoteException {
        Stub.hello(privTasksCb, crashLogFile, mRemote);
      }

      public void setExitOnAppDeath(boolean exitOnAppDeath) throws RemoteException {
        Stub.setExitOnAppDeath(exitOnAppDeath, mRemote);
      }

      public PrivsStatus getPrivsStatus() throws RemoteException {
        return Stub.getPrivsStatus(mRemote);
      }

      public boolean setDebug(IBinder logCallback) throws RemoteException {
        return Stub.setDebug(logCallback, mRemote);
      }

      public void dumpHeap(String dir) throws RemoteException {
        Stub.dumpHeap(dir, mRemote);
      }

      public void grantAppPrivileges(String pkgName, int uid) throws RemoteException {
        Stub.grantPrivileges(pkgName, uid, mRemote);
      }

      public void stopDaemon() throws RemoteException {
        Stub.stopDaemon(mRemote);
      }

      public AppOpsLists getAppOpsLists() throws RemoteException {
        return Stub.getAppOpsLists(mRemote);
      }

      public PermFixedFlags getPermFixedFlags() throws RemoteException {
        return Stub.getPermFixedFlags(mRemote);
      }

      public int getPkgCountForUid(int uid) throws RemoteException {
        return Stub.getPkgCountForUid(uid, mRemote);
      }

      public List<MyPackageOps> getOpsForPackage(int uid, String pkgName, int[] ops)
          throws RemoteException {
        return Stub.getOpsForPackage(uid, pkgName, ops, mRemote);
      }

      public int getPermFlags(String permName, String pkgName, int userId) throws RemoteException {
        return Stub.getPermFlags(permName, pkgName, userId, mRemote);
      }

      public void setPermState(boolean grant, String pkgName, String permName, int userId)
          throws RemoteException {
        Stub.setPermState(grant, pkgName, permName, userId, mRemote);
      }

      public void setAppOpMode(int uid, String pkgName, int op, int mode) throws RemoteException {
        Stub.setAppOpMode(uid, pkgName, op, mode, mRemote);
      }

      public void resetAppOps(int userId, String pkgName) throws RemoteException {
        Stub.resetAppOps(userId, pkgName, mRemote);
      }

      public void setPkgState(boolean enable, String pkgName, int userId) throws RemoteException {
        Stub.setPkgState(enable, pkgName, userId, mRemote);
      }

      public void openAppInfo(String pkgName, int userId) throws RemoteException {
        Stub.openAppInfo(pkgName, userId, mRemote);
      }
    }
  }
}
