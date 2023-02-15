package com.mirfatif.permissionmanagerx.privs;

import android.os.Build;
import android.sun.misc.BASE64Encoder;
import android.sun.security.provider.X509Factory;
import android.sun.security.x509.AlgorithmId;
import android.sun.security.x509.CertificateAlgorithmId;
import android.sun.security.x509.CertificateExtensions;
import android.sun.security.x509.CertificateIssuerName;
import android.sun.security.x509.CertificateSerialNumber;
import android.sun.security.x509.CertificateSubjectName;
import android.sun.security.x509.CertificateValidity;
import android.sun.security.x509.CertificateVersion;
import android.sun.security.x509.CertificateX509Key;
import android.sun.security.x509.KeyIdentifier;
import android.sun.security.x509.PrivateKeyUsageExtension;
import android.sun.security.x509.SubjectKeyIdentifierExtension;
import android.sun.security.x509.X500Name;
import android.sun.security.x509.X509CertImpl;
import android.sun.security.x509.X509CertInfo;
import com.mirfatif.err.AdbException;
import com.mirfatif.permissionmanagerx.BuildConfig;
import com.mirfatif.permissionmanagerx.app.App;
import io.github.muntashirakon.adb.AbsAdbConnectionManager;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.spec.EncodedKeySpec;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Date;
import java.util.Random;

public class AdbConnManager extends AbsAdbConnectionManager {

  private PrivateKey mPrivateKey;
  private Certificate mCertificate;

  public AdbConnManager() throws AdbException {
    try {
      init();
    } catch (NoSuchAlgorithmException
        | SignatureException
        | NoSuchProviderException
        | InvalidKeyException
        | InvalidKeySpecException
        | CertificateException
        | IOException e) {
      throw new AdbException(e);
    }
  }

  private void init()
      throws NoSuchAlgorithmException, IOException, CertificateException, InvalidKeySpecException,
          InvalidKeyException, NoSuchProviderException, SignatureException {

    setApi(Build.VERSION.SDK_INT);

    mPrivateKey = readPrivateKeyFromFile();
    mCertificate = readCertificateFromFile();

    if (mPrivateKey == null || mCertificate == null) {

      int keySize = 2048;
      KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
      keyPairGenerator.initialize(keySize, SecureRandom.getInstance("SHA1PRNG"));
      KeyPair generateKeyPair = keyPairGenerator.generateKeyPair();
      PublicKey publicKey = generateKeyPair.getPublic();
      mPrivateKey = generateKeyPair.getPrivate();

      String subject = "CN=" + BuildConfig.APPLICATION_ID;
      String algorithmName = "SHA512withRSA";
      long expiryDate = System.currentTimeMillis() + 86400000;
      CertificateExtensions certificateExtensions = new CertificateExtensions();
      certificateExtensions.set(
          "SubjectKeyIdentifier",
          new SubjectKeyIdentifierExtension(new KeyIdentifier(publicKey).getIdentifier()));
      X500Name x500Name = new X500Name(subject);
      Date notBefore = new Date();
      Date notAfter = new Date(expiryDate);
      certificateExtensions.set(
          "PrivateKeyUsage", new PrivateKeyUsageExtension(notBefore, notAfter));
      CertificateValidity certificateValidity = new CertificateValidity(notBefore, notAfter);
      X509CertInfo x509CertInfo = new X509CertInfo();
      x509CertInfo.set("version", new CertificateVersion(2));
      x509CertInfo.set(
          "serialNumber", new CertificateSerialNumber(new Random().nextInt() & Integer.MAX_VALUE));
      x509CertInfo.set("algorithmID", new CertificateAlgorithmId(AlgorithmId.get(algorithmName)));
      x509CertInfo.set("subject", new CertificateSubjectName(x500Name));
      x509CertInfo.set("key", new CertificateX509Key(publicKey));
      x509CertInfo.set("validity", certificateValidity);
      x509CertInfo.set("issuer", new CertificateIssuerName(x500Name));
      x509CertInfo.set("extensions", certificateExtensions);
      X509CertImpl x509CertImpl = new X509CertImpl(x509CertInfo);
      x509CertImpl.sign(mPrivateKey, algorithmName);
      mCertificate = x509CertImpl;

      writePrivateKeyToFile(mPrivateKey);
      writeCertificateToFile(mCertificate);
    }
  }

  protected PrivateKey getPrivateKey() {
    return mPrivateKey;
  }

  protected Certificate getCertificate() {
    return mCertificate;
  }

  protected String getDeviceName() {
    return BuildConfig.APPLICATION_ID;
  }

  private static final String CERT_FILE = "cert.pem";

  private static Certificate readCertificateFromFile() throws IOException, CertificateException {
    File certFile = getFile(CERT_FILE);
    if (!certFile.exists()) return null;
    try (InputStream cert = new FileInputStream(certFile)) {
      return CertificateFactory.getInstance("X.509").generateCertificate(cert);
    }
  }

  private static void writeCertificateToFile(Certificate certificate)
      throws CertificateEncodingException, IOException {
    File certFile = getFile(CERT_FILE);
    BASE64Encoder encoder = new BASE64Encoder();

    try (OutputStream os = new FileOutputStream(certFile)) {
      os.write(X509Factory.BEGIN_CERT.getBytes(StandardCharsets.UTF_8));
      os.write('\n');
      encoder.encode(certificate.getEncoded(), os);
      os.write('\n');
      os.write(X509Factory.END_CERT.getBytes(StandardCharsets.UTF_8));
    }
  }

  private static final String KEY_FILE = "private.key";

  private static PrivateKey readPrivateKeyFromFile()
      throws IOException, NoSuchAlgorithmException, InvalidKeySpecException {
    File keyFile = getFile(KEY_FILE);
    if (!keyFile.exists()) {
      return null;
    }

    int size = (int) keyFile.length();
    byte[] privKeyBytes = new byte[size];
    try (InputStream is = new FileInputStream(keyFile)) {
      if (is.read(privKeyBytes) != size) {
        throw new IOException("Failed to read private key file");
      }
    }

    KeyFactory keyFactory = KeyFactory.getInstance("RSA");
    EncodedKeySpec privateKeySpec = new PKCS8EncodedKeySpec(privKeyBytes);
    return keyFactory.generatePrivate(privateKeySpec);
  }

  private static void writePrivateKeyToFile(PrivateKey privateKey) throws IOException {
    File privateKeyFile = getFile(KEY_FILE);
    try (OutputStream os = new FileOutputStream(privateKeyFile)) {
      os.write(privateKey.getEncoded());
    }
  }

  private static File getFile(String file) throws IOException {
    File adbDir = new File(App.getCxt().getFilesDir(), "adb");
    if (!adbDir.exists() && !adbDir.mkdirs()) {
      throw new IOException("Could not create directory: " + adbDir);
    }
    return new File(adbDir, file);
  }
}
