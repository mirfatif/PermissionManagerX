package com.mirfatif;

import com.mirfatif.annotation.HiddenClass.HiddenClasses;
import com.mirfatif.annotation.HiddenField.HiddenFields;
import com.mirfatif.annotation.HiddenMethod.HiddenMethods;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public class annotation {

  @Retention(RetentionPolicy.SOURCE)
  @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
  @Repeatable(HiddenClasses.class)
  public @interface HiddenClass {

    Class<?> cls();

    CType type() default CType.CLASS;

    enum CType {
      CLASS,
      INNER_CLASS
    }

    @Retention(RetentionPolicy.SOURCE)
    @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
    @interface HiddenClasses {

      @SuppressWarnings("UnusedDeclaration")
      HiddenClass[] value();
    }
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
  @Repeatable(HiddenMethods.class)
  public @interface HiddenMethod {

    String name();

    MType type() default MType.METHOD;

    Class<?>[] cls();

    int minSDK() default 1;

    int maxSDK() default 1;

    enum MType {
      METHOD,
      STATIC_METHOD
    }

    @Retention(RetentionPolicy.SOURCE)
    @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
    @interface HiddenMethods {

      @SuppressWarnings("UnusedDeclaration")
      HiddenMethod[] value();
    }
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  @Repeatable(HiddenFields.class)
  public @interface HiddenField {

    String[] name();

    FType type() default FType.FIELD;

    Class<?> cls();

    int minSDK() default 1;

    enum FType {
      FIELD,
      STATIC_FIELD
    }

    @Retention(RetentionPolicy.SOURCE)
    @Target(ElementType.METHOD)
    @interface HiddenFields {

      @SuppressWarnings("UnusedDeclaration")
      HiddenField[] value();
    }
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  public @interface Throws {

    String name();
  }

  // Mostly permission checks regard UIDs: 0 and 1000, some 2000 too. On failed check usually a
  // SecurityException is thrown. E.g. ADB lacks permissions on MIUI.
  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  public @interface Privileged {

    String[] requires() default "";
  }

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  public @interface DaemonOnly {}

  @Retention(RetentionPolicy.SOURCE)
  @Target(ElementType.METHOD)
  public @interface NonDaemonOnly {}
}
