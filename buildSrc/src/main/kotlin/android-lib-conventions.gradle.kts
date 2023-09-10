import org.gradle.accessors.dm.LibrariesForLibs

plugins {
  id("com.android.library")
  id("android-base-conventions")
}

val libs = the<LibrariesForLibs>()

android {
  compileSdk = libs.versions.sdk.compile.get().toInt()
  buildToolsVersion = libs.versions.sdk.tools.build.get()

  defaultConfig { minSdk = libs.versions.sdk.min.get().toInt() }

  compileOptions {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
  }

  kotlinOptions { jvmTarget = "17" }

  buildFeatures { buildConfig = false }
}

dependencies {
  implementation(libs.androidx.annotation)
  compileOnly(libs.lsparanoid.core)
}
