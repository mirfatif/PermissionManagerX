import org.gradle.accessors.dm.LibrariesForLibs
import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
  id("com.android.application")
  id("android-base-conventions")
}

val libs = the<LibrariesForLibs>()

android {
  compileSdk = libs.versions.sdk.compile.get().toInt()
  buildToolsVersion = libs.versions.sdk.tools.build.get()

  defaultConfig {
    minSdk = libs.versions.sdk.min.get().toInt()
    targetSdk = libs.versions.sdk.target.get().toInt()

    multiDexEnabled = true // Required for desugaring
  }

  compileOptions {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17

    isCoreLibraryDesugaringEnabled = true
  }

  kotlin.compilerOptions.jvmTarget.set(JvmTarget.JVM_17)

  buildTypes {
    release {
      isMinifyEnabled = true
      isShrinkResources = true

      proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
    }
  }

  dependenciesInfo { includeInApk = false }

  buildFeatures {
    viewBinding = true
    dataBinding = true
    buildConfig = true
  }
}

dependencies {
  implementation(libs.androidx.annotation)
  coreLibraryDesugaring(libs.desugar.jdk.nio)
}
