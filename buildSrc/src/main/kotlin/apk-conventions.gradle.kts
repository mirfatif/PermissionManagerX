import org.gradle.accessors.dm.LibrariesForLibs

plugins {
  id("com.android.application")
  id("org.lsposed.lsparanoid")
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

  kotlinOptions { jvmTarget = "17" }

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
    compose = true
  }

  composeOptions {
    kotlinCompilerExtensionVersion = libs.versions.compose.kotlin.compiler.ext.get()
  }
}

lsparanoid {
  includeDependencies = true
  classFilter = { it.startsWith("com.mirfatif.") }
}

dependencies {
  implementation(libs.androidx.annotation)
  coreLibraryDesugaring(libs.desugar.jdk)
}
