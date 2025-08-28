import org.gradle.accessors.dm.LibrariesForLibs
import org.jetbrains.kotlin.gradle.dsl.JvmTarget

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

  kotlin.compilerOptions.jvmTarget.set(JvmTarget.JVM_17)

  buildFeatures { buildConfig = false }
}

dependencies { implementation(libs.androidx.annotation) }
