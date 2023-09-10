plugins {
  `kotlin-dsl`
  id("com.diffplug.spotless").version("6.21.0").apply(true)
}

// This block goes under pluginManagement {} in settings.gradle.kts
repositories {
  maven { url = uri("/tmp/maven-repo/") }
  mavenCentral()
  google()
  gradlePluginPortal()
}

// This is a replacement of plugins {} block in root build.gradle.kts
dependencies {
  implementation(libs.plugin.android.application)
  implementation(libs.plugin.android.library)
  implementation(libs.plugin.jetbrains.kotlin.android)

  implementation(libs.plugin.spotless)
  implementation(libs.plugin.lsparanoid)

  // Make version catalogs available to convention plugins
  // https://github.com/gradle/gradle/issues/15383
  implementation(files(libs.javaClass.superclass.protectionDomain.codeSource.location))
}

spotless {
  java {
    target("src/**/*.java")
    googleJavaFormat()
  }

  kotlin {
    target("src/**/*.kt", "src/**/*.kts", "*.kts")
    ktfmt()
  }
}

tasks.named("jar").get().dependsOn("spotlessApply")
