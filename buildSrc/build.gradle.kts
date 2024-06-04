import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

plugins {
  `kotlin-dsl`
  id("com.diffplug.spotless").version("6.25.0").apply(true)
  id("com.github.ben-manes.versions").version("0.51.0").apply(true)
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

fun isStableVersion(version: String): Boolean {
  return ".*-(rc|beta|alpha)(|-)[0-9]*$".toRegex().matches(version.lowercase()).not()
}

tasks.withType<DependencyUpdatesTask> {
  rejectVersionIf { !isStableVersion(candidate.version) && isStableVersion(currentVersion) }
}
