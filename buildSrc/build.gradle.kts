import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

plugins {
  `kotlin-dsl`
  id("com.diffplug.spotless").version("7.2.1").apply(true)
  id("com.github.ben-manes.versions").version("0.52.0").apply(true)
}

dependencies {
  implementation(libs.plugin.android.application)
  implementation(libs.plugin.android.library)
  implementation(libs.plugin.jetbrains.kotlin.android)

  implementation(libs.plugin.spotless)

  // Make version catalogs available to convention plugins
  // https://github.com/gradle/gradle/issues/15383
  implementation(files(libs.javaClass.superclass.protectionDomain.codeSource.location))
}

spotless {
  java {
    target("src/**/*.java")
    googleJavaFormat(libs.versions.google.java.format.get())
  }

  kotlin {
    target("src/**/*.kt", "src/**/*.kts", "*.kts")
    ktfmt(libs.versions.ktfmt.get())
  }
}

tasks.named("jar").get().dependsOn("spotlessApply")

fun isStableVersion(version: String): Boolean {
  return ".*-(rc|beta|alpha)(|-)[0-9]*$".toRegex().matches(version.lowercase()).not()
}

tasks.withType<DependencyUpdatesTask> {
  rejectVersionIf { !isStableVersion(candidate.version) && isStableVersion(currentVersion) }
}
