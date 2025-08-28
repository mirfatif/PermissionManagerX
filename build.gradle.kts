import com.diffplug.gradle.spotless.SpotlessExtensionPredeclare
import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

plugins {
  id("spotless-conventions")
  alias(libs.plugins.gradle.versions)
}

spotless { predeclareDeps() }

configure<SpotlessExtensionPredeclare> {
  java { googleJavaFormat(libs.versions.google.java.format.get()) }
  kotlin { ktfmt(libs.versions.ktfmt.get()) }
}

fun isStableVersion(version: String): Boolean {
  return ".*-(rc|beta|alpha)(|-)[0-9]*$".toRegex().matches(version.lowercase()).not()
}

tasks.register<GradleBuild>("buildSrcDependencyUpdates") {
  dir = File(rootDir, "buildSrc")
  tasks = mutableListOf("dependencyUpdates")
  buildName = "buildSource"
}

tasks.withType<DependencyUpdatesTask> {
  dependsOn("buildSrcDependencyUpdates")
  rejectVersionIf { !isStableVersion(candidate.version) && isStableVersion(currentVersion) }
}
