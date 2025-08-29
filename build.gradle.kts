import com.diffplug.gradle.spotless.SpotlessExtensionPredeclare

plugins { id("spotless-conventions") }

allprojects { plugins.apply("dependency-updates-conventions") }

spotless { predeclareDeps() }

configure<SpotlessExtensionPredeclare> {
  java { googleJavaFormat(libs.versions.google.java.format.get()) }
  kotlin { ktfmt(libs.versions.ktfmt.get()) }
}
