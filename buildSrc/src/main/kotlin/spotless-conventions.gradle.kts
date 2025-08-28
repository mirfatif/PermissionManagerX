import org.gradle.accessors.dm.LibrariesForLibs
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins { id("com.diffplug.spotless") }

val libs = the<LibrariesForLibs>()

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

tasks.withType<JavaCompile> { dependsOn("spotlessCheck") }

tasks.withType<KotlinCompile> { dependsOn("spotlessCheck") }

afterEvaluate { tasks.findByName("preBuild")?.dependsOn("spotlessCheck") }
