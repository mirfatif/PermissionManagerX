import org.gradle.accessors.dm.LibrariesForLibs
import org.gradle.kotlin.dsl.the
import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
  id("org.jetbrains.kotlin.jvm")
  id("java-kotlin-conventions")
}

java {
  sourceCompatibility = JavaVersion.VERSION_17
  targetCompatibility = JavaVersion.VERSION_17
}

tasks.withType<KotlinCompile> {
  kotlinOptions.jvmTarget = "17"
  compilerOptions.jvmTarget.set(JvmTarget.JVM_17)
}

val libs = the<LibrariesForLibs>()

dependencies { compileOnly(libs.jetbrains.annotations) }
