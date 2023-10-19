import org.gradle.accessors.dm.LibrariesForLibs

plugins {
  id("java-library")
  id("java-lib-conventions")
}

val libs = the<LibrariesForLibs>()

dependencies { compileOnly(libs.lsparanoid.core) }
