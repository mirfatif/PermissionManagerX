import gradle.kotlin.dsl.accessors._269897c053523d6d13cc4af87bdc794f.compileOnly
import org.gradle.accessors.dm.LibrariesForLibs

plugins {
  id("java-library")
  id("java-lib-conventions")
}

val libs = the<LibrariesForLibs>()

dependencies { compileOnly(libs.lsparanoid.core) }
