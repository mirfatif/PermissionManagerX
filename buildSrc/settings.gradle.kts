@Suppress("UnstableApiUsage")
dependencyResolutionManagement {
  repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)

  repositories {
    mavenLocal()
    mavenCentral()
    google()
    gradlePluginPortal()
  }

  versionCatalogs { create("libs") { from(files("../gradle/libs.versions.toml")) } }
}
