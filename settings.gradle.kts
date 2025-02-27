@Suppress("UnstableApiUsage")
dependencyResolutionManagement {
  repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)

  repositories {
    mavenLocal()
    mavenCentral()
    google()
    gradlePluginPortal()
    maven { url = uri("https://jitpack.io") } // For libadb-android
  }
}

include(":hidden_apis")

include(":priv_library")

include(":priv_daemon")

include(":app")
