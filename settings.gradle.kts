@Suppress("UnstableApiUsage")
dependencyResolutionManagement {
  repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)

  repositories {
    maven { url = uri("/tmp/maven-repo/") }

    google()
    mavenCentral()

    // For libadb-android
    maven { url = uri("https://jitpack.io") }
  }
}

if (File(rootDir, "configs/settings-pro.gradle.kts").isFile) {
  apply(from = "configs/settings-pro.gradle.kts")
}

include(":hidden_apis")

include(":priv_library")

include(":priv_daemon")

include(":app")
