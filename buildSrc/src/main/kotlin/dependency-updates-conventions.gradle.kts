import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

plugins { id("com.github.ben-manes.versions") }

object Color {
  private const val ESC = 27.toChar()
  const val RED_BOLD = "$ESC[1;91m"
  const val ESC_END = "$ESC[0m"
}

val ignoredVersions = mutableSetOf<String>()

fun isStableVersion(version: String): Boolean {
  return ".*(-|.)(rc|beta|alpha)(-|)[0-9]*(|-|.)[0-9.]*$"
      .toRegex()
      .matches(version.lowercase())
      .not()
      .also {
        if (!it && !ignoredVersions.contains(version)) {
          println("${Color.RED_BOLD}Unstable${Color.ESC_END}: $version")
          ignoredVersions.add(version)
        }
      }
}

val buildSrcDependencyUpdatesTaskName = "buildSrcDependencyUpdates"
val buildSrcDir: File
  get() = File(rootDir, "buildSrc")

if (buildSrcDir.exists()) {
  tasks.register<GradleBuild>(buildSrcDependencyUpdatesTaskName) {
    dir = buildSrcDir
    tasks = mutableListOf("dependencyUpdates")
    buildName = "buildSource"
  }
}

tasks.withType<DependencyUpdatesTask> {
  if (buildSrcDir.exists()) {
    dependsOn(buildSrcDependencyUpdatesTaskName)
  }
  rejectVersionIf { !isStableVersion(candidate.version) && isStableVersion(currentVersion) }
}
