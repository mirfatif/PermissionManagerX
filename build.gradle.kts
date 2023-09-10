import com.diffplug.gradle.spotless.SpotlessExtensionPredeclare
import java.io.FileInputStream
import java.util.Properties

plugins {
  id("spotless-conventions")
  alias(libs.plugins.task.tree)
}

val localProps = Properties()

localProps.load(FileInputStream(File(rootDir, "local.properties")))

val ndkDir: String =
    File(localProps["sdk.dir"].toString(), "ndk/" + libs.versions.sdk.ndk.get()).absolutePath

allprojects {
  project.extra.set("ndkDir", ndkDir)
  project.extra.set("nativeDir", rootDir.absolutePath + "/native")
  project.extra.set("daemonDex", "daemon.dx")
  project.extra.set("noPro", !File(rootDir, "configs/pro-app.gradle").isFile)
}

// https://github.com/diffplug/spotless/issues/1380#issuecomment-1405392506
spotless { predeclareDeps() }

configure<SpotlessExtensionPredeclare> {
  java { googleJavaFormat() }
  kotlin { ktfmt() }
}
