plugins { id("android-lib-conventions") }

apply(from = "$rootDir/configs/foss-pro-flavors.gradle")

android {
  namespace = "com.mirfatif.privtasks"
  buildTypes { release { consumerProguardFiles("proguard-rules.pro", "proguard-rules-pro.pro") } }

  buildFeatures { aidl = true }
}

dependencies {
  // Just to resolve APIs in editor
  compileOnly(project(path = ":hidden_apis"))
}

fun createTasksForHiddenAPIs() {
  val dir = File(rootDir, "hidden_apis/build/intermediates/aar_main_jar/")

  for (foss in booleanArrayOf(true, false)) {
    for (debug in booleanArrayOf(true, false)) {
      val variant = (if (foss) "Foss" else "Pro") + (if (debug) "Debug" else "Release")

      val task = tasks.named("compile" + variant + "JavaWithJavac").get()
      task.dependsOn(":hidden_apis:sync" + variant + "LibJars")

      var hiddenAPIsJarFile = variant.replaceFirstChar { it.lowercaseChar() }
      hiddenAPIsJarFile += "/sync" + variant + "LibJars" + "/classes.jar"

      task.doFirst {
        this as JavaCompile
        // dependencies.compileOnly() appends the jar but we need to the
        // hidden APIs jar so that to override the Android SDK classes.
        val cp = project.objects.fileCollection()
        cp.from(File(dir, hiddenAPIsJarFile))
        cp.from(classpath)
        classpath = cp
      }
    }
  }
}

afterEvaluate { createTasksForHiddenAPIs() }
