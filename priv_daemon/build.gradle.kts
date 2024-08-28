plugins { id("apk-conventions") }

apply(from = "$rootDir/configs/foss-pro-flavors.gradle")

android {
  namespace = "com.mirfatif.privdaemon"

  defaultConfig { applicationId = namespace }

  buildFeatures {
    resValues = false

    viewBinding = false
    dataBinding = false
    buildConfig = false
    compose = false
  }
}

dependencies { implementation(project(path = ":priv_library")) }

lsparanoid { variantFilter = { !it.name.startsWith("foss") && it.buildType != "debug" } }
