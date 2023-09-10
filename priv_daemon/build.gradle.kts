plugins { id("apk-conventions") }

apply(from = "$rootDir/configs/foss-pro-flavors.gradle")

android {
  namespace = "com.mirfatif.privdaemon"

  defaultConfig { applicationId = namespace }

  buildFeatures {
    buildConfig = false
    resValues = false
  }
}

dependencies { implementation(project(path = ":priv_library")) }

lsparanoid { variantFilter = { !it.name.startsWith("foss") } }
