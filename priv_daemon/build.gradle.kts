plugins { id("apk-conventions") }

android {
  namespace = "com.mirfatif.privdaemon"

  defaultConfig { applicationId = namespace }

  buildFeatures {
    resValues = false

    viewBinding = false
    dataBinding = false
    buildConfig = false
  }
}

dependencies { implementation(project(path = ":priv_library")) }
