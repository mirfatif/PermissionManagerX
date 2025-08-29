plugins { id("android-lib-conventions") }

android {
  namespace = "com.mirfatif.privdaemon"

  buildTypes { release { consumerProguardFiles("daemon-proguard-rules.pro") } }
}

dependencies { implementation(project(path = ":priv_library")) }
