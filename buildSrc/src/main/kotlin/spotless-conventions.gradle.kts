import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins { id("com.diffplug.spotless") }

spotless {
  java {
    target("src/**/*.java")
    googleJavaFormat()
  }

  kotlin {
    target("src/**/*.kt", "src/**/*.kts", "*.kts")
    ktfmt()
  }
}

tasks.withType<JavaCompile> { dependsOn("spotlessCheck") }

tasks.withType<KotlinCompile> { dependsOn("spotlessCheck") }

afterEvaluate { tasks.findByName("preBuild")?.dependsOn("spotlessCheck") }
