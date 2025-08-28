import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins { id("spotless-conventions") }

tasks.withType<JavaCompile> { options.compilerArgs.addAll(arrayOf("-Xlint:all")) }

tasks.withType<KotlinCompile> {
  compilerOptions.freeCompilerArgs.addAll("-Xjavac-arguments=['-Xlint:all']")
}
