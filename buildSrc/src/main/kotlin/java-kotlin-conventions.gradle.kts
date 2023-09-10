import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins { id("spotless-conventions") }

// https://docs.oracle.com/en/java/javase/17/docs/specs/man/javac.html#examples-of-using--xlint-keys
tasks.withType<JavaCompile> {
  options.compilerArgs.addAll(
      arrayOf("-Xlint:all,-processing,-try,-serial", "-XDstring-concat=inline"))
}

tasks.withType<KotlinCompile> {
  kotlinOptions.freeCompilerArgs +=
      "-Xjavac-arguments=['-Xlint:all,-processing,-try,-serial', '-XDstring-concat=inline']"
  kotlinOptions.freeCompilerArgs += "-Xstring-concat=inline"
}
