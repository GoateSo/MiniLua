lazy val root = project
  .in(file("."))
  .settings(
    name := "LangPlayground",
    version := "0.1.0",
    scalaVersion := "3.3.7",
    libraryDependencies ++= Seq(
        "org.scalameta" %% "munit"        % "0.7.29" % Test,
        "com.lihaoyi"   %% "mainargs"     % "0.7.7",
        "com.lihaoyi"   %% "pprint"       % "0.7.0",
        "com.lihaoyi"   %% "os-lib"       % "0.9.1",
        "com.lihaoyi"   %% "sourcecode"   % "0.3.0",
        "com.lihaoyi"   %% "fastparse"    % "3.0.2"
    )
  )

addCommandAlias(
  "testCoverage",
  "clean ; coverage ; test ; coverageReport"
)