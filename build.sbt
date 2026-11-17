ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "io.github.ondeoma"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / licenses     := List("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / homepage     := Some(url("https://github.com/Ondeoma/skereru"))
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports")

val http4sV = "1.0.0-M49"
val circeV  = "0.14.16"
val doobieV = "1.0.0-RC12"
val ceV     = "3.7.1"
val fs2V    = "3.14.0"
val munitV  = "1.3.6"
val awsV    = "2.30.38"
val flywayV = "11.3.0"
val otel4sV = "1.2.0"
val otelV   = "1.68.0"
val otelInstV = "2.33.1-alpha"

lazy val commonTest = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"             % munitV  % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test
  ),
  Test / fork := true
)

// バックエンドと画面(Scala.js)で共有する API の型
lazy val shared = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full) // 日時(Timestamp)だけ JVM と JS で実装が違う
  .in(file("modules/shared"))
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core"   % circeV,
      "io.circe" %%% "circe-parser" % circeV
    )
  )

// 画面: Scalactive の SPA（frontend/ の Vite から読み込む）
lazy val web = (project in file("modules/web"))
  .enablePlugins(ScalaJSPlugin, ScalactivePlugin)
  .dependsOn(shared.js)
  .settings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "io.github.ondeoma" %%% "scalactive" % "0.6.2"
    )
  )

// ドメインの純粋なロジック（MIME・IMAP の構文・迷惑メールの判定・SPF/DKIM/DMARC）
lazy val core = (project in file("modules/core"))
  .dependsOn(shared.jvm)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe"         %% "circe-parser" % circeV,
      "org.eclipse.angus" % "angus-mail"   % "2.0.3",
      "org.jsoup"         % "jsoup"        % "1.21.2",
      // 組織ドメイン（DMARC）の判定に、Public Suffix List を使う
      "com.google.guava"  % "guava"        % "33.2.1-jre",
      // 添付ファイル（PDF）の本文を検索できるようにする
      "org.apache.pdfbox" % "pdfbox"       % "3.0.4"
    )
  )

lazy val db = (project in file("modules/db"))
  .dependsOn(core, observe)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat"  %% "doobie-core"           % doobieV,
      "org.tpolecat"  %% "doobie-hikari"         % doobieV,
      "org.tpolecat"  %% "doobie-postgres"       % doobieV,
      "org.tpolecat"  %% "doobie-postgres-circe" % doobieV,
      "org.typelevel" %% "cats-effect"           % ceV,
      "org.postgresql" % "postgresql"            % "42.7.14",
      "org.flywaydb"   % "flyway-core"           % flywayV,
      "org.flywaydb"   % "flyway-database-postgresql" % flywayV % Runtime
    )
  )

lazy val auth = (project in file("modules/auth"))
  .dependsOn(core)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-client" % http4sV,
      "org.http4s" %% "http4s-circe"  % http4sV
    )
  )

// メッセージの本体の保存先（PostgreSQL または S3 互換ストレージ）
lazy val storage = (project in file("modules/storage"))
  .dependsOn(db)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      // netty-nio-client は依存ごとに外す（プロジェクト全体で外すと下流の解決で AWS SDK 自体が落ちることがある）
      ("software.amazon.awssdk" % "s3"            % awsV).exclude("software.amazon.awssdk", "netty-nio-client"),
      "software.amazon.awssdk"  % "apache-client" % awsV
    )
  )

// 観測（OpenTelemetry: otel4s-oteljava）. ログ・メトリクス・トレースを OTLP で skorimako に送る
lazy val observe = (project in file("modules/observe"))
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "otel4s-oteljava"                 % otel4sV,
      "org.typelevel"   %% "otel4s-oteljava-context-storage" % otel4sV,
      "org.typelevel"   %% "otel4s-instrumentation-metrics"  % otel4sV,
      "io.opentelemetry" % "opentelemetry-exporter-otlp"     % otelV,
      "io.opentelemetry.instrumentation" % "opentelemetry-logback-appender-1.0" % otelInstV,
      "io.opentelemetry.instrumentation" % "opentelemetry-logback-mdc-1.0"      % otelInstV,
      "io.opentelemetry.instrumentation" % "opentelemetry-runtime-telemetry"    % otelInstV,
      "org.http4s"      %% "http4s-server"                   % http4sV,
      "org.http4s"      %% "http4s-client"                   % http4sV,
      "org.http4s"      %% "http4s-circe"                    % http4sV,
      "org.http4s"      %% "http4s-dsl"                      % http4sV,
      "ch.qos.logback"   % "logback-classic"                 % "1.5.18",
      "org.tpolecat"    %% "doobie-core"                     % doobieV
    )
  )

// プッシュ通知（PWA. Web Push の暗号化と VAPID は JDK の暗号の機能だけで実装）
lazy val push = (project in file("modules/push"))
  .dependsOn(db, observe)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-client" % http4sV,
      "io.circe"   %% "circe-core"    % circeV
    )
  )

// メールのプロトコル（SMTP・IMAP・POP3）、配送、迷惑メールのフィルタ
lazy val mail = (project in file("modules/mail"))
  .dependsOn(core, db % "compile;test->test", auth, storage, observe)
  .settings(commonTest)

lazy val api = (project in file("modules/api"))
  .dependsOn(core, db % "compile;test->test", auth, storage, mail, shared.jvm, observe, push)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl"    % http4sV,
      "org.http4s" %% "http4s-circe"  % http4sV,
      "org.http4s" %% "http4s-server" % http4sV
    )
  )

lazy val app = (project in file("modules/app"))
  .dependsOn(api)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-ember-server" % http4sV,
      "org.http4s"    %% "http4s-ember-client" % http4sV,
      "ch.qos.logback" % "logback-classic"     % "1.5.18",
      "org.typelevel" %% "log4cats-slf4j"      % "2.8.0"
    ),
    Compile / mainClass := Some("skereru.app.Main"),
    fork := true
  )

lazy val root = (project in file("."))
  .aggregate(shared.jvm, shared.js, web, core, db, auth, storage, observe, push, mail, api, app)
  .settings(name := "skereru", publish / skip := true)
