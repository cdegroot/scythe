name := "Scythe"

version := "0.1"

scalaVersion := "2.9.1"

// Sorry for this. I could not use the scala refactoring libs as published in 
// on scala-tools. You need to grab it, mvn install it, and then it'll work. I
// used git://git.assembla.com/scala-refactoring.git. The code is either a mess
// or I fail to understand it. Here's what I did:
// - Pull the code
// - Change the scala.version in the main pom to 2.9.1
// - Change the ${scala.version} in org.scala-refactoring.library/pom.xml's artifact to 2.9.1
// - Compile with "mvn -Dscala.version=2.9.1 clean install"
// Without the manual edits, I got invalid poms in my local repository (with scala.version unexpanded, and thus
// referring to the default of 2.8.0)
resolvers += "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

libraryDependencies ++= Seq(
    "org.scala-refactoring" %% "org.scala-refactoring" % "0.3.0-SNAPSHOT",
    "org.scala-lang" % "scala-compiler" % "2.9.1",
	"org.scalatest" %% "scalatest"        % "1.6.1" % "test",
	"org.jmock"      % "jmock"            % "2.5.1" % "test",
	"org.jmock"      % "jmock-legacy"     % "2.5.1" % "test",
	"cglib"		 % "cglib-nodep"      % "2.1_3" % "test",
	"org.objenesis"  % "objenesis"        % "1.0"   % "test",
	"junit"		 % "junit"            % "4.8.2" % "test"
)

