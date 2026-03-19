.PHONY: demo dev prod

demo:
	clj -M:demo

dev:
	DEBUG=true clj -M:dev

local:
	lein clean && lein with-profile +macos,+prod trampoline run

uberjar:
	lein clean && clojure -T:build uber

run-jar:
	java --enable-native-access=ALL-UNNAMED -cp target/uber.jar clojure.main -m inkstain.core

jar-test: uberjar run-jar
	echo "DONE"

uberjar-deps:
	lein clean && clojure -T:build uber

prod-deps: uberjar-deps prod-mac # prod-win prod-linux ## Others can only be built on respective systems, so test later
	echo "DONE"

prod: uberjar prod-mac # prod-win prod-linux ## Others can only be built on respective systems, so test later
	echo "DONE"
prod-mac:
	export JAVA_HOME=`/usr/libexec/java_home` && \
	cd releases && \
	$$JAVA_HOME/bin/jpackage --name "Mechstain" \
	--input ../target --main-jar uber.jar \
	--main-class clojure.main --arguments -m --arguments inkstain.core \
	--java-options --enable-native-access=ALL-UNNAMED
prod-win:
	export JAVA_HOME=`/usr/libexec/java_home` && \
    	cd releases && \
    	$$JAVA_HOME/bin/jpackage --name "Mechstain" \
    	--input ../target --main-jar uber.jar \
    	--main-class clojure.main --arguments -m --arguments inkstain.core \
    	--type exe
prod-linux:
	export JAVA_HOME=`/usr/libexec/java_home` && \
    	cd releases && \
    	$$JAVA_HOME/bin/jpackage --name "Mechstain" \
    	--input ../target --main-jar uber.jar \
    	--main-class clojure.main --arguments -m --arguments inkstain.core \
    	--type pkg --linux-shortcut

bin:
	lein with-profile +macos,+dev trampoline bin

jwm:
	cd checkouts/JWM && \
	JAVA_HOME=`/usr/libexec/java_home` python3 script/install.py && \
	mvn install:install-file -Dfile=target/jwm-0.0.0-SNAPSHOT.jar -DpomFile=target/maven/META-INF/maven/io.github.humbleui/jwm/pom.xml

sleep:
	sleep 5