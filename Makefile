.PHONY: demo dev prod

demo:
	clj -M:demo

dev:
	DEBUG=true clj -M:dev

local:
	lein clean && lein with-profile +macos,+prod trampoline run

uberjar:
	lein clean && lein uberjar

run-jar:
	java -jar target/fruit-economy-standalone.jar

jar-test: uberjar run-jar
	echo "DONE"

uberjar-deps:
	lein clean && clojure -A:uberjar

prod-deps: uberjar-deps prod-mac # prod-win prod-linux ## Others can only be built on respective systems, so test later
	echo "DONE"

prod: uberjar prod-mac # prod-win prod-linux ## Others can only be built on respective systems, so test later
	echo "DONE"
prod-mac:
	export JAVA_HOME=`/usr/libexec/java_home` && \
	cd releases && \
	$$JAVA_HOME/bin/jpackage --name "Fruit Economy" \
	--input ../target --main-jar fruit-economy-standalone.jar \
	--main-class clojure.main --arguments -m --arguments fruit-economy.core \
prod-win:
	export JAVA_HOME=`/usr/libexec/java_home` && \
    	cd releases && \
    	$$JAVA_HOME/bin/jpackage --name "Fruit Economy" \
    	--input ../target --main-jar fruit-economy-standalone.jar \
    	--main-class clojure.main --arguments -m --arguments fruit-economy.core \
    	--type exe
prod-linux:
	export JAVA_HOME=`/usr/libexec/java_home` && \
    	cd releases && \
    	$$JAVA_HOME/bin/jpackage --name "Fruit Economy" \
    	--input ../target --main-jar fruit-economy-standalone.jar \
    	--main-class clojure.main --arguments -m --arguments fruit-economy.core \
    	--type pkg --linux-shortcut

bin:
	lein with-profile +macos,+dev trampoline bin

jwm:
	cd checkouts/JWM && \
	JAVA_HOME=`/usr/libexec/java_home` python3 script/install.py && \
	mvn install:install-file -Dfile=target/jwm-0.0.0-SNAPSHOT.jar -DpomFile=target/maven/META-INF/maven/io.github.humbleui/jwm/pom.xml

sleep:
	sleep 5