(defproject fruit-economy "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.specs.alpha "0.2.62"]
                 [org.clojure/spec.alpha "0.3.218"]
                 [org.clojure/test.check "1.1.1"]
                 [org.clojure/data.priority-map "1.1.0"]
                 ;[org.projectlombok/lombok "1.18.22" :scope "provided"]
                 ;[org.jetbrains/annotations "20.1.0"]
                 [environ "1.2.0"]
                 [io.github.humbleui/types "0.1.2" :classifier "clojure"]
                 [io.github.humbleui/jwm "0.4.5" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-shared "0.105.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-windows "0.105.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-linux "0.105.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-macos-x64 "0.105.0" :exclusions [io.github.humbleui/types]]
                 [io.github.humbleui/skija-macos-arm64 "0.105.0" :exclusions [io.github.humbleui/types]]
                 [humbleui "0195b208173ed1616a19be49e3914d11d41fd531"]
                 [datascript "1.3.13"]
                 [denistakeda/posh "0.5.9"]
                 [org.clojars.quoll/asami "2.3.0"]
                 [org.clojars.quoll/asami-loom "0.3.1"]
                 [org.clojars.quoll/naga "0.3.15"]
                 [aysylu/loom "1.0.2"]

                 [net.sekao/odoyle-rules "0.11.0"]

                 [net.mikera/clisk "0.11.0"]
                 [ubergraph "0.8.2"]
                 [hiccup "1.0.5"]
                 [dali "1.0.2"]
                 [guru.nidi/graphviz-java "0.18.1"]
                 [com.squidpony/squidlib-util "3.0.4"]
                 [org.jfree/jfreechart "1.5.3"]

                 [com.taoensso/timbre "5.2.1"]
                 [com.taoensso/tufte "2.2.0"]

                 #_[overtone "0.10.6"]]
  :java-source-paths ["src/java" "test/java"]
  :plugins [[reifyhealth/lein-git-down "0.4.1"]
            [lein-environ "1.2.0"]]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {humbleui {:coordinates HumbleUI/HumbleUI}}
  :middleware [lein-git-down.plugin/inject-properties]
  :main fruit-economy.core
  :uberjar-name "fruit-economy-standalone.jar"
  :env {:game-version :project/version}
  :profiles {:macos {:jvm-opts  ["-XstartOnFirstThread"]}
             :dev {:jvm-opts  ["-Djdk.attach.allowAttachSelf" "-XX:+UnlockDiagnosticVMOptions" "-XX:+DebugNonSafepoints" #_"-ea"]
                   :source-paths ["dev"]
                   :env {:debug? "true"}
                   :dependencies  [[nrepl/nrepl "0.9.0"]
                                   [com.clojure-goes-fast/clj-async-profiler "0.5.1" #_"1.0.0-alpha1"]
                                   [djblue/portal "0.26.0"]
                                   [hashp "0.2.1"]
                                   [spyscope "0.1.6"]
                                   [pjstadig/humane-test-output "0.11.0"]
                                   [com.github.jpmonettas/flow-storm-dbg "2.2.99"]
                                   [com.github.jpmonettas/flow-storm-inst "2.2.99"]]
                   :injections [(require 'hashp.core)
                                (require 'spyscope.core)
                                (require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
                   :main-opts   ["-m" "user" "--interactive"]}
             :prod {:env {:debug? "false"}}}
  :repl-options {:init-ns fruit-economy.core})
