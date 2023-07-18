{:paths ["src" "resources"]
 :deps  {org.clojure/clojure   {:mvn/version "1.11.1"}
         org.clojure/tools.cli {:mvn/version "RELEASE"}
         org.clojure/tools.reader {:mvn/version "1.3.6"}
         org.clojure/tools.analyzer.jvm {:mvn/version "1.2.2"}
         commons-io/commons-io {:mvn/version "2.11.0"}
         }
 :aliases
 {:run-m
  {:jvm-opts  ["-Xmx1G"]
   :main-opts ["-m" "aeonik.joystick_fixer"
               "--directory" "/run/media/dave/backup_nvme"]}
  :run-x {:ns-default aeonik.joystick-fixer.core
          :exec-fn    main
          :exec-args  {:name "Clojure"}}
  :build {:deps       {io.github.seancorfield/build-clj
                       {:git/tag   "v0.8.2" :git/sha "0ffdb4c"
                        ;; since we're building an app uberjar, we do not
                        ;; need deps-deploy for clojars.org deployment:
                        :deps/root "slim"}}
          :ns-defjoy      {:jvm-omain
          :extra-paths ["test"]
          :extra-deps  {org.clojure/test.check {:mvn/version "1.1.1"}
                        io.github.cognitect-labs/test-runner
                        {:git/tag "v0.5.0" :git/sha "48c3c67"}}}}}
 
