(ns aeonik.controlmap.core-test
  (:require
   [aeonik.controlmap.core :as core]
   [aeonik.controlmap.state :as state]
   [clojure.test :refer :all]))

(def ctx (state/build-context))

(deftest instance-mappings-test
  (is (= 1 1))
  (is (true? true)))

(deftest svg-id->display-name-test
  (let [test-id (-> ctx :registry :matchers first :svg-id)
        expected-display (-> ctx :registry :matchers first :display)]
    (is (= expected-display
           (core/svg-id->display-name ctx test-id)))))
