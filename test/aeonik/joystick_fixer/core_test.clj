(ns aeonik.joystick-fixer.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [babashka.fs :as fs]
            [aeonik.joystick-fixer.core :refer [get-joystick-names
                                                filename->joystick-name
                                                file->symlink
                                                split-usb-pci
                                                get-corresponding-path
                                                by-id->by-path
                                                symlink->target
                                                by-path->by-id
                                                search-path
                                                regex-search->id-symlinks
                                                correlate-joystick-name
                                                symlink-target->evdev-info
                                                symlink-target->joydev-info
                                                symlink-target->info
                                                join-evdev+joydev-info
                                                get-joystick-info
                                                process-all-joysticks
                                                sort-joysticks
                                                transform-data
                                                ]]))


(def resources
  [
   {:name "event20"}
   {:name "js1"}
   {:name "by-id"
    :content [
              {:name "usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"}
              {:name "usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick"}
              {:name "usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick"}
              {:name "usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-event-joystick"}
              ]
    }
   {:name "event21"}
   {:name "by-path"
    :content [
              {:name "pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick"}
              {:name "pci-0000:2c:00.1-usb-0:1.4.4:1.0-event-joystick"}
              {:name "pci-0000:2c:00.1-usb-0:1.4.2:1.0-event-joystick"}
              {:name "pci-0000:2c:00.1-usb-0:1.4.4:1.0-joystick"}
              ]
    }
   {:name "js0"}
   ]
  )


(comment (def example-joystick-map {:name        "VPC Throttle MT-50CM3"
                                    :evdev-info  {:evdev-id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"
                                                  :evdev-physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-event-joystick"
                                                  :evdev-symlink-target "/dev/input/event5"}
                                    :joydev-info {:joydev-id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick"
                                                  :joydev-physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-joystick"
                                                  :joydev-symlink-target "/dev/input/js1"}
                                    :pci-address "0000:2c:00.1"
                                    :usb-address "0:1.4.4:1.0"}))

(def example-joystick-map
  {:name "usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"
   :evdev-info {:evdev-id-path "by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"
                :evdev-physical-path "by-path/pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick"
                :evdev-symlink-target "event20"}
   :joydev-info {:joydev-id-path "by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"
                 :joydev-physical-path "by-path/pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick"
                 :joydev-symlink-target "js0"}
   :pci-address "0000:2c:00.1"
   :usb-address "usb-0:1.4.2:1.0"})


(def device-paths {:by-id "test/resources/by-id"
                   :by-path "test/resources/by-path"})

;; List files from device-paths

(deftest get-joystick-names-test
  (testing "get-joystick-names"
    (is (= (get-joystick-names device-paths)
           '("L-VPC_Stick_MT-50CM2"
              "VPC_SharKa-50_Panel"))))
  )


(deftest filename->joystick-name-test
  (testing "filename->joystick-name"
    (is (= (filename->joystick-name "usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick")
           "VPC_SharKa-50_Panel"))
    (is (= (filename->joystick-name "usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick")
           "L-VPC_Stick_MT-50CM2")))
  )

(deftest file->symlink-test
  (testing "file->symlink"
    (is (= (file->symlink (io/file "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"))
           {:link   "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"
            :target "/home/dave/Projects/joystick_fixer/test/resources/js1"}
    (is (= (file->symlink (io/file "test/resources/by-path/pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick"))
           {:link   "test/resources/by-path/pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick"
            :target "/home/dave/Projects/joystick_fixer/test/resources/js1"}
           )))
  )))

(deftest split-usb-pci-test
  (testing "split-usb-pci"
    (is (= (split-usb-pci "pci-0000:00:14.0-usb-0:2:1.0-event-joystick")
           {:pci-address "0000:00:14.0"
            :usb-address "0:2:1.0"}))))


(comment (get-corresponding-path (:by-path device-paths) (file->symlink (io/file by-id-file))))
(deftest get-corresponding-path-test
  (testing "get-corresponding-path"
    (println (:by-path device-paths))
    (println (file->symlink (io/file "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick")))
    (println (get-corresponding-path (:by-path device-paths) (file->symlink (io/file "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"))))

    (is (= (get-corresponding-path (:by-path device-paths) (file->symlink (io/file "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick")))
           "test/resources/by-path/pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick"))
    (is (= (get-corresponding-path (:by-id device-paths) (file->symlink (io/file "test/resources/by-path/pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick")))
           "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"))
  ))

(deftest by-id->by-path-test
  (testing "by-id->by-path"
    (is (= (by-id->by-path "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick")
           "test/resources/by-path/pci-0000:2c:00.1-usb-0:1.4.2:1.0-joystick"))
  ))

(deftest by-path->by-id-test
  )

(deftest search-path-test
  
  )

(deftest regex-search->id-symlinks-test
  )

(deftest correlate-joystick-name-test
  )

(deftest symlink-target->evdev-info-test
  )

(deftest symlink-target->joydev-info-test
  )

(deftest symlink-target->info-test
  )

(deftest join-evdev+joydev-info-test
  )

(deftest get-joystick-info-test
  )

(deftest process-all-joysticks-test
  )

(deftest sort-joysticks-test
  )

(deftest transform-data-test
  )


(deftest symlink->target-test
  (testing "symlink->target"
    (is (= (symlink->target (io/file "test/resources/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"))
           "/home/dave/Projects/joystick_fixer/test/resources/js1"))))
