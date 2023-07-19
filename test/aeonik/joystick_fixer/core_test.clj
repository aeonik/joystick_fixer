(ns aeonik.joystick-fixer.core-test
  (:require [clojure.test :refer :all])
  (:require [aeonik.joystick-fixer.core :refer [get-joystick-names]]))


(def example-joystick-map {:name "VPC Throttle MT-50CM3"
                           :evdev-info {:evdev-id-path "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"
                                        :evdev-physical-path "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-event-joystick"
                                        :evdev-symlink-target "/dev/input/event5"}
                           :joydev-info {:joydev-id-path "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick"
                                         :joydev-physical-path "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-joystick"
                                         :joydev-symlink-target "/dev/input/js1"}
                           :pci-address "0000:2c:00.1"
                           :usb-address "0:1.4.4:1.0"})

(def example-file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick")

(deftest get-joystick-names-test

  )
