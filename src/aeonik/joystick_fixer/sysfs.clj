(ns aeonik.joystick-fixer.sysfs
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [clojure.pprint :refer [pprint]]
            [malli.core :as m]
            [ubergraph.core :as g])

  (:import [java.nio.file Files Paths]
           (org.apache.commons.io FileUtils)))

"❯ udevadm info --no-pager /dev/input/event27\nP: /devices/pci0000:00/0000:00:01.2/0000:20:00.0/0000:21:08.0/0000:2c:00.1/usb3/3-1/3-1.1/3-1.1.4/3-1.1.4:1.0/0003:3344:025D.0012/input/input29/event27\nM: event27\nR: 27\nU: input\nD: c 13:91\nN: input/event27\nL: 0\nS: input/by-path/pci-0000:2c:00.1-usb-0:1.1.4:1.0-event-joystick\nS: input/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-event-joystick\nE: DEVPATH=/devices/pci0000:00/0000:00:01.2/0000:20:00.0/0000:21:08.0/0000:2c:00.1/usb3/3-1/3-1.1/3-1.1.4/3-1.1.4:1.0/0003:3344:025D.0012/input/input29/event27\nE: DEVNAME=/dev/input/event27\nE: MAJOR=13\nE: MINOR=91\nE: SUBSYSTEM=input\nE: USEC_INITIALIZED=8437788\nE: ID_INPUT=1\nE: ID_INPUT_JOYSTICK=1\nE: ID_BUS=usb\nE: ID_MODEL=VPC_SharKa-50_Panel\nE: ID_MODEL_ENC=VPC\\x20SharKa-50\\x20Panel\nE: ID_MODEL_ID=025d\nE: ID_SERIAL=VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF\nE: ID_SERIAL_SHORT=FF\nE: ID_VENDOR=VIRPIL_Controls_20220720\nE: ID_VENDOR_ENC=VIRPIL\\x20Controls\\x2020220720\nE: ID_VENDOR_ID=3344\nE: ID_REVISION=0100\nE: ID_TYPE=hid\nE: ID_USB_MODEL=VPC_SharKa-50_Panel\nE: ID_USB_MODEL_ENC=VPC\\x20SharKa-50\\x20Panel\nE: ID_USB_MODEL_ID=025d\nE: ID_USB_SERIAL=VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF\nE: ID_USB_SERIAL_SHORT=FF\nE: ID_USB_VENDOR=VIRPIL_Controls_20220720\nE: ID_USB_VENDOR_ENC=VIRPIL\\x20Controls\\x2020220720\nE: ID_USB_VENDOR_ID=3344\nE: ID_USB_REVISION=0100\nE: ID_USB_TYPE=hid\nE: ID_USB_INTERFACES=:030000:\nE: ID_USB_INTERFACE_NUM=00\nE: ID_USB_DRIVER=usbhid\nE: ID_PATH=pci-0000:2c:00.1-usb-0:1.1.4:1.0\nE: ID_PATH_TAG=pci-0000_2c_00_1-usb-0_1_1_4_1_0\nE: ID_FOR_SEAT=input-pci-0000_2c_00_1-usb-0_1_1_4_1_0\nE: LIBINPUT_DEVICE_GROUP=3/3344/25d:usb-0000:2c:00.1-1.1\nE: DEVLINKS=/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.1.4:1.0-event-joystick /dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-event-joystick\nE: TAGS=:seat:uaccess:\nE: CURRENT_TAGS=:seat:uaccess:\n"

(defn udev-info [device-path]
  (let [udevadm (sh/sh "udevadm" "info" "--no-pager" device-path)
        udevadm-out (str/trim (:out udevadm))]
    (->> (str/split udevadm-out #"\n")
         (map #(str/split % #":" 2))  ;; Split only on the first `:`
         (map (fn [[k v]] {(str/trim k) [(str/trim (or v ""))]})) ;; Avoid nil in trim function and put into map
         (apply merge-with concat)))) ;; Use merge-with concat to combine values into a list

(udev-info "/dev/input/event27")