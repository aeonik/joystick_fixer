(ns aeonik.joystick-fixer.typing)
"unique-id
❯ ll /dev/input/by-id
total 0
lrwxrwxrwx 1 root root 10 Jul 18 23:13 usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick -> ../event28
lrwxrwxrwx 1 root root  6 Jul 18 23:13 usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick -> ../js4
lrwxrwxrwx 1 root root 10 Jul 18 23:13 usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-event-joystick -> ../event27
lrwxrwxrwx 1 root root  6 Jul 18 23:13 usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick -> ../js3
lrwxrwxrwx 1 root root 10 Jul 18 23:13 usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick -> ../event10
lrwxrwxrwx 1 root root  6 Jul 18 23:13 usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick -> ../js1
lrwxrwxrwx 1 root root 10 Jul 18 23:13 usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick -> ../event26
lrwxrwxrwx 1 root root  6 Jul 18 23:13 usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick -> ../js2
"

"physical-path
❯ ll /dev/input/by-path
total 0
lrwxrwxrwx 1 root root 9 Jul 18 23:13 pci-0000:2c:00.1-usb-0:1.1.2:1.3-event -> ../event9
lrwxrwxrwx 1 root root 10 Jul 18 23:13 pci-0000:2c:00.1-usb-0:1.1.4:1.0-event-joystick -> ../event27
lrwxrwxrwx 1 root root 6 Jul 18 23:13 pci-0000:2c:00.1-usb-0:1.1.4:1.0-joystick -> ../js3
lrwxrwxrwx 1 root root 10 Jul 18 23:13 pci-0000:2c:00.1-usb-0:1.2.2:1.0-event-joystick -> ../event26
lrwxrwxrwx 1 root root 6 Jul 18 23:13 pci-0000:2c:00.1-usb-0:1.2.2:1.0-joystick -> ../js2
lrwxrwxrwx 1 root root 10 Jul 18 23:13 pci-0000:2c:00.1-usb-0:1.4.3:1.0-event-joystick -> ../event10
lrwxrwxrwx 1 root root 6 Jul 18 23:13 pci-0000:2c:00.1-usb-0:1.4.3:1.0-joystick -> ../js1
"

;;Types used in the maps
(def symlink-tuple [:path :symlink])
(def by-id-map [:symlink-tuple :type])
;; eg: [:id "usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-event-joystick" :symlink "/dev/input/event27" :evdev]
;; eg: [:id "usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick" :symlink "/dev/input/js3" :joydev]
(def by-path-map [:symlink-tuple :type])
;; eg :physical-path "pci-0000:2c:00.1-usb-0:1.1.4:1.0-event-joystick" :symlink "/dev/input/event27 :evdev"
;; eg :physical-path "pci-0000:2c:00.1-usb-0:1.1.4:1.0-joystick" :symlink "/dev/input/js3 :joydev"
(defrecord joystick-map [name evdev-id-path evdev-symlink evdev-physical-path joydev-id-path joydev-symlink joydev-physical-path])

{:name "VPC_Stick_MT-50CM2"
 :evdev-id-path "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick",
 :evdev-symlink "/dev/input/event27",
 :evdev-physical-path"/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.3:1.0-event-joystick",
 :joydev-id-path "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick",
 :joydev-symlink "/dev/input/js3",
 :joydev-physical-path "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.3:1.0-joystick",
 :pci-address "0000:2c:00.1",
 :usb-address "0:1.4.3:1.0"}

;; High level steps
;; 1. Get a list of all the joystick devices from /dev/input/by-id because we can look up name of Joysticks this way
;; 2. Get the destination that these symlinks point to
;; 3. Search /dev/by-path for the symlinks using the destination from step 2 as a joining key
;; 4. Merge all the information
;; 5. Extract name from id, and pci, and usb address (when this step occurs doesn't really matter.)
