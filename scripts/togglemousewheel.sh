#!/bin/bash
xinput list-props 'pointer:Logitech MX Ergo' | grep -q 'libinput Natural Scrolling Enabled.*1$' && xinput set-prop 'pointer:Logitech MX Ergo' 'libinput Natural Scrolling Enabled' 0 || xinput set-prop 'pointer:Logitech MX Ergo' 'libinput Natural Scrolling Enabled' 1
