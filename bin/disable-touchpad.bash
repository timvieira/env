#!/usr/bin/env bash

# http://xpapad.wordpress.com/2010/01/04/fully-disabling-touchpad-in-ubuntu-9-10-karmic-koala/

# To figure out what your mouse might try grepping xinput
# $ xinput list | grep -i Touchpad

NAME="SynPS/2 Synaptics TouchPad"

# 0: disable; 1: enable
ENABLE=0

# enable/disable the device
xinput set-int-prop "$NAME" "Device Enabled" 8 $ENABLE  # I don't know what the deal with 8 is...

# shut down mousetweaks...
mousetweaks --shutdown
