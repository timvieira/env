#!/usr/bin/env bash

DEVICE_NAME="VGA-1"
WIDTH="1920"
HEIGHT="1080"
NAME="$WIDTHx$HEIGHT"

# To list available resolutions use
# $ xrandr -q

# To clear existing configuration, which might be getting in your way
# $ rm ~/.config/monitors.xml*

#                                          I have no idea what these parameters mean...
#                              |-----------------------------------------------------------------------\
xrandr --newmode "$NAME"  173.00  "$WIDTH" 2048 2248 2576  "$HEIGHT" 1083 1088 1120 -HSync +VSync

# If the command above succeeds, we can then add the mode.
xrandr --addmode "$DEVICE_NAME" "$NAME"

# Finally, set the device to the new mode
xrandr --output "$DEVICE_NAME" --mode "$NAME"
