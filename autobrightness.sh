#!/bin/sh
# Adjust brightness of backlights based on power source

case $1 in
    # On battery
    true)
        # Dim keyboard backlight
        #echo 0 > /sys/class/leds/asus::kbd_backlight/brightness
        # Dim screen backlight
#        expr `cat /sys/class/backlight/intel_backlight/max_brightness` / 100 > \
#            /sys/class/backlight/intel_backlight/brightness
        echo 244 > /sys/class/backlight/intel_backlight/brightness

    ;;

    # On AC
    false)
        # Dim keyboard backlight
        #cat /sys/class/leds/asus::kbd_backlight/max_brightness > \
        #    /sys/class/leds/asus::kbd_backlight/brightness
        # Dim screen backlight
#        cat /sys/class/backlight/intel_backlight/max_brightness > \
#            /sys/class/backlight/intel_backlight/brightness
        echo 1862 > /sys/class/backlight/intel_backlight/brightness

    ;;
esac

return 0
