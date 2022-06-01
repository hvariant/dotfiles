#!/usr/bin/env bash

DATE=`date +%Y-%m-%d-%H-%M-%S`
grim screenshot-${DATE}.png
notify-send "screen shots saved to ~/screenshot-${DATE}.png"
