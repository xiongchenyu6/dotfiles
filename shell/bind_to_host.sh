#!/usr/bin/env bash

#!/bin/bash
GPU="0000:01:00.0"
AUDIO="0000:01:00.1"
DRIVER="nvidia"

echo "Unbinding GPU from vfio-pci driver..."
echo "$GPU" | sudo tee /sys/bus/pci/devices/$GPU/driver/unbind
echo "$AUDIO" | sudo tee /sys/bus/pci/devices/$AUDIO/driver/unbind

echo "Binding GPU to nvidia driver..."
echo "$GPU" | sudo tee /sys/bus/pci/drivers/$DRIVER/bind
echo "$AUDIO" | sudo tee /sys/bus/pci/drivers/snd_hda_intel/bind
