#!/usr/bin/env bash

list=(ethlint)
for package in "${list[@]}"; do
	npm -g i "$package"
done
