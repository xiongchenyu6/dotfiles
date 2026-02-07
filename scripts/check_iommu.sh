#!/usr/bin/env bash
set -euo pipefail

# Lists IOMMU groups and their attached devices.

usage() {
    echo "Usage: $0"
    echo "Lists the IOMMU groups and the devices in each group."
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    usage
    exit 0
fi

shopt -s nullglob
for g in $(find /sys/kernel/iommu_groups/* -maxdepth 0 -type d | sort -V);
    echo "IOMMU Group ${g##*/}":
    for d in $g/devices/*;
        echo -e "\t$(lspci -nns ${d##*/})"
    done;
done;
