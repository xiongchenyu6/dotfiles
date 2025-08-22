#!/usr/bin/env bash
set -euo pipefail

# This script parses the cloudflared tunnel command to extract the tunnel credentials.

usage() {
    echo "Usage: $0 <tunnel-name> <cloudflared-command>"
    echo "Example: $0 amd-002 \"cloudflared tunnel run --token eyJhI...\""
    echo "Parses the cloudflared tunnel command to extract the tunnel credentials."
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    usage
    exit 0
fi

# Check if we have the required arguments
if [ $# -lt 2 ]; then
    echo "Usage: $0 <tunnel-name> <cloudflared-command>"
    echo "Example: $0 amd-002 \"cloudflared tunnel run --token eyJhI...\""
    exit 1
fi

TUNNEL_NAME="$1"
shift
COMMAND="$*"

# Extract the token from the command
TOKEN=$(echo "$COMMAND" | grep -oP '(?<=--token )[^ ]+')

if [ -z "$TOKEN" ]; then
    echo "Error: Could not extract token from command"
    exit 1
fi

# Decode the base64 token to get the JSON data
DECODED=$(echo "$TOKEN" | base64 -d 2>/dev/null)

if [ $? -ne 0 ]; then
    echo "Error: Failed to decode base64 token"
    exit 1
fi

# Parse the JSON fields
ACCOUNT_TAG=$(echo "$DECODED" | jq -r '.a // empty')
TUNNEL_ID=$(echo "$DECODED" | jq -r '.t // empty')
TUNNEL_SECRET=$(echo "$DECODED" | jq -r '.s // empty')

if [ -z "$ACCOUNT_TAG" ] || [ -z "$TUNNEL_ID" ] || [ -z "$TUNNEL_SECRET" ]; then
    echo "Error: Could not parse all required fields from token"
    exit 1
fi

# Generate the output JSON
cat <<EOF
{
    "AccountTag": "$ACCOUNT_TAG",
    "TunnelID": "$TUNNEL_ID",
    "TunnelName": "$TUNNEL_NAME",
    "TunnelSecret": "$TUNNEL_SECRET"
}
EOF
