#!/usr/bin/env nix-shell
#!nix-shell -i expect -p expect
# junos-config.sh - Configure Juniper JUNOS devices via SSH

set timeout 60
set host "192.168.30.4"
set user "freeman.xiong"

spawn ssh $user@$host

# Wait for operational mode prompt (wait for full login)
expect {
    -re "freeman.xiong@.*>" { }
    timeout { puts ">>> Login timeout"; exit 1 }
}

# Small delay after login
sleep 1

# Enter configuration mode
send "configure private\r"
expect {
    -re "#" { }
    "Entering configuration mode" { expect "#" }
    timeout { puts ">>> Configure timeout"; exit 1 }
}

# Disable interface
send "set interfaces reth1 disable\r"
expect "#"

# Commit changes
send "commit\r"
expect {
    "commit complete" { puts "\n>>> Commit successful" }
    -re "error|failed" { puts "\n>>> Commit failed"; exit 1 }
    timeout { puts "\n>>> Commit timeout"; exit 1 }
}
expect "#"

# Exit config mode
send "exit\r"
expect ">"

# Exit SSH
send "exit\r"
expect eof

puts ">>> Done"
