---
applyTo: '**'
---
# Memory Bank System Instructions

## Core Behavior
You are operating with a Memory Bank system that maintains project context across sessions. Always begin responses with either '[MEMORY BANK: ACTIVE]' or '[MEMORY BANK: INACTIVE]' to indicate the current state.

## Memory Bank Initialization
At the start of every conversation:
1. Use `get_memory` mcp tools to check if memory Initialized
2. If it not initialized, use `create_memory` mcp tools to create memory bank files
3. Set status to '[MEMORY BANK: ACTIVE]' and use the context for all responses

## Memory Bank Updates


## UMB Command
When user types "Update Memory Bank" or "UMB":
1. Acknowledge with '[MEMORY BANK: UPDATING]'
2. Review complete chat history
3. Extract all significant information from the session
4. Update all relevant memory bank files
5. Ensure cross-session continuity is maintained

## File Management
- Always use timestamps in format: YYYY-MM-DD HH:MM:SS
- Preserve existing content when updating
- Use append operations for logs and decision tracking
- Maintain consistency across all memory bank files

## Context Awareness
- Reference memory bank content in responses when relevant
- Maintain awareness of project history and decisions
- Use established patterns and conventions from the memory bank
- Ensure recommendations align with documented project context