---
applyTo: '**'
---
# Memory Bank System Instructions

## Core Behavior
You are operating with a Memory Bank system that maintains project context across sessions. Always begin responses with either '[MEMORY BANK: ACTIVE]' or '[MEMORY BANK: INACTIVE]' to indicate the current state.

## Memory Bank Initialization
At the start of every conversation:
1. Check if the `memory-bank/` directory exists in the repository root
2. If it exists, read ALL memory bank files in this order:
   - `memory-bank/productContext.md`
   - `memory-bank/activeContext.md`
   - `memory-bank/systemPatterns.md`
   - `memory-bank/decisionLog.md`
   - `memory-bank/progress.md`
3. Set status to '[MEMORY BANK: ACTIVE]' and use the context for all responses
4. If memory-bank/ doesn't exist, set status to '[MEMORY BANK: INACTIVE]' and offer to create it

## Memory Bank Updates
Update memory bank files when significant changes occur:

### decisionLog.md
- **When**: Architectural decisions, technology choices, new components
- **Format**: `[2025-05-24 07:34:38] - [Decision Summary]`
- **Action**: Append new entries with timestamps

### productContext.md
- **When**: Project goals, features, or overall architecture changes
- **Format**: `[2025-05-24 07:34:38] - [Change Summary]`
- **Action**: Append updates or modify existing content

### systemPatterns.md
- **When**: New architectural patterns introduced or modified
- **Format**: `[2025-05-24 07:34:38] - [Pattern Description]`
- **Action**: Append new patterns with timestamps

### activeContext.md
- **When**: Current work focus changes or significant progress made
- **Format**: `[2025-05-24 07:34:38] - [Context Update]`
- **Action**: Update relevant sections (Current Focus, Recent Changes, Open Questions)

### progress.md
- **When**: Tasks begin, complete, or change status
- **Format**: `[2025-05-24 07:34:38] - [Progress Update]`
- **Action**: Append new entries, never overwrite existing ones

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