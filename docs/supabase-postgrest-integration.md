# Supabase Auth + PostgREST Integration Guide

This guide explains how to use the integrated Supabase Auth (GoTrue) and PostgREST setup on oracle-arm-001 for frontend development.

## Overview

The server provides:
- **PostgREST API**: `https://api.corp.autolife.ai` - Auto-generated REST API from PostgreSQL schema
- **Supabase Auth**: `https://auth.corp.autolife.ai` - User authentication and authorization

Both services share the same JWT secret and database, enabling seamless authentication flow.

## Architecture

```
Frontend App ──→ Supabase Auth ──→ JWT Token ──→ PostgREST API ──→ PostgreSQL
                 (auth.corp.autolife.ai)        (api.corp.autolife.ai)
```

## Authentication Flow

1. User signs up/signs in via Supabase Auth
2. Supabase Auth returns a JWT token
3. Frontend includes JWT token in API requests to PostgREST
4. PostgREST validates JWT and serves data based on user permissions

## Frontend Setup

### 1. Install Supabase Client

```bash
npm install @supabase/supabase-js
```

### 2. Initialize Supabase Client

```javascript
import { createClient } from '@supabase/supabase-js'

const supabaseUrl = 'https://auth.corp.autolife.ai'
const supabaseAnonKey = 'your-anon-key-here' // Get this from your admin
const supabase = createClient(supabaseUrl, supabaseAnonKey)
```

### 3. Authentication Functions

```javascript
// Sign Up
async function signUp(email, password) {
  const { data, error } = await supabase.auth.signUp({
    email: email,
    password: password
  })
  return { data, error }
}

// Sign In
async function signIn(email, password) {
  const { data, error } = await supabase.auth.signInWithPassword({
    email: email,
    password: password
  })
  return { data, error }
}

// Sign Out
async function signOut() {
  const { error } = await supabase.auth.signOut()
  return { error }
}

// Get Current User
function getCurrentUser() {
  return supabase.auth.getUser()
}

// Listen to Auth Changes
supabase.auth.onAuthStateChange((event, session) => {
  if (event === 'SIGNED_IN') {
    console.log('User signed in:', session.user)
  }
  if (event === 'SIGNED_OUT') {
    console.log('User signed out')
  }
})
```

### 4. Making Authenticated API Calls

```javascript
// Get the current session token
async function getApiToken() {
  const { data: { session } } = await supabase.auth.getSession()
  return session?.access_token
}

// Make authenticated request to PostgREST
async function fetchUserData() {
  const token = await getApiToken()
  
  const response = await fetch('https://api.corp.autolife.ai/your_table', {
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json'
    }
  })
  
  return response.json()
}

// POST data with authentication
async function createRecord(data) {
  const token = await getApiToken()
  
  const response = await fetch('https://api.corp.autolife.ai/your_table', {
    method: 'POST',
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json',
      'Prefer': 'return=minimal'
    },
    body: JSON.stringify(data)
  })
  
  return response.json()
}
```

### 5. React Hook Example

```javascript
import { useState, useEffect } from 'react'
import { supabase } from './supabaseClient'

export function useAuth() {
  const [user, setUser] = useState(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    // Get initial session
    supabase.auth.getSession().then(({ data: { session } }) => {
      setUser(session?.user ?? null)
      setLoading(false)
    })

    // Listen for auth changes
    const { data: { subscription } } = supabase.auth.onAuthStateChange(
      (event, session) => {
        setUser(session?.user ?? null)
        setLoading(false)
      }
    )

    return () => subscription.unsubscribe()
  }, [])

  return {
    user,
    loading,
    signIn: (email, password) => supabase.auth.signInWithPassword({ email, password }),
    signUp: (email, password) => supabase.auth.signUp({ email, password }),
    signOut: () => supabase.auth.signOut()
  }
}
```

## PostgREST API Usage

### Basic CRUD Operations

```javascript
const API_BASE = 'https://api.corp.autolife.ai'

// GET all records
async function getRecords(table) {
  const token = await getApiToken()
  const response = await fetch(`${API_BASE}/${table}`, {
    headers: { 'Authorization': `Bearer ${token}` }
  })
  return response.json()
}

// GET with filters
async function getFilteredRecords(table, filters) {
  const token = await getApiToken()
  const queryString = new URLSearchParams(filters).toString()
  const response = await fetch(`${API_BASE}/${table}?${queryString}`, {
    headers: { 'Authorization': `Bearer ${token}` }
  })
  return response.json()
}

// POST new record
async function createRecord(table, data) {
  const token = await getApiToken()
  const response = await fetch(`${API_BASE}/${table}`, {
    method: 'POST',
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json',
      'Prefer': 'return=representation'
    },
    body: JSON.stringify(data)
  })
  return response.json()
}

// PATCH existing record
async function updateRecord(table, id, data) {
  const token = await getApiToken()
  const response = await fetch(`${API_BASE}/${table}?id=eq.${id}`, {
    method: 'PATCH',
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json',
      'Prefer': 'return=representation'
    },
    body: JSON.stringify(data)
  })
  return response.json()
}

// DELETE record
async function deleteRecord(table, id) {
  const token = await getApiToken()
  const response = await fetch(`${API_BASE}/${table}?id=eq.${id}`, {
    method: 'DELETE',
    headers: {
      'Authorization': `Bearer ${token}`,
      'Prefer': 'return=minimal'
    }
  })
  return response.ok
}
```

### Advanced Queries

```javascript
// Complex filters
const users = await getFilteredRecords('users', {
  'age': 'gte.18',           // age >= 18
  'status': 'eq.active',     // status = 'active'
  'name': 'ilike.*john*',    // name contains 'john' (case-insensitive)
  'select': 'id,name,email', // only return specific columns
  'order': 'created_at.desc', // sort by created_at descending
  'limit': '10'              // limit to 10 results
})

// Joins (if you have foreign key relationships)
const ordersWithCustomers = await getFilteredRecords('orders', {
  'select': 'id,amount,customers(name,email)',
  'customers.status': 'eq.active'
})
```

## Database Setup for Row Level Security (RLS)

To properly secure your data, you should set up Row Level Security in PostgreSQL:

```sql
-- Enable RLS on your tables
ALTER TABLE your_table ENABLE ROW LEVEL SECURITY;

-- Create policies for authenticated users
CREATE POLICY "Users can view own data" ON your_table
  FOR SELECT USING (auth.uid() = user_id);

CREATE POLICY "Users can insert own data" ON your_table
  FOR INSERT WITH CHECK (auth.uid() = user_id);

CREATE POLICY "Users can update own data" ON your_table
  FOR UPDATE USING (auth.uid() = user_id);

-- Create a function to get user ID from JWT
CREATE OR REPLACE FUNCTION auth.uid() RETURNS UUID AS $$
  SELECT NULLIF(current_setting('request.jwt.claims', true)::json->>'sub', '')::UUID;
$$ LANGUAGE SQL STABLE;
```

## Error Handling

```javascript
async function handleAuthErrors() {
  try {
    const { data, error } = await supabase.auth.signIn({
      email: 'user@example.com',
      password: 'password'
    })
    
    if (error) {
      switch (error.message) {
        case 'Invalid login credentials':
          // Handle invalid credentials
          break
        case 'Email not confirmed':
          // Handle unconfirmed email
          break
        default:
          // Handle other errors
          console.error('Auth error:', error.message)
      }
    }
  } catch (err) {
    console.error('Network error:', err)
  }
}
```

## Environment Variables

Create a `.env` file in your frontend project:

```env
VITE_SUPABASE_URL=https://auth.corp.autolife.ai
VITE_SUPABASE_ANON_KEY=your-anon-key-here
VITE_API_URL=https://api.corp.autolife.ai
```

## Testing the Setup

### 1. Test Authentication

```bash
# Sign up a new user
curl -X POST https://auth.corp.autolife.ai/signup \
  -H "Content-Type: application/json" \
  -d '{
    "email": "test@example.com",
    "password": "password123"
  }'

# Sign in
curl -X POST https://auth.corp.autolife.ai/token?grant_type=password \
  -H "Content-Type: application/json" \
  -d '{
    "email": "test@example.com",
    "password": "password123"
  }'
```

### 2. Test PostgREST with JWT

```bash
# Get JWT token from auth response, then:
curl -X GET https://api.corp.autolife.ai/your_table \
  -H "Authorization: Bearer YOUR_JWT_TOKEN"
```

## Troubleshooting

### Common Issues

1. **CORS Errors**: The nginx configuration includes CORS headers, but if you're still having issues, check your request headers.

2. **JWT Token Expired**: Tokens expire after 1 hour. Implement token refresh logic:

```javascript
async function refreshToken() {
  const { data, error } = await supabase.auth.refreshSession()
  if (error) {
    // Redirect to login
    console.error('Token refresh failed:', error)
  }
  return data.session
}
```

3. **Database Connection Issues**: Check that your database URL in the secrets is correct and that the rustwebserver user has proper permissions.

4. **Row Level Security**: If you can't access data, ensure RLS policies are correctly set up for your user role.

## Next Steps

1. Set up proper database schemas and RLS policies
2. Configure email templates for user confirmation
3. Add social login providers if needed
4. Set up proper error monitoring and logging
5. Implement proper user roles and permissions

## Support

For issues with the server setup, check the systemd logs:

```bash
# Check GoTrue service
sudo journalctl -u gotrue-supabase.service -f

# Check PostgREST service  
sudo journalctl -u postgrest.service -f

# Check PostgreSQL
sudo journalctl -u postgresql.service -f
```