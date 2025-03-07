import { supabase, testConnection } from '../supabase/client.js'

// Test database connection on startup
testConnection().then(connected => {
  if (connected) {
    console.log('Successfully connected to Supabase')
  } else {
    console.error('Failed to connect to Supabase')
  }
})