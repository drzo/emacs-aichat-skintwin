import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App'
import './index.css'
import { supabase, testConnection } from '../supabase/client'

// Test database connection on startup
testConnection().then(connected => {
  if (connected) {
    console.log('Successfully connected to Supabase')
  } else {
    console.error('Failed to connect to Supabase')
  }
})

ReactDOM.createRoot(document.getElementById('app')).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
)