import { createClient } from '@supabase/supabase-js'

const supabaseUrl = import.meta.env.VITE_SUPABASE_URL
const supabaseAnonKey = import.meta.env.VITE_SUPABASE_ANON_KEY

if (!supabaseUrl || !supabaseAnonKey) {
  console.error('Missing Supabase URL or anon key. Check your .env file.')
}

// Create a single supabase client for interacting with your database
export const supabase = createClient(supabaseUrl, supabaseAnonKey)

// Helper function to check if database is accessible
export async function testConnection() {
  try {
    const { data, error } = await supabase
      .from('skin_layers')
      .select('name')
      .limit(1)
    
    if (error) {
      console.error('Database connection failed:', error.message)
      return false
    }
    
    console.log('Database connection successful')
    return true
  } catch (err) {
    console.error('Exception in database connection test:', err.message)
    return false
  }
}

// API for working with skin layers
export const skinLayersAPI = {
  async getAll() {
    const { data, error } = await supabase
      .from('skin_layers')
      .select('*')
      .order('order_index')
    
    if (error) {
      console.error('Error fetching skin layers:', error.message)
      return []
    }
    
    return data
  },
  
  async getById(id) {
    const { data, error } = await supabase
      .from('skin_layers')
      .select('*')
      .eq('id', id)
      .single()
    
    if (error) {
      console.error(`Error fetching skin layer ${id}:`, error.message)
      return null
    }
    
    return data
  }
}

// API for working with skin conditions
export const skinConditionsAPI = {
  async getAll() {
    const { data, error } = await supabase
      .from('skin_conditions')
      .select('*')
    
    if (error) {
      console.error('Error fetching skin conditions:', error.message)
      return []
    }
    
    return data
  },
  
  async getById(id) {
    const { data, error } = await supabase
      .from('skin_conditions')
      .select(`
        *,
        condition_affected_layers (
          layer_id,
          impact_level
        )
      `)
      .eq('id', id)
      .single()
    
    if (error) {
      console.error(`Error fetching skin condition ${id}:`, error.message)
      return null
    }
    
    return data
  },
  
  async getTreatments(conditionId) {
    const { data, error } = await supabase
      .from('treatment_efficacy')
      .select(`
        efficacy,
        confidence,
        evidence_level,
        treatments (
          id,
          name,
          description,
          type,
          formulation,
          side_effects
        )
      `)
      .eq('condition_id', conditionId)
    
    if (error) {
      console.error(`Error fetching treatments for condition ${conditionId}:`, error.message)
      return []
    }
    
    return data.map(item => ({
      ...item.treatments,
      efficacy: item.efficacy,
      confidence: item.confidence,
      evidence_level: item.evidence_level
    }))
  }
}

// API for working with treatments
export const treatmentsAPI = {
  async getAll() {
    const { data, error } = await supabase
      .from('treatments')
      .select('*')
    
    if (error) {
      console.error('Error fetching treatments:', error.message)
      return []
    }
    
    return data
  },
  
  async getById(id) {
    const { data, error } = await supabase
      .from('treatments')
      .select(`
        *,
        treatment_mechanisms (
          mechanism,
          description
        )
      `)
      .eq('id', id)
      .single()
    
    if (error) {
      console.error(`Error fetching treatment ${id}:`, error.message)
      return null
    }
    
    return data
  },
  
  async getConditions(treatmentId) {
    const { data, error } = await supabase
      .from('treatment_efficacy')
      .select(`
        efficacy,
        confidence,
        evidence_level,
        skin_conditions (
          id,
          name,
          description,
          category,
          symptoms
        )
      `)
      .eq('treatment_id', treatmentId)
    
    if (error) {
      console.error(`Error fetching conditions for treatment ${treatmentId}:`, error.message)
      return []
    }
    
    return data.map(item => ({
      ...item.skin_conditions,
      efficacy: item.efficacy,
      confidence: item.confidence,
      evidence_level: item.evidence_level
    }))
  }
}

// API for working with OpenCog knowledge nodes
export const knowledgeAPI = {
  async getNodes() {
    const { data, error } = await supabase
      .from('knowledge_nodes')
      .select('*')
    
    if (error) {
      console.error('Error fetching knowledge nodes:', error.message)
      return []
    }
    
    return data
  },
  
  async getLinks() {
    const { data, error } = await supabase
      .from('knowledge_links')
      .select(`
        *,
        source:source_id (name, type),
        target:target_id (name, type),
        predicate:predicate_id (name, type)
      `)
    
    if (error) {
      console.error('Error fetching knowledge links:', error.message)
      return []
    }
    
    return data
  },
  
  async getAttentionValues() {
    const { data, error } = await supabase
      .from('attention_values')
      .select(`
        *,
        node:node_id (name, type)
      `)
    
    if (error) {
      console.error('Error fetching attention values:', error.message)
      return []
    }
    
    return data
  }
}

// API for working with patients
export const patientsAPI = {
  async getCurrentPatient() {
    const { data: { user } } = await supabase.auth.getUser()
    
    if (!user) {
      console.error('No authenticated user found')
      return null
    }
    
    const { data, error } = await supabase
      .from('patients')
      .select('*')
      .eq('user_id', user.id)
      .single()
    
    if (error && error.code !== 'PGRST116') { // PGRST116 is "no rows returned"
      console.error('Error fetching patient:', error.message)
      return null
    }
    
    return data
  },
  
  async createPatient(patientData) {
    const { data: { user } } = await supabase.auth.getUser()
    
    if (!user) {
      console.error('No authenticated user found')
      return null
    }
    
    const { data, error } = await supabase
      .from('patients')
      .insert({
        ...patientData,
        user_id: user.id
      })
      .select()
      .single()
    
    if (error) {
      console.error('Error creating patient:', error.message)
      return null
    }
    
    return data
  },
  
  async updatePatient(patientId, patientData) {
    const { data, error } = await supabase
      .from('patients')
      .update(patientData)
      .eq('id', patientId)
      .select()
      .single()
    
    if (error) {
      console.error('Error updating patient:', error.message)
      return null
    }
    
    return data
  },
  
  async addCondition(patientId, conditionData) {
    const { data, error } = await supabase
      .from('patient_conditions')
      .insert({
        ...conditionData,
        patient_id: patientId
      })
      .select()
      .single()
    
    if (error) {
      console.error('Error adding condition to patient:', error.message)
      return null
    }
    
    return data
  },
  
  async getConditions(patientId) {
    const { data, error } = await supabase
      .from('patient_conditions')
      .select(`
        *,
        skin_conditions (
          id,
          name,
          description,
          category,
          symptoms
        )
      `)
      .eq('patient_id', patientId)
    
    if (error) {
      console.error(`Error fetching conditions for patient ${patientId}:`, error.message)
      return []
    }
    
    return data
  }
}

// API for working with condition progression predictions
export const progressionAPI = {
  async getPrediction(conditionId, treatmentId) {
    const { data, error } = await supabase
      .from('condition_progression')
      .select(`
        *,
        condition:condition_id (name),
        treatment:treatment_id (name)
      `)
      .eq('condition_id', conditionId)
      .eq('treatment_id', treatmentId)
      .single()
    
    if (error) {
      console.error(`Error fetching progression prediction:`, error.message)
      return null
    }
    
    return data
  },
  
  async getAllForCondition(conditionId) {
    const { data, error } = await supabase
      .from('condition_progression')
      .select(`
        *,
        treatment:treatment_id (name)
      `)
      .eq('condition_id', conditionId)
    
    if (error) {
      console.error(`Error fetching progression predictions for condition ${conditionId}:`, error.message)
      return []
    }
    
    return data
  }
}