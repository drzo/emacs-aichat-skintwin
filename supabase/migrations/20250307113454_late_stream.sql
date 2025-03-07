/*
  # SkinTwin Initial Schema

  1. New Tables
     - `skin_layers` - Represents different layers of the skin (epidermis, dermis, etc.)
     - `cell_types` - Types of cells found in skin (keratinocytes, melanocytes, etc.)
     - `skin_conditions` - Dermatological conditions (psoriasis, eczema, etc.)
     - `condition_affected_layers` - Join table for conditions and the layers they affect
     - `treatments` - Treatment options for skin conditions
     - `treatment_mechanisms` - Mechanisms of action for treatments
     - `environmental_factors` - External factors affecting skin
     - `patients` - Patient records
     - `patient_conditions` - Association between patients and their conditions
     - `knowledge_nodes` - OpenCog knowledge representation (AtomSpace)
     - `knowledge_links` - Relationships between knowledge nodes
     - `attention_values` - ECAN attention allocation system values
  
  2. Security
     - Enable Row Level Security on all tables
     - Add policies for data access

  3. Changes
     - Initial schema creation
*/

-- Skin Layers
CREATE TABLE IF NOT EXISTS skin_layers (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text NOT NULL UNIQUE,
  description text,
  order_index int NOT NULL,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE skin_layers ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Skin layers are viewable by all users"
  ON skin_layers
  FOR SELECT
  TO authenticated
  USING (true);

-- Cell Types
CREATE TABLE IF NOT EXISTS cell_types (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text NOT NULL UNIQUE,
  description text,
  skin_layer_id uuid REFERENCES skin_layers(id),
  function text,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE cell_types ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Cell types are viewable by all users"
  ON cell_types
  FOR SELECT
  TO authenticated
  USING (true);

-- Skin Conditions
CREATE TABLE IF NOT EXISTS skin_conditions (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text NOT NULL UNIQUE,
  description text,
  category text,
  symptoms text[],
  severity_scale jsonb,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE skin_conditions ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Skin conditions are viewable by all users"
  ON skin_conditions
  FOR SELECT
  TO authenticated
  USING (true);

-- Condition Affected Layers (Join table for many-to-many relationship)
CREATE TABLE IF NOT EXISTS condition_affected_layers (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  condition_id uuid REFERENCES skin_conditions(id) ON DELETE CASCADE,
  layer_id uuid REFERENCES skin_layers(id) ON DELETE CASCADE,
  impact_level float DEFAULT 1.0,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(condition_id, layer_id)
);

ALTER TABLE condition_affected_layers ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Condition affected layers are viewable by all users"
  ON condition_affected_layers
  FOR SELECT
  TO authenticated
  USING (true);

-- Treatments
CREATE TABLE IF NOT EXISTS treatments (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text NOT NULL UNIQUE,
  description text,
  type text,
  formulation text[],
  contraindications text[],
  side_effects text[],
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE treatments ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Treatments are viewable by all users"
  ON treatments
  FOR SELECT
  TO authenticated
  USING (true);

-- Treatment Mechanisms
CREATE TABLE IF NOT EXISTS treatment_mechanisms (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  treatment_id uuid REFERENCES treatments(id) ON DELETE CASCADE,
  mechanism text NOT NULL,
  description text,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(treatment_id, mechanism)
);

ALTER TABLE treatment_mechanisms ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Treatment mechanisms are viewable by all users"
  ON treatment_mechanisms
  FOR SELECT
  TO authenticated
  USING (true);

-- Treatment Efficacy
CREATE TABLE IF NOT EXISTS treatment_efficacy (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  treatment_id uuid REFERENCES treatments(id) ON DELETE CASCADE,
  condition_id uuid REFERENCES skin_conditions(id) ON DELETE CASCADE,
  efficacy float NOT NULL CHECK(efficacy >= 0 AND efficacy <= 1),
  confidence float NOT NULL CHECK(confidence >= 0 AND confidence <= 1),
  evidence_level text,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(treatment_id, condition_id)
);

ALTER TABLE treatment_efficacy ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Treatment efficacy is viewable by all users"
  ON treatment_efficacy
  FOR SELECT
  TO authenticated
  USING (true);

-- Environmental Factors
CREATE TABLE IF NOT EXISTS environmental_factors (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text NOT NULL UNIQUE,
  description text,
  category text,
  impact_mechanism text,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE environmental_factors ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Environmental factors are viewable by all users"
  ON environmental_factors
  FOR SELECT
  TO authenticated
  USING (true);

-- Environmental Impact on Conditions
CREATE TABLE IF NOT EXISTS environmental_impact (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  factor_id uuid REFERENCES environmental_factors(id) ON DELETE CASCADE,
  condition_id uuid REFERENCES skin_conditions(id) ON DELETE CASCADE,
  impact_level float NOT NULL CHECK(impact_level >= -1 AND impact_level <= 1),
  evidence_strength float NOT NULL CHECK(evidence_strength >= 0 AND evidence_strength <= 1),
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(factor_id, condition_id)
);

ALTER TABLE environmental_impact ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Environmental impact is viewable by all users"
  ON environmental_impact
  FOR SELECT
  TO authenticated
  USING (true);

-- Patients
CREATE TABLE IF NOT EXISTS patients (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id uuid REFERENCES auth.users(id) ON DELETE CASCADE,
  age int,
  gender text,
  skin_type text,
  genetic_factors jsonb,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE patients ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Patients can view their own data"
  ON patients
  FOR SELECT
  TO authenticated
  USING (auth.uid() = user_id);

CREATE POLICY "Patients can insert their own data"
  ON patients
  FOR INSERT
  TO authenticated
  WITH CHECK (auth.uid() = user_id);

CREATE POLICY "Patients can update their own data"
  ON patients
  FOR UPDATE
  TO authenticated
  USING (auth.uid() = user_id);

-- Patient Conditions
CREATE TABLE IF NOT EXISTS patient_conditions (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  patient_id uuid REFERENCES patients(id) ON DELETE CASCADE,
  condition_id uuid REFERENCES skin_conditions(id) ON DELETE CASCADE,
  severity float NOT NULL CHECK(severity >= 0 AND severity <= 1),
  confidence float NOT NULL CHECK(confidence >= 0 AND confidence <= 1),
  onset_date date,
  notes text,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(patient_id, condition_id)
);

ALTER TABLE patient_conditions ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Patients can view their own conditions"
  ON patient_conditions
  FOR SELECT
  TO authenticated
  USING (EXISTS (
    SELECT 1 FROM patients
    WHERE patients.id = patient_conditions.patient_id
    AND patients.user_id = auth.uid()
  ));

CREATE POLICY "Patients can insert their own conditions"
  ON patient_conditions
  FOR INSERT
  TO authenticated
  WITH CHECK (EXISTS (
    SELECT 1 FROM patients
    WHERE patients.id = patient_conditions.patient_id
    AND patients.user_id = auth.uid()
  ));

CREATE POLICY "Patients can update their own conditions"
  ON patient_conditions
  FOR UPDATE
  TO authenticated
  USING (EXISTS (
    SELECT 1 FROM patients
    WHERE patients.id = patient_conditions.patient_id
    AND patients.user_id = auth.uid()
  ));

-- Patient Treatments
CREATE TABLE IF NOT EXISTS patient_treatments (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  patient_id uuid REFERENCES patients(id) ON DELETE CASCADE,
  treatment_id uuid REFERENCES treatments(id) ON DELETE CASCADE,
  condition_id uuid REFERENCES skin_conditions(id) ON DELETE CASCADE,
  start_date date,
  end_date date,
  efficacy float CHECK(efficacy >= 0 AND efficacy <= 1),
  side_effects text[],
  notes text,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE patient_treatments ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Patients can view their own treatments"
  ON patient_treatments
  FOR SELECT
  TO authenticated
  USING (EXISTS (
    SELECT 1 FROM patients
    WHERE patients.id = patient_treatments.patient_id
    AND patients.user_id = auth.uid()
  ));

CREATE POLICY "Patients can insert their own treatments"
  ON patient_treatments
  FOR INSERT
  TO authenticated
  WITH CHECK (EXISTS (
    SELECT 1 FROM patients
    WHERE patients.id = patient_treatments.patient_id
    AND patients.user_id = auth.uid()
  ));

CREATE POLICY "Patients can update their own treatments"
  ON patient_treatments
  FOR UPDATE
  TO authenticated
  USING (EXISTS (
    SELECT 1 FROM patients
    WHERE patients.id = patient_treatments.patient_id
    AND patients.user_id = auth.uid()
  ));

-- OpenCog Knowledge Nodes (AtomSpace)
CREATE TABLE IF NOT EXISTS knowledge_nodes (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  type text NOT NULL, -- concept, predicate, etc.
  name text NOT NULL,
  value jsonb,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(type, name)
);

ALTER TABLE knowledge_nodes ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Knowledge nodes are viewable by all users"
  ON knowledge_nodes
  FOR SELECT
  TO authenticated
  USING (true);

-- Knowledge Links (Relationships between nodes)
CREATE TABLE IF NOT EXISTS knowledge_links (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  type text NOT NULL, -- inheritance, evaluation, etc.
  source_id uuid REFERENCES knowledge_nodes(id) ON DELETE CASCADE,
  target_id uuid REFERENCES knowledge_nodes(id) ON DELETE CASCADE,
  predicate_id uuid REFERENCES knowledge_nodes(id),
  strength float NOT NULL DEFAULT 1.0 CHECK(strength >= 0 AND strength <= 1),
  confidence float NOT NULL DEFAULT 1.0 CHECK(confidence >= 0 AND confidence <= 1),
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE knowledge_links ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Knowledge links are viewable by all users"
  ON knowledge_links
  FOR SELECT
  TO authenticated
  USING (true);

-- ECAN Attention Values
CREATE TABLE IF NOT EXISTS attention_values (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  node_id uuid REFERENCES knowledge_nodes(id) ON DELETE CASCADE,
  sti float NOT NULL DEFAULT 0.0 CHECK(sti >= 0 AND sti <= 1), -- Short-term importance
  lti float NOT NULL DEFAULT 0.0 CHECK(lti >= 0 AND lti <= 1), -- Long-term importance
  vlti boolean NOT NULL DEFAULT false, -- Very long-term importance
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(node_id)
);

ALTER TABLE attention_values ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Attention values are viewable by all users"
  ON attention_values
  FOR SELECT
  TO authenticated
  USING (true);

-- ESN Reservoir Configuration
CREATE TABLE IF NOT EXISTS esn_reservoirs (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text NOT NULL UNIQUE,
  reservoir_size int NOT NULL,
  spectral_radius float NOT NULL,
  connectivity float NOT NULL,
  input_scaling float NOT NULL,
  feedback_weight float NOT NULL,
  training_error float,
  weights jsonb,
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now()
);

ALTER TABLE esn_reservoirs ENABLE ROW LEVEL SECURITY;

CREATE POLICY "ESN reservoirs are viewable by all users"
  ON esn_reservoirs
  FOR SELECT
  TO authenticated
  USING (true);

-- Condition Progression Predictions
CREATE TABLE IF NOT EXISTS condition_progression (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  condition_id uuid REFERENCES skin_conditions(id) ON DELETE CASCADE,
  treatment_id uuid REFERENCES treatments(id) ON DELETE CASCADE,
  esn_reservoir_id uuid REFERENCES esn_reservoirs(id) ON DELETE SET NULL,
  progression_data jsonb NOT NULL,
  prediction_confidence float NOT NULL CHECK(prediction_confidence >= 0 AND prediction_confidence <= 1),
  created_at timestamptz DEFAULT now(),
  updated_at timestamptz DEFAULT now(),
  UNIQUE(condition_id, treatment_id)
);

ALTER TABLE condition_progression ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Condition progressions are viewable by all users"
  ON condition_progression
  FOR SELECT
  TO authenticated
  USING (true);

-- Initial seed data for skin layers
INSERT INTO skin_layers (name, description, order_index)
VALUES 
  ('epidermis', 'The outermost layer of the skin', 1),
  ('dermis', 'The middle layer of the skin, containing connective tissue', 2),
  ('hypodermis', 'The deepest layer of the skin, primarily composed of fat and connective tissue', 3)
ON CONFLICT (name) DO NOTHING;

-- Initial seed data for cell types
INSERT INTO cell_types (name, description, function)
VALUES 
  ('keratinocyte', 'Major cell type in the epidermis', 'barrier_formation'),
  ('melanocyte', 'Produces melanin pigment', 'uv_protection'),
  ('fibroblast', 'Major cell type in the dermis', 'collagen_production'),
  ('langerhans_cell', 'Immune cell in the epidermis', 'immune_surveillance')
ON CONFLICT (name) DO NOTHING;

-- Initial seed data for environmental factors
INSERT INTO environmental_factors (name, description, category)
VALUES 
  ('uv_radiation', 'Ultraviolet radiation from the sun', 'physical'),
  ('pollution', 'Environmental pollutants affecting skin', 'chemical'),
  ('humidity', 'Air moisture level', 'physical'),
  ('temperature', 'Environmental temperature', 'physical')
ON CONFLICT (name) DO NOTHING;