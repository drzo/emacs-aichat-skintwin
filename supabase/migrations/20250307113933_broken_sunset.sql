/*
  # SkinTwin Initial Data Seeding
  
  1. New Data
     - Skin conditions (psoriasis, eczema, acne, basal_cell_carcinoma)
     - Treatments (corticosteroid, retinoid, antibiotic, antihistamine)
     - Treatment mechanisms and efficacy data
     - Knowledge nodes for OpenCog integration
     - ECAN attention values
     - ESN reservoir configurations
     - Condition progression predictions
  
  2. Changes
     - Initial data seeding for all previously created tables
*/

-- First, handle skin conditions
DO $$ 
BEGIN
  -- First, verify that all required tables exist
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'skin_conditions') THEN
    -- Initial seed data for skin conditions
    INSERT INTO skin_conditions (name, description, category, symptoms)
    VALUES 
      (
        'psoriasis', 
        'Chronic inflammatory condition characterized by rapid keratinocyte proliferation', 
        'inflammatory',
        ARRAY['scaly_plaques', 'redness', 'itching', 'skin_thickening']
      ),
      (
        'eczema', 
        'Inflammatory condition with impaired skin barrier function', 
        'inflammatory',
        ARRAY['itching', 'redness', 'dryness', 'rash']
      ),
      (
        'acne', 
        'Inflammatory condition of hair follicles and sebaceous glands', 
        'inflammatory',
        ARRAY['comedones', 'papules', 'pustules', 'nodules']
      ),
      (
        'basal_cell_carcinoma', 
        'Common skin cancer arising from basal cells of the epidermis', 
        'neoplastic',
        ARRAY['pearly_papule', 'ulceration', 'bleeding', 'non_healing_lesion']
      )
    ON CONFLICT (name) DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Connect skin conditions to skin layers through the join table
DO $$
BEGIN
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'condition_affected_layers') 
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'skin_layers')
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'skin_conditions') THEN
    
    -- Add affected layers for psoriasis
    INSERT INTO condition_affected_layers (condition_id, layer_id, impact_level)
    SELECT 
      c.id, 
      l.id,
      0.9 -- High impact
    FROM 
      skin_conditions c,
      skin_layers l
    WHERE 
      c.name = 'psoriasis' AND 
      l.name = 'epidermis'
    ON CONFLICT DO NOTHING;
    
    -- Add affected layers for eczema
    INSERT INTO condition_affected_layers (condition_id, layer_id, impact_level)
    SELECT 
      c.id, 
      l.id,
      0.8 -- High impact
    FROM 
      skin_conditions c,
      skin_layers l
    WHERE 
      c.name = 'eczema' AND 
      l.name = 'epidermis'
    ON CONFLICT DO NOTHING;
    
    INSERT INTO condition_affected_layers (condition_id, layer_id, impact_level)
    SELECT 
      c.id, 
      l.id,
      0.3 -- Low impact
    FROM 
      skin_conditions c,
      skin_layers l
    WHERE 
      c.name = 'eczema' AND 
      l.name = 'dermis'
    ON CONFLICT DO NOTHING;
    
    -- Add affected layers for acne
    INSERT INTO condition_affected_layers (condition_id, layer_id, impact_level)
    SELECT 
      c.id, 
      l.id,
      0.7 -- Medium-high impact
    FROM 
      skin_conditions c,
      skin_layers l
    WHERE 
      c.name = 'acne' AND 
      l.name = 'epidermis'
    ON CONFLICT DO NOTHING;
    
    -- Add affected layers for basal cell carcinoma
    INSERT INTO condition_affected_layers (condition_id, layer_id, impact_level)
    SELECT 
      c.id, 
      l.id,
      0.9 -- High impact
    FROM 
      skin_conditions c,
      skin_layers l
    WHERE 
      c.name = 'basal_cell_carcinoma' AND 
      l.name = 'epidermis'
    ON CONFLICT DO NOTHING;
    
    INSERT INTO condition_affected_layers (condition_id, layer_id, impact_level)
    SELECT 
      c.id, 
      l.id,
      0.6 -- Medium impact
    FROM 
      skin_conditions c,
      skin_layers l
    WHERE 
      c.name = 'basal_cell_carcinoma' AND 
      l.name = 'dermis'
    ON CONFLICT DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Insert treatments
DO $$
BEGIN
  -- Insert treatments if table exists
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'treatments') THEN
    -- Initial seed data for treatments
    INSERT INTO treatments (name, description, type, formulation, side_effects)
    VALUES 
      (
        'corticosteroid', 
        'Anti-inflammatory agent used for various inflammatory skin conditions', 
        'topical_agent',
        ARRAY['cream', 'ointment', 'lotion'],
        ARRAY['skin_thinning', 'telangiectasia', 'hypopigmentation']
      ),
      (
        'retinoid', 
        'Vitamin A derivatives used for acne and photoaging', 
        'topical_agent',
        ARRAY['cream', 'gel'],
        ARRAY['skin_irritation', 'dryness', 'photosensitivity']
      ),
      (
        'antibiotic', 
        'Agents that kill or inhibit microorganisms', 
        'systemic_agent',
        ARRAY['tablet', 'capsule', 'cream'],
        ARRAY['nausea', 'diarrhea', 'photosensitivity']
      ),
      (
        'antihistamine', 
        'Blocks histamine receptors to reduce allergic symptoms', 
        'systemic_agent',
        ARRAY['tablet', 'capsule', 'syrup'],
        ARRAY['drowsiness', 'dry_mouth', 'blurred_vision']
      )
    ON CONFLICT (name) DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Insert treatment mechanisms
DO $$
BEGIN
  -- Insert treatment mechanisms if tables exist
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'treatment_mechanisms') 
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'treatments') THEN
    
    -- Add treatment mechanisms
    INSERT INTO treatment_mechanisms (treatment_id, mechanism, description)
    SELECT 
      t.id, 
      'anti_inflammatory', 
      'Reduces inflammation by inhibiting inflammatory mediators'
    FROM treatments t 
    WHERE t.name = 'corticosteroid'
    ON CONFLICT DO NOTHING;

    INSERT INTO treatment_mechanisms (treatment_id, mechanism, description)
    SELECT 
      t.id, 
      'keratinocyte_regulation', 
      'Normalizes keratinization process'
    FROM treatments t 
    WHERE t.name = 'retinoid'
    ON CONFLICT DO NOTHING;

    INSERT INTO treatment_mechanisms (treatment_id, mechanism, description)
    SELECT 
      t.id, 
      'antimicrobial', 
      'Kills or inhibits the growth of microorganisms'
    FROM treatments t 
    WHERE t.name = 'antibiotic'
    ON CONFLICT DO NOTHING;

    INSERT INTO treatment_mechanisms (treatment_id, mechanism, description)
    SELECT 
      t.id, 
      'antihistamine', 
      'Blocks histamine receptors'
    FROM treatments t 
    WHERE t.name = 'antihistamine'
    ON CONFLICT DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Insert treatment efficacy
DO $$
BEGIN
  -- Insert treatment efficacy data if tables exist
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'treatment_efficacy') 
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'treatments')
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'skin_conditions') THEN
    
    -- Add treatment efficacy
    INSERT INTO treatment_efficacy (treatment_id, condition_id, efficacy, confidence, evidence_level)
    SELECT 
      t.id, 
      c.id, 
      0.9, -- 90% effective
      0.95, -- 95% confidence
      'high'
    FROM 
      treatments t,
      skin_conditions c
    WHERE 
      t.name = 'corticosteroid' AND 
      c.name = 'psoriasis'
    ON CONFLICT DO NOTHING;

    INSERT INTO treatment_efficacy (treatment_id, condition_id, efficacy, confidence, evidence_level)
    SELECT 
      t.id, 
      c.id, 
      0.8, -- 80% effective
      0.9, -- 90% confidence
      'high'
    FROM 
      treatments t,
      skin_conditions c
    WHERE 
      t.name = 'corticosteroid' AND 
      c.name = 'eczema'
    ON CONFLICT DO NOTHING;

    INSERT INTO treatment_efficacy (treatment_id, condition_id, efficacy, confidence, evidence_level)
    SELECT 
      t.id, 
      c.id, 
      0.85, -- 85% effective
      0.9, -- 90% confidence
      'high'
    FROM 
      treatments t,
      skin_conditions c
    WHERE 
      t.name = 'retinoid' AND 
      c.name = 'acne'
    ON CONFLICT DO NOTHING;

    INSERT INTO treatment_efficacy (treatment_id, condition_id, efficacy, confidence, evidence_level)
    SELECT 
      t.id, 
      c.id, 
      0.75, -- 75% effective
      0.85, -- 85% confidence
      'medium'
    FROM 
      treatments t,
      skin_conditions c
    WHERE 
      t.name = 'antibiotic' AND 
      c.name = 'acne'
    ON CONFLICT DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Insert knowledge nodes
DO $$
BEGIN
  -- Insert knowledge nodes for OpenCog if table exists
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'knowledge_nodes') THEN
    -- Create initial knowledge nodes (concepts)
    INSERT INTO knowledge_nodes (type, name)
    VALUES
      ('concept', 'skin_layer'),
      ('concept', 'epidermis'),
      ('concept', 'dermis'),
      ('concept', 'hypodermis'),
      ('concept', 'cell_type'),
      ('concept', 'keratinocyte'),
      ('concept', 'melanocyte'),
      ('concept', 'fibroblast'),
      ('concept', 'skin_condition'),
      ('concept', 'psoriasis'),
      ('concept', 'eczema'),
      ('concept', 'acne'),
      ('concept', 'treatment'),
      ('concept', 'corticosteroid'),
      ('concept', 'retinoid'),
      ('concept', 'antibiotic'),
      ('predicate', 'treats'),
      ('predicate', 'has_location'),
      ('predicate', 'has_mechanism'),
      ('predicate', 'affects')
    ON CONFLICT DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Create knowledge links in a separate DO block
DO $$
DECLARE
  skin_layer_id uuid;
  epidermis_id uuid;
  dermis_id uuid;
  hypodermis_id uuid;
  cell_type_id uuid;
  keratinocyte_id uuid;
  melanocyte_id uuid;
  fibroblast_id uuid;
  skin_condition_id uuid;
  psoriasis_id uuid;
  eczema_id uuid;
  acne_id uuid;
  treatment_id uuid;
  corticosteroid_id uuid;
  retinoid_id uuid;
  antibiotic_id uuid;
BEGIN
  -- Only proceed if both required tables exist
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'knowledge_links') 
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'knowledge_nodes') THEN
    
    -- Get node IDs
    SELECT id INTO skin_layer_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'skin_layer';
    SELECT id INTO epidermis_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'epidermis';
    SELECT id INTO dermis_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'dermis';
    SELECT id INTO hypodermis_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'hypodermis';
    SELECT id INTO cell_type_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'cell_type';
    SELECT id INTO keratinocyte_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'keratinocyte';
    SELECT id INTO melanocyte_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'melanocyte';
    SELECT id INTO fibroblast_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'fibroblast';
    SELECT id INTO skin_condition_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'skin_condition';
    SELECT id INTO psoriasis_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'psoriasis';
    SELECT id INTO eczema_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'eczema';
    SELECT id INTO acne_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'acne';
    SELECT id INTO treatment_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'treatment';
    SELECT id INTO corticosteroid_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'corticosteroid';
    SELECT id INTO retinoid_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'retinoid';
    SELECT id INTO antibiotic_id FROM knowledge_nodes WHERE type = 'concept' AND name = 'antibiotic';

    -- Create inheritance links only if all the required nodes exist
    IF skin_layer_id IS NOT NULL AND epidermis_id IS NOT NULL THEN
      -- Epidermis is a skin layer
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', epidermis_id, skin_layer_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF skin_layer_id IS NOT NULL AND dermis_id IS NOT NULL THEN
      -- Dermis is a skin layer
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', dermis_id, skin_layer_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF skin_layer_id IS NOT NULL AND hypodermis_id IS NOT NULL THEN
      -- Hypodermis is a skin layer
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', hypodermis_id, skin_layer_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF cell_type_id IS NOT NULL AND keratinocyte_id IS NOT NULL THEN
      -- Keratinocyte is a cell type
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', keratinocyte_id, cell_type_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF cell_type_id IS NOT NULL AND melanocyte_id IS NOT NULL THEN
      -- Melanocyte is a cell type
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', melanocyte_id, cell_type_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF cell_type_id IS NOT NULL AND fibroblast_id IS NOT NULL THEN
      -- Fibroblast is a cell type
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', fibroblast_id, cell_type_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF skin_condition_id IS NOT NULL AND psoriasis_id IS NOT NULL THEN
      -- Psoriasis is a skin condition
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', psoriasis_id, skin_condition_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF skin_condition_id IS NOT NULL AND eczema_id IS NOT NULL THEN
      -- Eczema is a skin condition
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', eczema_id, skin_condition_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF skin_condition_id IS NOT NULL AND acne_id IS NOT NULL THEN
      -- Acne is a skin condition
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', acne_id, skin_condition_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF treatment_id IS NOT NULL AND corticosteroid_id IS NOT NULL THEN
      -- Corticosteroid is a treatment
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', corticosteroid_id, treatment_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF treatment_id IS NOT NULL AND retinoid_id IS NOT NULL THEN
      -- Retinoid is a treatment
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', retinoid_id, treatment_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;

    IF treatment_id IS NOT NULL AND antibiotic_id IS NOT NULL THEN
      -- Antibiotic is a treatment
      INSERT INTO knowledge_links (type, source_id, target_id, strength, confidence)
      VALUES ('inheritance', antibiotic_id, treatment_id, 1.0, 1.0)
      ON CONFLICT DO NOTHING;
    END IF;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Add ECAN attention values
DO $$
BEGIN
  -- Add ECAN attention values if tables exist
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'attention_values') 
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'knowledge_nodes') THEN
    
    -- Add initial ECAN attention values
    INSERT INTO attention_values (node_id, sti, lti, vlti)
    SELECT id, 0.8, 0.6, true
    FROM knowledge_nodes 
    WHERE type = 'concept' AND name = 'epidermis'
    ON CONFLICT DO NOTHING;

    INSERT INTO attention_values (node_id, sti, lti, vlti)
    SELECT id, 0.7, 0.6, true
    FROM knowledge_nodes 
    WHERE type = 'concept' AND name = 'dermis'
    ON CONFLICT DO NOTHING;

    INSERT INTO attention_values (node_id, sti, lti, vlti)
    SELECT id, 0.9, 0.7, true
    FROM knowledge_nodes 
    WHERE type = 'concept' AND name = 'skin_condition'
    ON CONFLICT DO NOTHING;

    INSERT INTO attention_values (node_id, sti, lti, vlti)
    SELECT id, 0.85, 0.65, true
    FROM knowledge_nodes 
    WHERE type = 'concept' AND name = 'psoriasis'
    ON CONFLICT DO NOTHING;

    INSERT INTO attention_values (node_id, sti, lti, vlti)
    SELECT id, 0.8, 0.6, true
    FROM knowledge_nodes 
    WHERE type = 'concept' AND name = 'eczema'
    ON CONFLICT DO NOTHING;

    INSERT INTO attention_values (node_id, sti, lti, vlti)
    SELECT id, 0.75, 0.55, true
    FROM knowledge_nodes 
    WHERE type = 'concept' AND name = 'acne'
    ON CONFLICT DO NOTHING;

    INSERT INTO attention_values (node_id, sti, lti, vlti)
    SELECT id, 0.9, 0.7, true
    FROM knowledge_nodes 
    WHERE type = 'concept' AND name = 'treatment'
    ON CONFLICT DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Add ESN reservoir configurations
DO $$
BEGIN
  -- Add ESN reservoir configurations if table exists
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'esn_reservoirs') THEN
    -- Create ESN reservoir configurations
    INSERT INTO esn_reservoirs (name, reservoir_size, spectral_radius, connectivity, input_scaling, feedback_weight)
    VALUES 
      (
        'psoriasis_progression', 
        150, 
        0.85, 
        0.15, 
        1.0, 
        0.1
      ),
      (
        'eczema_progression', 
        150, 
        0.85, 
        0.15, 
        1.0, 
        0.1
      ),
      (
        'acne_progression', 
        150, 
        0.85, 
        0.15, 
        1.0, 
        0.1
      )
    ON CONFLICT (name) DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Add condition progression predictions
DO $$
BEGIN
  -- Add condition progression predictions if tables exist
  IF EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'condition_progression') 
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'skin_conditions')
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'treatments')
     AND EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'esn_reservoirs') THEN
    
    -- Add condition progression predictions
    INSERT INTO condition_progression (condition_id, treatment_id, esn_reservoir_id, progression_data, prediction_confidence)
    SELECT 
      c.id, 
      t.id, 
      r.id,
      '{
        "initial_severity": 0.8,
        "progression": [0.8, 0.75, 0.68, 0.62, 0.55, 0.48, 0.42, 0.38, 0.35, 0.32]
      }'::jsonb,
      0.85
    FROM 
      skin_conditions c,
      treatments t,
      esn_reservoirs r
    WHERE 
      c.name = 'psoriasis' AND 
      t.name = 'corticosteroid' AND
      r.name = 'psoriasis_progression'
    ON CONFLICT DO NOTHING;

    INSERT INTO condition_progression (condition_id, treatment_id, esn_reservoir_id, progression_data, prediction_confidence)
    SELECT 
      c.id, 
      t.id, 
      r.id,
      '{
        "initial_severity": 0.7,
        "progression": [0.7, 0.65, 0.58, 0.52, 0.45, 0.4, 0.36, 0.33, 0.31, 0.3]
      }'::jsonb,
      0.8
    FROM 
      skin_conditions c,
      treatments t,
      esn_reservoirs r
    WHERE 
      c.name = 'eczema' AND 
      t.name = 'corticosteroid' AND
      r.name = 'eczema_progression'
    ON CONFLICT DO NOTHING;

    INSERT INTO condition_progression (condition_id, treatment_id, esn_reservoir_id, progression_data, prediction_confidence)
    SELECT 
      c.id, 
      t.id, 
      r.id,
      '{
        "initial_severity": 0.75,
        "progression": [0.75, 0.7, 0.64, 0.58, 0.52, 0.47, 0.42, 0.38, 0.35, 0.33]
      }'::jsonb,
      0.85
    FROM 
      skin_conditions c,
      treatments t,
      esn_reservoirs r
    WHERE 
      c.name = 'acne' AND 
      t.name = 'retinoid' AND
      r.name = 'acne_progression'
    ON CONFLICT DO NOTHING;
  END IF;
END;
$$ LANGUAGE plpgsql;