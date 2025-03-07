# SkinTwin: OpenCog-Based Dermatological Model for Emacs

## Overview

SkinTwin is a multiscale dermatological model powered by OpenCog's cognitive architecture components, implemented for Emacs. This system integrates various modules to represent skin biology, environmental factors, and clinical outcomes through a comprehensive knowledge graph and reasoning system.

## Key Components

- **DermatoGraph (AtomSpace)**: Knowledge representation system storing data about skin biology, conditions, and treatments
- **SensoryFocus (ECAN)**: Attention allocation system that prioritizes relevant information
- **DermatoLogic (PLN)**: Probabilistic reasoning system for deriving logical inferences about skin health 
- **EpidermiLearn (MOSES)**: Pattern mining and model building for treatment efficacy prediction
- **RuleDerm (URE)**: Rule-based reasoning system for executing logical rules
- **ESN Prediction**: Temporal pattern recognition for disease progression modeling

## Installation

This package requires [aichat](https://github.com/xhcoding/emacs-aichat) which implements the core OpenCog cognitive architecture components.

```elisp
;; Clone the repository
(add-to-list 'load-path "/path/to/skintwin")

;; Load the package
(require 'skintwin)

;; Enable the interactive mode (optional)
(skintwin-mode 1)
```

## Usage

### Basic Commands

- `M-x skintwin-initialize` - Initialize the SkinTwin system
- `M-x skintwin-dashboard` - Display the main dashboard
- `M-x skintwin-query-treatments` - Query treatments for a skin condition
- `M-x skintwin-analyze-patient` - Analyze a patient's skin conditions
- `M-x skintwin-predict-progression` - Predict disease progression with treatment

### Interactive Mode

Enable the minor mode with `M-x skintwin-mode`. This provides convenient keybindings:

- `C-c s d` - Display dashboard
- `C-c s q` - Query treatments
- `C-c s a` - Analyze patient
- `C-c s p` - Predict progression
- `C-c s v` - Visualize knowledge graph
- `C-c s h` - Display help

### Org Mode Integration

When working with Org files:

- `C-c o k` - Convert the current heading to knowledge base entries
- `C-c o b` - Convert the entire buffer to the knowledge base
- `C-c o v` - Visualize attention flow
- `C-c o q` - Query related concepts
- `C-c o r` - Apply reasoning

## Example Workflow

1. Initialize the system with `C-c s i`
2. Open the dashboard with `C-c s d`
3. Add a patient with `C-c s P` then press `a`
4. Add a condition to the patient using `c`
5. Analyze the patient with `C-c s a`
6. Query treatments for a condition with `C-c s q`
7. Predict disease progression with `C-c s p`

## Extending the Knowledge Base

The knowledge base can be extended using Org mode:

```org
* Skin Condition
:PROPERTIES:
:STI: 0.900
:LTI: 0.700
:END:

** Rosacea
:PROPERTIES:
:STI: 0.800
:END:

Chronic inflammatory skin condition affecting the face.
has_symptom(facial_redness)
has_symptom(visible_blood_vessels)
has_symptom(papules)
has_trigger(sun_exposure)
```

Convert this to the knowledge base with `C-c o k`.

## Visualization

SkinTwin provides multiple visualization options:

- Knowledge graph visualization
- Attention heatmaps
- Disease progression predictions
- Network analysis

Access these through the visualization dashboard with `C-c s V`.

## License

This project is available under the GPLv3 License.