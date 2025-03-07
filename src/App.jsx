import React from 'react'
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom'
import Dashboard from './components/Dashboard'
import PatientAnalysis from './components/PatientAnalysis'
import TreatmentQuery from './components/TreatmentQuery'
import ProgressionPrediction from './components/ProgressionPrediction'
import KnowledgeGraph from './components/KnowledgeGraph'

function App() {
  return (
    <Router>
      <div className="min-h-screen bg-gray-50">
        <nav className="bg-white shadow-sm">
          <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
            <div className="flex justify-between h-16">
              <div className="flex">
                <div className="flex-shrink-0 flex items-center">
                  <h1 className="text-xl font-bold text-gray-900">SkinTwin</h1>
                </div>
              </div>
            </div>
          </div>
        </nav>

        <main className="max-w-7xl mx-auto py-6 sm:px-6 lg:px-8">
          <Routes>
            <Route path="/" element={<Dashboard />} />
            <Route path="/analyze/:patientId" element={<PatientAnalysis />} />
            <Route path="/treatments/:conditionName" element={<TreatmentQuery />} />
            <Route path="/predict/:conditionName/:treatmentName" element={<ProgressionPrediction />} />
            <Route path="/knowledge/:concept" element={<KnowledgeGraph />} />
          </Routes>
        </main>
      </div>
    </Router>
  )
}

export default App