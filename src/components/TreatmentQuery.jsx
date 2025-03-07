import React, { useState, useEffect } from 'react'
import { useParams, Link } from 'react-router-dom'
import { skinConditionsAPI } from '../../supabase/client'

function TreatmentQuery() {
  const { conditionName } = useParams()
  const [condition, setCondition] = useState(null)
  const [treatments, setTreatments] = useState([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    async function loadTreatments() {
      try {
        const conditionsData = await skinConditionsAPI.getAll()
        const condition = conditionsData.find(c => c.name === conditionName)
        
        if (condition) {
          setCondition(condition)
          const treatmentsData = await skinConditionsAPI.getTreatments(condition.id)
          setTreatments(treatmentsData)
        }
      } catch (error) {
        console.error('Error loading treatments:', error)
      } finally {
        setLoading(false)
      }
    }

    loadTreatments()
  }, [conditionName])

  if (loading) {
    return (
      <div className="flex justify-center items-center h-64">
        <div className="text-lg text-gray-600">Loading...</div>
      </div>
    )
  }

  if (!condition) {
    return (
      <div className="text-center py-12">
        <h3 className="text-lg font-medium text-gray-900">Condition not found</h3>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      <div className="bg-white shadow sm:rounded-lg">
        <div className="px-4 py-5 sm:p-6">
          <h2 className="text-lg font-medium text-gray-900">
            Treatments for {condition.name}
          </h2>
          <p className="mt-2 text-sm text-gray-500">{condition.description}</p>

          <div className="mt-6 grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
            {treatments.map(treatment => (
              <div key={treatment.id} className="bg-gray-50 p-4 rounded-lg">
                <h3 className="text-md font-medium text-gray-900">{treatment.name}</h3>
                <p className="mt-1 text-sm text-gray-500">{treatment.description}</p>
                
                <div className="mt-4">
                  <div className="flex justify-between text-sm">
                    <span className="text-gray-500">Efficacy</span>
                    <span className="font-medium text-gray-900">
                      {Math.round(treatment.efficacy * 100)}%
                    </span>
                  </div>
                  <div className="mt-1 bg-gray-200 rounded-full h-2">
                    <div 
                      className="bg-indigo-600 rounded-full h-2" 
                      style={{ width: `${treatment.efficacy * 100}%` }}
                    />
                  </div>
                </div>

                <div className="mt-4 space-y-2">
                  {treatment.formulation && (
                    <div>
                      <h4 className="text-xs font-medium text-gray-500">Formulation</h4>
                      <div className="mt-1 flex flex-wrap gap-1">
                        {treatment.formulation.map(form => (
                          <span 
                            key={form}
                            className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-gray-100 text-gray-800"
                          >
                            {form}
                          </span>
                        ))}
                      </div>
                    </div>
                  )}

                  {treatment.side_effects && (
                    <div>
                      <h4 className="text-xs font-medium text-gray-500">Side Effects</h4>
                      <div className="mt-1 flex flex-wrap gap-1">
                        {treatment.side_effects.map(effect => (
                          <span 
                            key={effect}
                            className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-red-100 text-red-800"
                          >
                            {effect}
                          </span>
                        ))}
                      </div>
                    </div>
                  )}
                </div>

                <div className="mt-4">
                  <Link
                    to={`/predict/${condition.name}/${treatment.name}`}
                    className="inline-flex items-center px-3 py-1.5 border border-transparent text-xs font-medium rounded-full shadow-sm text-white bg-indigo-600 hover:bg-indigo-700"
                  >
                    Predict Progression
                  </Link>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}

export default TreatmentQuery