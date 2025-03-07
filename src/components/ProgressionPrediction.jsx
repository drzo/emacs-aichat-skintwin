import React, { useState, useEffect } from 'react'
import { useParams } from 'react-router-dom'
import { progressionAPI } from '../../supabase/client'

function ProgressionPrediction() {
  const { conditionName, treatmentName } = useParams()
  const [prediction, setPrediction] = useState(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    async function loadPrediction() {
      try {
        const predictions = await progressionAPI.getAllForCondition(conditionName)
        const matchingPrediction = predictions.find(p => 
          p.treatment.name === treatmentName
        )
        setPrediction(matchingPrediction)
      } catch (error) {
        console.error('Error loading prediction:', error)
      } finally {
        setLoading(false)
      }
    }

    loadPrediction()
  }, [conditionName, treatmentName])

  if (loading) {
    return (
      <div className="flex justify-center items-center h-64">
        <div className="text-lg text-gray-600">Loading...</div>
      </div>
    )
  }

  if (!prediction) {
    return (
      <div className="text-center py-12">
        <h3 className="text-lg font-medium text-gray-900">
          No prediction available for this combination
        </h3>
      </div>
    )
  }

  const progressionData = prediction.progression_data
  const timePoints = Object.keys(progressionData).sort((a, b) => Number(a) - Number(b))
  const maxSeverity = Math.max(...Object.values(progressionData))

  return (
    <div className="space-y-6">
      <div className="bg-white shadow sm:rounded-lg">
        <div className="px-4 py-5 sm:p-6">
          <h2 className="text-lg font-medium text-gray-900">
            Predicted Progression: {conditionName} with {treatmentName}
          </h2>
          
          <div className="mt-6">
            <h3 className="text-sm font-medium text-gray-500">Confidence</h3>
            <div className="mt-2 flex items-center">
              <div className="flex-1">
                <div className="bg-gray-200 rounded-full h-2">
                  <div 
                    className="bg-indigo-600 rounded-full h-2" 
                    style={{ width: `${prediction.prediction_confidence * 100}%` }}
                  />
                </div>
              </div>
              <span className="ml-3 text-sm font-medium text-gray-900">
                {Math.round(prediction.prediction_confidence * 100)}%
              </span>
            </div>
          </div>

          <div className="mt-6">
            <h3 className="text-sm font-medium text-gray-500">Progression Timeline</h3>
            <div className="mt-4 relative">
              <div className="absolute inset-0 flex items-center" aria-hidden="true">
                <div className="w-full border-t border-gray-200" />
              </div>
              <div className="relative flex justify-between">
                {timePoints.map((time, index) => {
                  const severity = progressionData[time]
                  const percentage = Math.round((severity / maxSeverity) * 100)
                  
                  return (
                    <div 
                      key={time}
                      className="flex flex-col items-center"
                      style={{ height: '150px' }}
                    >
                      <div 
                        className="bg-indigo-600 rounded-t"
                        style={{ 
                          height: `${percentage}%`,
                          width: '20px'
                        }}
                      />
                      <div className="mt-2 text-xs text-gray-500">
                        Week {time}
                      </div>
                      <div className="text-xs font-medium text-gray-900">
                        {Math.round(severity * 100)}%
                      </div>
                    </div>
                  )
                })}
              </div>
            </div>
          </div>

          <div className="mt-6">
            <h3 className="text-sm font-medium text-gray-500">Analysis</h3>
            <div className="mt-2 text-sm text-gray-500">
              {prediction.prediction_confidence > 0.8 ? (
                <p>High confidence prediction suggests good treatment response.</p>
              ) : prediction.prediction_confidence > 0.5 ? (
                <p>Moderate confidence in prediction. Monitor response closely.</p>
              ) : (
                <p>Low confidence prediction. Consider alternative treatments.</p>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}

export default ProgressionPrediction