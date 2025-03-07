import React, { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { skinConditionsAPI, skinLayersAPI } from '../../supabase/client'

function Dashboard() {
  const [conditions, setConditions] = useState([])
  const [layers, setLayers] = useState([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    async function loadData() {
      try {
        const [conditionsData, layersData] = await Promise.all([
          skinConditionsAPI.getAll(),
          skinLayersAPI.getAll()
        ])
        setConditions(conditionsData)
        setLayers(layersData)
      } catch (error) {
        console.error('Error loading data:', error)
      } finally {
        setLoading(false)
      }
    }

    loadData()
  }, [])

  if (loading) {
    return (
      <div className="flex justify-center items-center h-64">
        <div className="text-lg text-gray-600">Loading...</div>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      <div className="bg-white shadow sm:rounded-lg">
        <div className="px-4 py-5 sm:p-6">
          <h2 className="text-lg font-medium text-gray-900">Skin Conditions</h2>
          <div className="mt-4 grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
            {conditions.map(condition => (
              <div key={condition.id} className="bg-gray-50 p-4 rounded-lg">
                <h3 className="text-md font-medium text-gray-900">{condition.name}</h3>
                <p className="mt-1 text-sm text-gray-500">{condition.description}</p>
                <div className="mt-4 space-x-2">
                  <Link 
                    to={`/treatments/${condition.name}`}
                    className="inline-flex items-center px-3 py-1.5 border border-transparent text-xs font-medium rounded-full shadow-sm text-white bg-indigo-600 hover:bg-indigo-700"
                  >
                    View Treatments
                  </Link>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      <div className="bg-white shadow sm:rounded-lg">
        <div className="px-4 py-5 sm:p-6">
          <h2 className="text-lg font-medium text-gray-900">Skin Structure</h2>
          <div className="mt-4 space-y-4">
            {layers.map(layer => (
              <div key={layer.id} className="bg-gray-50 p-4 rounded-lg">
                <h3 className="text-md font-medium text-gray-900">{layer.name}</h3>
                <p className="mt-1 text-sm text-gray-500">{layer.description}</p>
                <Link 
                  to={`/knowledge/${layer.name}`}
                  className="mt-2 inline-flex items-center px-3 py-1.5 border border-transparent text-xs font-medium rounded-full shadow-sm text-white bg-indigo-600 hover:bg-indigo-700"
                >
                  View Knowledge Graph
                </Link>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}

export default Dashboard