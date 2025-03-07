import React, { useState, useEffect } from 'react'
import { useParams } from 'react-router-dom'
import { patientsAPI } from '../../supabase/client'

function PatientAnalysis() {
  const { patientId } = useParams()
  const [patient, setPatient] = useState(null)
  const [conditions, setConditions] = useState([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    async function loadPatientData() {
      try {
        const [patientData, conditionsData] = await Promise.all([
          patientsAPI.getCurrentPatient(),
          patientsAPI.getConditions(patientId)
        ])
        setPatient(patientData)
        setConditions(conditionsData)
      } catch (error) {
        console.error('Error loading patient data:', error)
      } finally {
        setLoading(false)
      }
    }

    loadPatientData()
  }, [patientId])

  if (loading) {
    return (
      <div className="flex justify-center items-center h-64">
        <div className="text-lg text-gray-600">Loading...</div>
      </div>
    )
  }

  if (!patient) {
    return (
      <div className="text-center py-12">
        <h3 className="text-lg font-medium text-gray-900">Patient not found</h3>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      <div className="bg-white shadow sm:rounded-lg">
        <div className="px-4 py-5 sm:p-6">
          <h2 className="text-lg font-medium text-gray-900">Patient Information</h2>
          <div className="mt-4 grid grid-cols-1 gap-4 sm:grid-cols-2">
            <div>
              <h3 className="text-sm font-medium text-gray-500">Age</h3>
              <p className="mt-1 text-sm text-gray-900">{patient.age}</p>
            </div>
            <div>
              <h3 className="text-sm font-medium text-gray-500">Gender</h3>
              <p className="mt-1 text-sm text-gray-900">{patient.gender}</p>
            </div>
            <div>
              <h3 className="text-sm font-medium text-gray-500">Skin Type</h3>
              <p className="mt-1 text-sm text-gray-900">{patient.skin_type}</p>
            </div>
          </div>
        </div>
      </div>

      <div className="bg-white shadow sm:rounded-lg">
        <div className="px-4 py-5 sm:p-6">
          <h2 className="text-lg font-medium text-gray-900">Conditions</h2>
          <div className="mt-4 space-y-4">
            {conditions.map(condition => (
              <div key={condition.id} className="bg-gray-50 p-4 rounded-lg">
                <div className="flex justify-between items-start">
                  <div>
                    <h3 className="text-md font-medium text-gray-900">
                      {condition.skin_conditions.name}
                    </h3>
                    <p className="mt-1 text-sm text-gray-500">
                      {condition.skin_conditions.description}
                    </p>
                  </div>
                  <div className="text-right">
                    <div className="text-sm text-gray-500">Severity</div>
                    <div className="text-lg font-medium text-gray-900">
                      {Math.round(condition.severity * 100)}%
                    </div>
                  </div>
                </div>
                {condition.notes && (
                  <div className="mt-4">
                    <h4 className="text-sm font-medium text-gray-500">Notes</h4>
                    <p className="mt-1 text-sm text-gray-900">{condition.notes}</p>
                  </div>
                )}
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}

export default PatientAnalysis