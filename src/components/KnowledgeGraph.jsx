import React, { useState, useEffect } from 'react'
import { useParams } from 'react-router-dom'
import { knowledgeAPI } from '../../supabase/client'

function KnowledgeGraph() {
  const { concept } = useParams()
  const [nodes, setNodes] = useState([])
  const [links, setLinks] = useState([])
  const [attentionValues, setAttentionValues] = useState([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    async function loadKnowledgeGraph() {
      try {
        const [nodesData, linksData, attentionData] = await Promise.all([
          knowledgeAPI.getNodes(),
          knowledgeAPI.getLinks(),
          knowledgeAPI.getAttentionValues()
        ])
        
        // Filter to relevant nodes and links
        const relevantNodes = nodesData.filter(node => 
          node.name === concept || 
          linksData.some(link => 
            link.source.name === concept && link.target.name === node.name ||
            link.target.name === concept && link.source.name === node.name
          )
        )
        
        const relevantLinks = linksData.filter(link =>
          link.source.name === concept || link.target.name === concept
        )
        
        setNodes(relevantNodes)
        setLinks(relevantLinks)
        setAttentionValues(attentionData)
      } catch (error) {
        console.error('Error loading knowledge graph:', error)
      } finally {
        setLoading(false)
      }
    }

    loadKnowledgeGraph()
  }, [concept])

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
          <h2 className="text-lg font-medium text-gray-900">
            Knowledge Graph: {concept}
          </h2>

          <div className="mt-6">
            <h3 className="text-sm font-medium text-gray-500">Direct Relationships</h3>
            <div className="mt-4 space-y-4">
              {links.map(link => {
                const sourceNode = nodes.find(n => n.id === link.source.id)
                const targetNode = nodes.find(n => n.id === link.target.id)
                const predicateNode = nodes.find(n => n.id === link.predicate.id)
                
                if (!sourceNode || !targetNode || !predicateNode) return null

                return (
                  <div key={link.id} className="bg-gray-50 p-4 rounded-lg">
                    <div className="flex items-center space-x-2">
                      <span className="text-sm font-medium text-gray-900">
                        {sourceNode.name}
                      </span>
                      <span className="text-sm text-gray-500">
                        {predicateNode.name}
                      </span>
                      <span className="text-sm font-medium text-gray-900">
                        {targetNode.name}
                      </span>
                    </div>
                    <div className="mt-2 flex items-center space-x-4">
                      <div className="flex items-center">
                        <span className="text-xs text-gray-500">Strength:</span>
                        <span className="ml-1 text-xs font-medium text-gray-900">
                          {Math.round(link.strength * 100)}%
                        </span>
                      </div>
                      <div className="flex items-center">
                        <span className="text-xs text-gray-500">Confidence:</span>
                        <span className="ml-1 text-xs font-medium text-gray-900">
                          {Math.round(link.confidence * 100)}%
                        </span>
                      </div>
                    </div>
                  </div>
                )
              })}
            </div>
          </div>

          <div className="mt-6">
            <h3 className="text-sm font-medium text-gray-500">Attention Values</h3>
            <div className="mt-4 space-y-4">
              {attentionValues
                .filter(av => nodes.some(n => n.id === av.node.id))
                .sort((a, b) => b.sti - a.sti)
                .map(av => (
                  <div key={av.id} className="bg-gray-50 p-4 rounded-lg">
                    <div className="flex justify-between items-center">
                      <span className="text-sm font-medium text-gray-900">
                        {av.node.name}
                      </span>
                      <div className="flex items-center space-x-4">
                        <div>
                          <span className="text-xs text-gray-500">STI:</span>
                          <span className="ml-1 text-xs font-medium text-gray-900">
                            {Math.round(av.sti * 100)}%
                          </span>
                        </div>
                        <div>
                          <span className="text-xs text-gray-500">LTI:</span>
                          <span className="ml-1 text-xs font-medium text-gray-900">
                            {Math.round(av.lti * 100)}%
                          </span>
                        </div>
                        {av.vlti && (
                          <span className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-indigo-100 text-indigo-800">
                            VLTI
                          </span>
                        )}
                      </div>
                    </div>
                    <div className="mt-2">
                      <div className="bg-gray-200 rounded-full h-1">
                        <div 
                          className="bg-indigo-600 rounded-full h-1" 
                          style={{ width: `${av.sti * 100}%` }}
                        />
                      </div>
                    </div>
                  </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}

export default KnowledgeGraph