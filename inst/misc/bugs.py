import xmlrpclib
server = xmlrpclib.ServerProxy("http://localhost:9000")
#------------------------------------------------------------------------------------------------------------------------
def edgeTargetArrowColorBug ():

  network_id = '389'   # reassign this as necesseary 

  #edgePropName = 'Edge Target Arrow Color'
  #edgePropName = 'Edge Source Arrow Color'
  edgePropName = 'Edge Color'

  server.Cytoscape.discreteMapper (network_id, 'default', 'edgeType', edgePropName, '#FFFFFF',
                                   ["phosphorylates", "synthetic lethal", "undefined"],
                                   ["#0000FF", "#00FF00", "#FF0000"])

    # 'score' is an edge attribute with these values
    #   A-B:  35
    #   B-C:  -12
    #   C-A:  0

    # try to call     
    # public boolean createContinuousEdgeVisualStyle(String attrName,
    #                     String edgePropName, List<Object> attrValues,
    #                     List<Object> propValues) throws XmlRpcException {

  server.Cytoscape.createContinuousEdgeVisualStyle ('score', edgePropName,
                                                     [-40.0, 0.0, 40.0],
                                                     ["#0000FF", "#FFFFFF", "#FF0000"])
  # xmlrpclib.Fault: <Fault 0:  'Failed to invoke method createContinuousEdgeVisualStyle in class 
  # tudelft.CytoscapeRPC.CytoscapeRPCCallHandler: java.lang.String cannot be cast to java.lang.Double'>


#------------------------------------------------------------------------------------------------------------------------

