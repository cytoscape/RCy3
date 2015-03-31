library (RUnit)
source ('unitTests/test_cytoscape.R')

if (!exists ('cy')) {
  write ('creating global variable "cy" -- an instance of CytoscapeConnection', stderr ())
  cy = CytoscapeConnection ()
  }

#------------------------------------------------------------------------------------------------------------------------
reload.rcytoscape = function ()
{  
  if (length (grep ('RCytoscape', names (sessionInfo()$otherPkgs))) > 0)
    detach (package:RCytoscape)

  library (RCytoscape)

} # reload.rcytoscape
#------------------------------------------------------------------------------------------------------------------------
getVizMapList = function (cw)
{
  return (xml.rpc (cw@uri, 'Cytoscape.getVisualStyleNames'))

} # getVizMapList
#------------------------------------------------------------------------------------------------------------------------
current.vizmap = function (cw)
{
  return (xml.rpc (cw@uri, 'Cytoscape.getCurrentVisualStyle'))

} #  vizmap.names
#------------------------------------------------------------------------------------------------------------------------
set.vizmap = function (cw, style.name)
{
  invisible (xml.rpc (cw@uri, 'Cytoscape.setVisualStyle', style.name))

} # set.vizmap
#------------------------------------------------------------------------------------------------------------------------
copy.vizmap = function (cw, from.name, to.name)
{
  invisible (xml.rpc (cw@uri, 'Cytoscape.copyVisualStyle', from.name, to.name))

} # copy.vizmap
#------------------------------------------------------------------------------------------------------------------------
set.nodeShape = function (cw)
{
  node.shapes = c ('triangle', 'rect', 'ellipse')
  node.shapes = c ('diamond', 'triangle', 'rect')
  node.shapes = c ('ellipse', 'trapezoid', 'triangle')

  attribute.values = c ('kinase', 'glycoprotein', 'transcription factor')
  window.id = cw@window.id
  style.name = 'default'
  style.name = 'newViz'

  xml.rpc (cw@uri, "Cytoscape.setNodeShapeRule", window.id, style.name, 'type', 'ellipse', attribute.values, node.shapes, .convert=TRUE)

}
#------------------------------------------------------------------------------------------------------------------------
# temporary (25 dec 2010)
parse.pos = function (position.strings)
{
  tokens = strsplit (position.strings, ":")
  result = list ()
  for (token in tokens) {
     name = token [1]
     xy.tokens = strsplit (token [2], ",")
     x = as.integer (xy.tokens[[1]][1])
     y = as.integer (xy.tokens[[1]][2])
     printf ('%s:  %d, %d', name, x, y)
     result [[name]] = list (x=x, y=y)
    } # for token

  return (result)
  
} # parse.pos
#------------------------------------------------------------------------------------------------------------------------
