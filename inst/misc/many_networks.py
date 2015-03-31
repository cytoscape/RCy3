import xmlrpclib
import os
import random
import string

server = xmlrpclib.ServerProxy("http://localhost:9000")
nr_nodes = 100
nr_edges = 400
for i in range(100):
    print "Creating network bla: " + str(i)
    netid = server.Cytoscape.createNetwork("bla:"+ str(i))
    nodes = []
    nodes_in = []
    nodes_out = []
    for i in range(nr_nodes):
    #    nodes.append(os.urandom(20))
        nodes.append(''.join(random.choice(string.letters + string.digits) for i in xrange(10)))
    server.Cytoscape.createNodes(netid, nodes)
    for i in range(nr_edges):
        nodes_in.append(nodes[random.randint(0, nr_nodes-1)])
        nodes_out.append(nodes[random.randint(0, nr_nodes-1)])
    types = ['relation'] * nr_edges
    directed = [True] * nr_edges
    server.Cytoscape.createEdges(netid, nodes_in, nodes_out, types, directed, True)
    server.Cytoscape.performLayout(netid, 'grid')
