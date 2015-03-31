#!/usr/bin/perl 

# An example of how to call CytoscapeRPC from Perl.
# Jan Bot | 27/04/2010
# Modified & cleaned 18/08/2010

use strict;
use warnings;
use Frontier::RPC2;
use Frontier::Client;

# Tell Frontier where the server lives
my $url = "http://localhost:9000/Cytoscape";
my $client = Frontier::Client->new( url     => $url,
                                    debug   => 0,
                                    );

# We need a coder because of the 'intelligent' var casting in perl
my $coder = Frontier::RPC2->new;

# Create a network to work with
my $networkID = $client->call(
        'Cytoscape.createNetwork',      # function name
        'Perl Test Network'             # name of the network to create
);

# Lets see if the name we gave the network is still the same
my $networkTitle = $client->call(
        'Cytoscape.getNetworkTitle',    # function name
        $coder->string("$networkID")    # identifier of the network
);
print $networkTitle, "\n";

# Create some nodes.
my $done = $client->call(
        'Cytoscape.createNodes',        # function name
        ["a", "b", "c", "d", "e", "f", "g"] # list of node names
);
print "Done adding nodes.\n";
my $nrNodes = $client->call('Cytoscape.countAllNodes');
print "There are now ", $nrNodes, " nodes present in Cytoscape.\n";

# Add some attributes.
$done = $client->call(
        'Cytoscape.addNodeAttributes',  # function name
        $coder->string("str"),          # name of the attribute to add
        $coder->string("STRING"),       # type of the attribute
        {                               # hash of key-value pairs for the
            'a' => 'aa',                # attributes
            'b' => 'aa',
            'c' => 'bb',
            'd' => 'bb',
            'e' => 'cc',
            'f' => 'cc',
            'g' => 'cc'
        },
        $coder->boolean(1)              # forgive parameter
);

$done = $client->call(
    'Cytoscape.addNodeAttributes',
    $coder->string('nr'),
    $coder->string("FLOATING"),
    {
        'a' => 1.10,
        'b' => 2.10,
        'c' => 3.10,
        'd' => 4.10,
        'e' => 5.10,
        'f' => 6.10,
        'g' => 7.10
    },
    $coder->boolean(1)
);

# Add edges
print "Adding edges.\n";
my @from = ("b", "b", "c", "d", "d", "d", "d");
my @to =   ("b", "c", "d", "a", "e", "f", "g");
my @type = ("relation") x 7;
my @directed = ($coder->boolean(1)) x 7;
$done = $client->call(
        'Cytoscape.createEdges',        # function name
        $coder->string("$networkID"),   # identifier of the network
        \@from,                         # list of node names (from)
        \@to,                           # list of node names (to)
        \@type,                         # list of types of the edges
        \@directed,                     # list of booleans
        $coder->boolean(1)              # forgive parameter
);
print "Done adding edges.\n";
my $nrEdges = $client->call("Cytoscape.countAllEdges");
print "There are now " . $nrEdges . " edges present in Cytoscape.\n";

# Set node selection
$done = $client->call(
        "Cytoscape.setSelectedNodes",   # function name
        \@from                          # nodes to select
);

# Get a list of the selected nodes
print "calling getSelectedNodes...";
my $out = $client->call("Cytoscape.getSelectedNodes");
print @$out;

# Perform force-directed Layout
# Take a look at the possible layouts with: getLayoutNames
$done = $client->call(
        'Cytoscape.performLayout',
        $coder->string("$networkID"),
        "force-directed"
);

# Set another attribute as the node label
# $done = $client->call(
#         "Cytoscape.setNodeLabel",       # function name
#         $coder->string("$networkID"),   # identifier of the network
#         "str",                          # name of the attribute to use
#         "defaultvalue",                 # default value
#         "default"                       # name of the vizmapper to use
# );

# Force a redraw of the current network
$done = $client->call("Cytoscape.redraw");

# Set the shapes of the nodes based on another attribute
$done = $client->call(
        "Cytoscape.discreteMapper",     # function name
        $coder->string("$networkID"),   # identifier of the network
        "default",                      # name of the vizmap to use
        "str",                          # name of the attribute to use
        "Node Shape",                   # name of the vis. param. to change
        "triangle",                     # default value
        {                               # hash of nodeid - shape
            'aa' => 'triangle',
            'bb' => 'ellipse',
            'cc' => 'rect'
        }
);

$done = $client->call(
    "Cytoscape.createContinuousNodeVisualStyle",
    $coder->string("nr"),
    $coder->string("Node Color"),
    [2.1, 2.51, 7.3, 8.2],
    ['FFFFFF', '#FFFF00', '#FF0000', '#000000', '#0000FF', '#AAAAAA'],
);

print "\n";
