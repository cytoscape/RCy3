# Welcome to RCy3: 2.0 and Beyond
Building upon the phenominal success of RCytoscape and RCy3, Cytoscape is adopting the project (and primary responsibility) 
to provide a robust R package for the rapidly evolving Cytoscape ecosystem. We are beginning with a major refactor of RCy3
that includes:

* independence from the graphNEL object model
* optional object-based arguments
* harmonized function and argument names
* support for Cytoscape commands
* better support for Cytoscape apps
* coordinated development with CyREST and the Cytoscape service model
  * [New API](https://github.com/cytoscape/cyREST/issues?utf8=✓&q=milestone%3A*+label%3A%22new+API%22+is%3A*)
* coordinated development with other scripting libraries, e.g., 
  * [py2cytoscape](https://github.com/cytoscape/py2cytoscape)
  * [RCyjs](http://bioconductor.org/packages/release/bioc/html/RCyjs.html)

## How to install
**_Official bioconductor releases_ (recommended)**
```
source("https://bioconductor.org/biocLite.R")
biocLite("RCy3")
```

_Unstable development code from this repo_ (at your own risk)
```
install_github('cytoscape/RCy3')
library(RCy3)
```

## How to contribute
This is a public, open source project. Come on in! You can contribute at multiple levels:

* Report an issue or feature request
* Fork and make pull requests
* Contact current Cytoscape developers and inquire about joining the team

## Testing
Unit tests are a crucial tool in software development.
In order to run them 'offline' (not on the Bioconductor build system),
take these steps from within a running R session:

  1) source(system.file("unitTests", "test_RCy3.R", package="RCy3"))
  2) run.tests ()

They take about 4 minutes to run.

## Bioconductor
While this is the primary development repository for the RCy3 project, we also make regular pushes to official bioconductor repository ([devel](http://bioconductor.org/packages/devel/bioc/html/RCy3.html) & [release](http://bioconductor.org/packages/release/bioc/html/RCy3.html)) from which the official releases are generated. This is the correct repo for all coding and bug reporting interests. The tagged releases here correspond to the bioconductor releases via a manual syncing process. The `master` branch here corresponds to the latest code in development and not yet released. 

## Former Repo
The entire commit history has been preserved during this transition, but you can find the repo for RCy3 v1.5.3 and earlier
at https://github.com/tmuetze/Bioconductor_RCy3_the_new_RCytoscape.
