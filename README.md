# Welcome to RCy3: 2.0 and Beyond
[![Travis-CI Build Status](https://travis-ci.org/cytoscape/RCy3.svg?branch=master)](https://travis-ci.org/cytoscape/RCy3)

Building upon the phenominal success of RCytoscape and RCy3, Cytoscape is adopting 
this project to provide a robust R package for the rapidly evolving Cytoscape 
ecosystem. We are beginning with a major refactor of RCy3 that includes:

* independence from the graphNEL object model
* harmonized function and argument names
* support for Cytoscape commands
* better support for Cytoscape apps
* see [NEWS](NEWS) for the complete release notes
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
install.packages("devtools")
library(devtools)
install_github('cytoscape/RCy3', build_vignettes=TRUE)
#If installation fails due to package 'XXX' not found,
# then run install.packages("XXX") and then try install_github('cytoscape/RCy3') again
library(RCy3)
```

#### Troubleshooting
1. If you see this error on a Mac: ```make: gfortran-4.8: No such file or directory```, then try reinstalling R via [homebrew](https://brew.sh/): ```brew update && brew reinstall r```
   * warning: this make take ~30 minutes
2. If you see this error in RStudio: ```ERROR: dependency ‘XML’ is not available for package```, then try this command: ```install.packages("XML")``` and then try installing RCy3 again.

## How to contribute
This is a public, open source project. Come on in! You can contribute at multiple levels:

* Report an issue or feature request
* Fork and make pull requests
* Contact current Cytoscape developers and inquire about joining the team

### Development
```
install.packages("devtools")
install.packages("roxygen2") 
library(devtools,roxygen2)
devtools::document()
devtools::check()
BiocCheck::BiocCheck('./')
```

### Testing
Unit tests are a crucial tool in software development.
In order to run them 'offline' (not on the Bioconductor build system),
take these steps from within a running R session:

  1) source(system.file("unitTests", "test_RCy3.R", package="RCy3"))
  2) run.tests()

They take about 4 minutes to run.

### Bioconductor
While this is the primary development repository for the RCy3 project, we also make regular pushes to official bioconductor repository ([devel](http://bioconductor.org/packages/devel/bioc/html/RCy3.html) & [release](http://bioconductor.org/packages/release/bioc/html/RCy3.html)) from which the official releases are generated. This is the correct repo for all coding and bug reporting interests. The tagged releases here correspond to the bioconductor releases via a manual syncing process. The `master` branch here corresponds to the latest code in development and not yet released. 

```
git commit -m "informative commit message"
git push origin master
git push upstream master
```
http://bioconductor.org/developers/how-to/git/push-to-github-bioc/

#### Vignettes
When adding or updating vignettes, consider the following tips for consistency:
* Copy/paste the header from an existing RCy3 vignette, including the global knitr options
* Number the *VignetteIndexEntry* names w.r.t. other vignettes (this determines their presentation order)
* Avoid markdown encoded links; only exposed URLs will translate to the PDF version
* Avoid spaces in Rmd filenames; causes CHECK errors
* When ready, run **Knit to html_vignette_** and review the generated html
* For PDF generation:
  * Uncomment ```highlight=FALSE``` in global knitr options and generate a new html. Unfortunately, the code highlighting looks terrible in PDF.
  * Pop the html out of the Viewer and into a browser
  * Choose print, then Save as PDF. Click *Print Background Colors* to retain code backgrounds. Replace spaces with hyphens in filename.
  * Review the generated PDF and comment out ```highlight=FALSE``` once again
* In the end, you should have Rmd and PDf versions of each vignette.

### Former Repo
The entire commit history has been preserved during this transition, but you can find the repo for RCy3 v1.5.3 and earlier
at https://github.com/tmuetze/Bioconductor_RCy3_the_new_RCytoscape.
