# Welcome to RCy3: 2.0 and Beyond

[![BioC Release Build Status](http://bioconductor.org/shields/build/release/bioc/RCy3.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/RCy3/) - Bioconductor Release Build

[![BioC Dev Build Status](http://bioconductor.org/shields/build/devel/bioc/RCy3.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/RCy3/) - Bioconductor Dev Build

[![Travis-CI Build Status](https://travis-ci.org/cytoscape/RCy3.svg?branch=master)](https://travis-ci.org/cytoscape/RCy3) - GitHub Dev Build by Travis

Building upon the phenominal success of RCytoscape and RCy3, Cytoscape is adopting 
this project to provide a robust R package for the rapidly evolving Cytoscape 
ecosystem. We are beginning with a major refactor of RCy3 that includes:

* independence from the graphNEL object model
* harmonized function and argument names
* support for Cytoscape commands
* better support for Cytoscape apps
* see [NEWS](https://github.com/cytoscape/RCy3/blob/master/NEWS) for the complete release notes
* coordinated development with CyREST and the Cytoscape service model
  * [New API](https://github.com/cytoscape/cyREST/issues?utf8=✓&q=milestone%3A*+label%3A%22new+API%22+is%3A*)
* coordinated development with other scripting libraries, e.g., 
  * [py4cytoscape](https://github.com/cytoscape/py4cytoscape)
  * [RCyjs](http://bioconductor.org/packages/release/bioc/html/RCyjs.html)

## Getting Started
 * [Documenation site](http://cytoscape.org/RCy3/index.html) 
 * [Cytoscape Rmd noteboods](https://cytoscape.org/cytoscape-automation/for-scripters/R/notebooks/)
 
## How to install
**_Official bioconductor releases_ (recommended)**
```
install.packages("BiocManager")
BiocManager::install("RCy3")
```
*Note: Be sure to use the [latest Bioconductor](https://www.bioconductor.org/install/) and recommended R version*  

**_Development version from this repo_ (at your own risk)**
```
install.packages("devtools")
library(devtools)
install_github('cytoscape/RCy3', build_vignettes=FALSE)
#If installation fails due to package 'XXX' not found,
# then run install.packages("XXX") and then try install_github('cytoscape/RCy3') again
library(RCy3)
```

#### Troubleshooting
1. If you see this error on a Mac: ```make: gfortran-4.8: No such file or directory```, then try reinstalling R via [homebrew](https://brew.sh/): ```brew update && brew reinstall r```
   * warning: this may take ~30 minutes
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
devtools::install_github("AlexanderPico/docthis")
library(docthis) #shift+cmd+D 
BiocManager::install("BiocStyle")
library(BiocStyle)
BiocManager::install("BiocCheck")
library(BiocCheck)
install.packages("RUnit")
library(RUnit)
setwd("/git/cytoscape/RCy3") #customize to your setup
devtools::document()
devtools::check(args = "--no-examples", vignettes = F)
BiocCheck::BiocCheck('./')
```

### Testing
Unit tests are a crucial tool in software development.
In order to run them 'offline' (not on the Bioconductor build system),
take these steps from within a running R session (requires RUnit):

```
source(system.file("unitTests", "test_RCy3.R", package="RCy3"))
run.tests()
```

They take about 4 minutes to run.


### Updating site
We use [pkgdown](https://pkgdown.r-lib.org/) to generate the [main site for RCy3](http://cytoscape.org/RCy3/index.html) based on this README, metadata, man pages and vignettes. If you make changes to any of these, please take a moment to regenerate the site:
```
library(pkgdown)
pkgdown::build_site(examples=FALSE)
```


### Bioconductor
While this is the primary development repository for the RCy3 project, we also make regular pushes to official bioconductor repository ([devel](http://bioconductor.org/packages/devel/bioc/html/RCy3.html) & [release](http://bioconductor.org/packages/release/bioc/html/RCy3.html)) from which the official releases are generated. This is the correct repo for all coding and bug reporting interests. The tagged releases here correspond to the bioconductor releases via a manual syncing process. The `master` branch here corresponds to the latest code in development and not yet released. 

```
git commit -m "informative commit message"
git push origin master
git push upstream master
```
http://bioconductor.org/developers/how-to/git/push-to-github-bioc/

Following each bioconductor release, a `RELEASE_#_#` branch is created. The new branch is fetched and master is updated:

```
git fetch upstream
git checkout -b RELEASE_3_13 upstream/RELEASE_3_13
git push origin RELEASE_3_13
git checkout master
git pull upstream master
git push origin master
```

Only bug fixes and documentation updates can be pushed to the official bioconductor release branch. After committing and pushing fixes to `master`, then:

```
git checkout RELEASE_3_13
git cherry-pick master #for lastest commit
# or git cherry-pick 1abc234 #for specific commit
# or git cherry-pick 1abc234^..5def678 #for an inclusive range
# bump release version in DESCRIPTION
git commit -am 'version bump'
git push origin RELEASE_3_13
# double check changes, and then...
git push upstream RELEASE_3_13
git checkout master
# bump dev version in DESCRIPTION
git commit -am 'version bump'
git push origin master
git push upstream master
```

https://bioconductor.org/developers/how-to/git/bug-fix-in-release-and-devel/

### Vignettes
When adding or updating vignettes, consider the following tips for consistency:
* Copy/paste the header from an existing RCy3 vignette, including the global knitr options
* Number the *VignetteIndexEntry* names w.r.t. other vignettes (this determines their presentation order)
* Avoid spaces in Rmd filenames; causes CHECK errors
* When ready, run **Knit to html_document** and review the generated html (requires BiocStyle)
* Note: you don't need to save the html version; it will be generated anew at Bioconductor.
* In the end, you should just have an Rmd version of each vignette in the repo.

### Former Repo
The entire commit history has been preserved during this transition, but you can find the repo for RCy3 v1.5.3 and earlier
at https://github.com/tmuetze/Bioconductor_RCy3_the_new_RCytoscape and you can find the Bioconductor
page for v1.8.0, including archives at https://www.bioconductor.org/packages/3.6/bioc/html/RCy3.html.

```Note to repository maintainers: Please *DO NOT* move this page ... the Cytoscape Automation paper refers directly to it.```
