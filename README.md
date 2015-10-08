# Bioconductor_RCytoscape
Update RCytoscape to work for Cytoscape 3.1.1 and higher using CyREST

## RCy3 Environment Setup

In addition to this package (RCy3), you will need:

1. Java SE 8. Cytoscape and CyREST currently do not support Java SE 6 anymore. It can be can downloaded from Oracle here: http://www.oracle.com/technetwork/java/javase/downloads/index.html. Java SE 7 works only in combination with Cytoscape 3.2.0.
2. Cytoscape 3.2.1+ (with installed CyREST plugin 0.9.14+). Cytoscape can be downloaded using this URL: http://www.cytoscape.org/download.php. The CyREST plugin, a Cytoscape plugin which provides the Cytoscape end of the communication layer, can be easily be installed via the integrated app store or it can be downloaded from here: http://apps.cytoscape.org/apps/cyrest. See instructions below.
3. R 3.1.3+ (and RStudio as IDE if desired) which can be downloaded here: http://cran.r-project.org.

## Setup steps:

First install Java, then Cytoscape and follow the instructions on the screen.

Option 1: Open Cytoscape, click on "Apps" in the menubar and select "App Manager". In the App Manager on the "Install Apps" tab, type "CyREST" in the search bar and click on cyREST when it appears and then click on "Install".

Option 2: Alternatively you can download the CyREST plugin as .jar file from Cytoscape's app store here: http://apps.cytoscape.org/apps/cyrest. Then, open Cytoscape, click on "Apps" in the menubar and select "App Manager". In the App Manager on the "Install Apps" tab click on "Install from File...", choose the .jar file that you just downloaded, then click "Open" and then "Install". You are all set!

(To work on the project, you will also need the following packages: RCurl, RJSONIO, httr, igraph as well as the Bioconductor packages: graph.) source("http://bioconductor.org/biocLite.R") biocLite("graph")

More background reading: http://www.biomedcentral.com/1471-2105/14/217
