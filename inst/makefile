default: build

RVER=R
#RVER=R-devel

help:
	egrep "^#" makefile | sed "s/^#//"

# --- build
# build and install
#

build:
	(cd ../..; $(RVER) CMD build --no-vignettes RCytoscape)
	(cd ../..; $(RVER) CMD install RCytoscape)

test:
	$(RVER) --vanilla < test.R > /dev/null

testverbose:
	$(RVER) --vanilla < test.R

# --- check
# check, but ignore man pages and vignette
#

check: build
	(cd ../..; $(RVER) CMD check --no-manual --no-vignettes RCytoscape)

# --- checkfull
# check everything
#

checkfull: 
	(cd ../..; $(RVER) --vanilla CMD build RCytoscape)
	(cd ../..; $(RVER) --vanilla CMD check --timings RCytoscape)


# --- codoc
# check just the man pages
#

codoc:
	(cd ../..; $(RVER) CMD check --no-manual --no-vignettes --no-tests --no-examples RCytoscape)



# --- sweave
# creates RCytoscape.tex, runs all embedded examples
# run this before the pdf target
#

sweave: 
	(cd doc; $(RVER) --no-save < ../sweave.R)

# --- pdf
# make and open RCytoscape.pdf, the vignette 
#

pdf:  sweave doc/RCytoscape.tex
	(cd doc; rm *.pdf; latex --output-format=pdf  RCytoscape.tex; open RCytoscape.pdf)

# --- vanillaTest
# run all the unit tests, in a clean context
#
vanillaTest:  build
	- rm vanillaTest.out
	$(RVER) --vanilla < vanillaTest.R > vanillaTest.out 2>&1

vt: vanillaTest

