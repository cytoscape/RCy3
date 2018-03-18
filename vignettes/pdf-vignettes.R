setwd("/git/RCy3/vignettes")
require(knitr)
require(markdown)

knit("Overview.Rmd")
markdownToHTML('Overview.md', 'Overview.html', options=c("use_xhml"))
system("pandoc -s Overview.html -o Overview.pdf")
