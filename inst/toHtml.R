# go.R
#------------------------------------------------------------------------------------------------------------------------
library (RUnit)
library (tools)
#------------------------------------------------------------------------------------------------------------------------
run = function (levels, trace=FALSE)
{
  if (0 %in% levels) {
    if (trace) print ('')
    else {
      #man.directory <<- '~/s/src/R/bioc/RCytoscape/man'
      man.directory <<- '~/s/src/R/bioc/rcy-devel/RCytoscape/man'
      setwd (man.directory)
      rd.files <<-list.files ('.', pattern='Rd')
      } # else
    } # 0

  if (1 %in% levels) {
    if (trace) print ('')
    else {
      for (file in rd.files) {
        target.dir <<- '~/tmp/html'
        input <<- paste (man.directory, file, sep='/')
        basename = strsplit (file, '\\.Rd')[[1]][1]
        output <<- paste (target.dir, "/",  basename, '.html', sep='')
        print (sprintf ('%s -> %s', input, output))
        Rd2HTML (input, out=output)
        } # for file
      } # else
    } # 1

} # run
#------------------------------------------------------------------------------------------------------------------------
#run (0)
#run (1)


