## Test environments
* win-builder (devel and release)
* local OS X install, R 4.1.1
* local Linux, R 4.1.1

## R CMD check results
There were no ERRORs or WARNINGs. 

When checking on R Devel, there is a warning I am not able to fix (after a deep check 
and mail exchange with CRAN team). 

* checking re-building of vignette outputs ... [54s] WARNING
Error(s) in re-building vignettes:
--- re-building 'footBayes_a_rapid_guide.Rmd' using rmarkdown
...
Error: processing vignette 'footBayes_a_rapid_guide.Rmd' failed with diagnostics:
invalid connection
--- failed re-building 'footBayes_a_rapid_guide.Rmd'

SUMMARY: processing the following file failed:
  'footBayes_a_rapid_guide.Rmd'

Error: Vignette re-building failed.
Execution halted

  
## Downstream dependencies
There are currently no downstream dependencies for this package.

Thank you

