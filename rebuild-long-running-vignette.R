old_wd <- getwd()

setwd("vignettes/")
knitr::knit("footBayes_a_rapid_guide.Rmd.orig", output = "footBayes_a_rapid_guide.Rmd")
knitr::purl("footBayes_a_rapid_guide.Rmd.orig", output = "footBayes_a_rapid_guide.R")

setwd(old_wd)
