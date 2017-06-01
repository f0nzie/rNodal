library(testthat)
library(rNodal)

report <- test_check("rNodal", reporter = "summary")
report <<- as.data.frame(report)
