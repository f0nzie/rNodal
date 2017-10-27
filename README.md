
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rNodal)](https://cran.r-project.org/package=rNodal) [![Travis-CI Build Status](https://travis-ci.org/f0nzie/rNodal.svg?branch=master)](https://travis-ci.org/f0nzie/rNodal) [![codecov](https://codecov.io/gh/f0nzie/rNodal/branch/develop/graph/badge.svg)](https://codecov.io/gh/f0nzie/rNodal)

rNodal
======

The goal of rNodal is to provide nodal analysis for oil and gas wells.

Installation
------------

You can install rNodal from github with:

``` r
# install.packages("devtools")
devtools::install_github("f0nzie/rNodal")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

Using `zFactor`
---------------

``` r
# use the new library zFactor
library(zFactor)

z.HallYarborough(pres.pr = 4.5, temp.pr = 1.4)
#> [1] 0.7373812
z.DranchukAbuKassem(pres.pr = 4.5, temp.pr = 1.4)
#> [1] 0.7396345
z.BeggsBrill(pres.pr = 4.5, temp.pr = 1.4)
#> [1] 0.7343367
z.Ann10(pres.pr = 4.5, temp.pr = 1.4)
#> [1] 0.736032
z.Papp(pres.pr = 4.5, temp.pr = 1.4)
#> [1] 0.7299354
```
