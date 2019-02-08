library(testthat)

context("VLP functions")


test_that("standard P, T saved to environment", {
    expect_equal(readFromProjectEnv("PRES.ATM"), 14.7, tolerance = 0.001)
    expect_equal(readFromProjectEnv("TEMP.STD"), 60)
    expect_equal(readFromProjectEnv("TEMP.RANKINE"), 460)
})


test_that("MD-TVD table 1 matches", {
    md_tvd <- "
                MD      TVD
                0	     0
                600	    600
                1005	 1000
                4075	 4000
                7700	 7500
                9275	 9000
                "
    md_tvd_1 <- set_deviation_survey(md_tvd)
    save(md_tvd_1, file ="md_tvd.rda")
    print(class(md_tvd_1))
})


test_that("MD-TVD table 2 matches", {
    md_tvd_2 <- "
      MD      TVD     Pres  Temp
       0       0     500.0  135.8
     242.7   242.7   563.1  137.9
     485.5	 485.5	 627.5  139.8
     728.2	 728.2	 693.1	141.6
     970.9	 970.9	 759.8	143.4
    1213.6	1213.6	 827.6	144.9
    1456.4	1456.4	 896.5	146.3
    1699.1	1699.1	 966.4	147.6
    1941.8	1941.8	1037.3	148.6
    2184.5	2184.5	1109.3	149.3
    2427.3	2427.3	1182.2	149.8
    2670.0	2670.0	1255.9	150.0
"
    md_tvd_2 <- set_deviation_survey(md_tvd_2)
    save(md_tvd_2, file ="md_tvd.rda")
})
