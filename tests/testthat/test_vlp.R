library(testthat)

context("Test MD vs TVD conversion from text to dataframe")

rda_file <- "md_tvd.rda"
if (!file.exists(rda_file)) stop("No test table.\nUse the table generator first\n")
load(rda_file)

test_that("standard P, T saved to environment", {
    expect_equal(readFromProjectEnv("PRES.ATM"), 14.7, tolerance = 0.001)
    expect_equal(readFromProjectEnv("TEMP.STD"), 60)
    expect_equal(readFromProjectEnv("TEMP.RANKINE"), 460)
})


test_that("MD-TVD table 1 matches", {
    md_tvd_txt_1 <- "
            MD      TVD
            0	     0
            600	    600
            1005	 1000
            4075	 4000
            7700	 7500
            9275	 9000
            "
    md_tvd_t_1 <- set_deviation_survey(md_tvd_txt_1)
    load(rda_file)
    expect_equal(class(md_tvd_t_1), "data.frame")
    expect_equal(md_tvd_t_1, md_tvd_1)
})


test_that("MD-TVD table 2 matches", {
    md_tvd_txt_2 <- "
          MD      TVD
           0       0
         242.7   242.7
         485.5	 485.5
         728.2	 728.2
         970.9	 970.9
        1213.6	1213.6
        1456.4	1456.4
        1699.1	1699.1
        1941.8	1941.8
        2184.5	2184.5
        2427.3	2427.3
        2670.0	2670.0
"
    md_tvd_t_2 <- set_deviation_survey(md_tvd_txt_2)
    load(rda_file)
    expect_equal(class(md_tvd_t_2), "data.frame")
    expect_equal(md_tvd_t_2, md_tvd_2)
})


test_that("MD-TVD table 3 matches", {
    md_tvd_txt_3 <- "
            MD      TVD
            0	         0
            248.7	   248.7
            497.4	   497.4
            746.2    746.2
            994.9	   994.9
            1243.6	1243.6
            1492.3	1492.3
            1741.0	1741.0
            1989.7	1989.7
            2238.5	2238.5
            2487.2	2487.2
            2735.9	2735.9
            2984.6	2984.6
            3233.3	3233.3
            3482.1	3482.1
            3730.8	3730.8
            3979.5	3979.5
            4228.2	4228.2
            4476.9	4476.9
            4725.6	4725.6
            4974.4	4974.4
            5223.1	5223.1
            5471.8	5471.8
            5720.5	5720.5
            5969.2	5969.2
            6217.9	6217.9
            6466.7	6466.7
            6715.4	6715.4
            6964.1	6964.1
            7212.8	7212.8
            7461.5	7461.5
            7710.3	7710.3
            7959.0	7959.0
            8207.7	8207.7
            8456.4	8456.4
            8705.1	8705.1
            8953.8	8953.8
            9202.6	9202.6
            9451.3	9451.3
            9700.0	9700.0
"
    md_tvd_t_3 <- set_deviation_survey(md_tvd_txt_3)
    load(rda_file)
    expect_equal(class(md_tvd_t_3), "data.frame")
    expect_equal(md_tvd_t_3, md_tvd_3)
})


