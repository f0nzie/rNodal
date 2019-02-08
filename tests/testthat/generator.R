
create_test_tables <- function() {
    proj_root <- rprojroot::is_rstudio_project$find_file()
    rda_file <- file.path(proj_root, "tests", "testthat", "md_tvd.rda")

    # table 1
    md_tvd_txt_1 <- "
                MD      TVD
                0	     0
                600	    600
                1005	 1000
                4075	 4000
                7700	 7500
                9275	 9000
                "
    md_tvd_1 <- set_deviation_survey(md_tvd_txt_1)
    rNodal:::append_to_rdata(md_tvd_1, file = rda_file)


    # table 2
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
    md_tvd_2 <- set_deviation_survey(md_tvd_txt_2)
    rNodal:::append_to_rdata(md_tvd_2, file = rda_file)


    # table 3
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
    md_tvd_3 <- set_deviation_survey(md_tvd_txt_3)
    rNodal:::append_to_rdata(md_tvd_3, file = rda_file)
}
