
create_test_tables <- function() {
    proj_root <- rprojroot::is_rstudio_project$find_file()


    # --------------------------------------------------------------------------
    # MD vs TVD tables
    test_rda <- "md_tvd.rda"
    rda_file <- file.path(proj_root, "tests", "testthat", test_rda)

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


    # -------------------------------------------------------------------------
    #
    # basic calculations
    test_rda <- "basic_calcs.rda"
    rda_file <- file.path(proj_root, "tests", "testthat", test_rda)

    # Brown_C13
    geothermal_2p_txt <- c("
      TVD    temp
      0      120
      2670   150
    ")
    deviation_survey_2p_txt <- "
       TVD    MD
         0     0
      2670  2670
    "
    well_input <- setWellInput(
      field.name = "HAGBR.MOD",
      well.name = "Brown_C13",
      depth.wh = 0, depth.bh = 2670,
      diam.in = 1.995,
      GLR = 500,
      liq.rt = 1000,
      wcut = 0.6,
      thp = 500,
      tht = 120,
      bht = 150,
      API = 22,
      gas.sg = 0.65,
      wat.sg = 1.07,
      if.tens = 30,
      geotherm = geothermal_2p_txt,
      dev_survey = deviation_survey_2p_txt
      )
    basic_calcs_c13 <- getBasicCalcs(well_input)
    rNodal:::append_to_rdata(basic_calcs_c13, file = rda_file)


    # Guo_P44
    geothermal_2p_txt <- c("
    TVD    temp
    0       80
    9700   180
    ")
    deviation_survey_2p_txt <- "
       TVD    MD
         0     0
      9700  9700
    "
    well_input <- setWellInput(
        field.name = "HAGBR.MOD",
        well.name = "Guo_P44",
        depth.wh = 0, depth.bh = 9700, diam.in = 1.995,
        GLR = 362.7, liq.rt = 758, wcut = 0.1,
        thp = 100, tht = 80, bht = 180,
        API = 40, gas.sg = 0.70, wat.sg = 1.05,
        if.tens = 30,
        U = 4,
        geotherm = geothermal_2p_txt,
        dev_survey = deviation_survey_2p_txt
        )

    basic_calcs_ep44 <- getBasicCalcs(well_input)
    rNodal:::append_to_rdata(basic_calcs_ep44, file = rda_file)


    # C44
    geothermal_2p_txt <- "
    TVD   temp
    0     120
    3590  150
    "
    deviation_survey_2p_txt <- "
    TVD   MD
    0     0
    3590  3590
    "
    well_input <- setWellInput(
        field.name = "HAGBR.MOD",
        well.name = "Brown_C44",
        depth.wh = 0, depth.bh = 3590,
        diam.in = 1.995,
        GLR = 1000, liq.rt = 600, wcut = 0.0,
        thp = 500, tht = 120, bht = 150,
        API = 42, oil.visc = 1.0,
        gas.sg = 0.65, wat.sg = 1.07, if.tens = 30,
        U = 8,
        geotherm = geothermal_2p_txt,
        dev_survey = deviation_survey_2p_txt
    )

    basic_calcs_c44 <- getBasicCalcs(well_input)
    rNodal:::append_to_rdata(basic_calcs_c44, file = rda_file)


    # T01_Oil_Well
    geothermal_3p_txt <- c("
    TVD    temp
    0      60
    600    40
    9000   210
    ")
    deviation_survey_6p_txt <- "
    MD      TVD
    0	     0
    600	    600
    1005	 1000
    4075	 4000
    7700	 7500
    9275	 9000
    "
    well_input <-  setWellInput(field.name = "HAGBR.MOD",
                                well.name = "T01_Oil_Well",
                                depth.wh = 0,
                                depth.bh = 9275,
                                diam.in = 4.052,
                                GLR = 800, liq.rt = 983, wcut = 0.0,
                                thp = 100,
                                tht = 60,
                                bht = 210,
                                API = 37,
                                oil.visc = 5.0,
                                gas.sg = 0.76,
                                wat.sg = 1.07,
                                if.tens = 30,
                                salinity = 23000,
                                U = 8,
                                dev_survey = deviation_survey_6p_txt,
                                geotherm = geothermal_3p_txt
    )

    basic_calcs_t01_ow <- getBasicCalcs(well_input)
    rNodal:::append_to_rdata(basic_calcs_t01_ow, file = rda_file)

    # basic calculations - end



    # -------------------------------------------------------------------------
    # runVLP() test tables
    #
    test_rda <- "vlp_run_output.rda"
    rda_file <- file.path(proj_root, "tests", "testthat", test_rda)

    # Brown_C13
    geothermal_data <- c("
      TVD   temp
      0     120
      2670  150
    ")
    deviation_survey <- c("
      MD    TVD
      0     0
      2670  2670
    ")
    well_input_c13 <- setWellInput(
      field.name = "HAGBR.MOD",
      well.name = "Brown_C13",
      depth.wh = 0, depth.bh = 2670,
      diam.in = 1.995,
      GLR = 500,
      liq.rt = 1000,
      wcut = 0.6,
      thp = 500,
      tht = 120,
      bht = 150,
      API = 22,
      gas.sg = 0.65,
      wat.sg = 1.07,
      if.tens = 30,
      geotherm = geothermal_2p_txt,
      dev_survey = deviation_survey_2p_txt
      )

    well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                              segments = 11,
                              tol = 0.000001,
                              well_input = well_input_c13)

    output_c13 <- runVLP(well.input = well_input_c13, well_model)
    rNodal:::append_to_rdata(output_c13, file = rda_file)


    # Guo_P44
    geothermal_2p_txt <- c("
    TVD    temp
    0       80
    9700   180
    ")

    deviation_survey_2p_txt <- "
       TVD    MD
         0     0
      9700  9700
    "
    well_input_p44 <- setWellInput(
      field.name = "HAGBR.MOD",
      well.name = "Guo_P44",
      depth.wh = 0, depth.bh = 9700, diam.in = 1.995,
      GLR = 362.7, liq.rt = 758, wcut = 0.1,
      thp = 100, tht = 80, bht = 180,
      API = 40, gas.sg = 0.70, wat.sg = 1.05,
      if.tens = 30,
      U = 4,
      geotherm = geothermal_2p_txt,
      dev_survey = deviation_survey_2p_txt
      )

    well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                              segments = 29,
                              tol = 0.000001,
                              well_input = well_input_p44)

    output_p44 <- runVLP(well.input = well_input_p44, well_model)
    rNodal:::append_to_rdata(output_p44, file = rda_file)


    # Brown_C44
    geothermal_2p_txt <- "
    TVD   temp
    0     120
    3590  150
    "
    deviation_survey_2p_txt <- "
    TVD   MD
    0     0
    3590  3590
    "
    well_input_c44 <- setWellInput(
      field.name = "HAGBR.MOD",
      well.name = "Brown_C44",
      depth.wh = 0, depth.bh = 3590,
      diam.in = 1.995,
      GLR = 1000, liq.rt = 600, wcut = 0.0,
      thp = 500, tht = 120, bht = 150,
      API = 42, oil.visc = 1.0,
      gas.sg = 0.65, wat.sg = 1.07, if.tens = 30,
      U = 8,
      geotherm = geothermal_2p_txt,
      dev_survey = deviation_survey_2p_txt
    )

    well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                              segments = 15,
                              tol = 0.00001,
                              well_input = well_input_c44
                              )

    output_c44 <- runVLP(well.input = well_input_c44, well_model)
    rNodal:::append_to_rdata(output_c44, file = rda_file)


    # this well taking a long time to save
    # # T01_Oil_Well
    # geothermal_3p_txt <- c("
    # TVD    temp
    # 0      60
    # 600    40
    # 9000   210
    # ")
    # deviation_survey_6p_txt <- "
    # MD      TVD
    # 0	     0
    # 600	    600
    # 1005	 1000
    # 4075	 4000
    # 7700	 7500
    # 9275	 9000
    # "
    # well_input_t01_ow <-  setWellInput(field.name = "HAGBR.MOD",
    #                             well.name = "T01_Oil_Well",
    #                             depth.wh = 0,
    #                             depth.bh = 9275,
    #                             diam.in = 4.052,
    #                             GLR = 800, liq.rt = 983, wcut = 0.0,
    #                             thp = 100,
    #                             tht = 60,
    #                             bht = 210,
    #                             API = 37,
    #                             oil.visc = 5.0,
    #                             gas.sg = 0.76,
    #                             wat.sg = 1.07,
    #                             if.tens = 30,
    #                             salinity = 23000,
    #                             U = 8,
    #                             dev_survey = deviation_survey_6p_txt,
    #                             geotherm = geothermal_3p_txt
    # )
    #
    # well_model <- setVLPmodel(vlp.model = "hagbr.mod",
    #                           segments = 29,
    #                           tol = 0.00001,
    #                           well_input = well_input_t01_ow
    #                           )
    #
    # output_t01_ow <- runVLP(well.input = well_input_t01_ow, well_model)
    # rNodal:::append_to_rdata(output_ow_dry, file = rda_file)


    # ========================================================================
    #
    # test data for:
    # test_geomethermal_gradient.R
    #
    # expected data RDA file
    test_rda <- "expected_geothermal.rda"
    rda_file <- file.path(proj_root, "tests", "testthat", test_rda)


    # well C13
    geothermal_txt <- c("
      TVD    temp
      0      120
      2670   150
    ")
    geothermal_c13 <- rNodal:::as_dataframe_geothermal_data(geothermal_txt)
    rNodal:::append_to_rdata(geothermal_c13, file = rda_file)


    # well C44
    geothermal_txt <- "
    TVD   temp
    0     120
    3590  150
    "
    geothermal_c44 <- rNodal:::as_dataframe_geothermal_data(geothermal_txt)
    rNodal:::append_to_rdata(geothermal_c44, file = rda_file)


    # well P44
    geothermal_txt <- c("
    TVD    temp
    0       80
    9700   180
    ")
    geothermal_p44 <- rNodal:::as_dataframe_geothermal_data(geothermal_txt)
    rNodal:::append_to_rdata(geothermal_p44, file = rda_file)


    # well T01_OW
    geothermal_txt <- c("
    TVD    temp
    0      60
    600    40
    9000   210
    ")
    geothermal_t01_ow <- rNodal:::as_dataframe_geothermal_data(geothermal_txt)
    rNodal:::append_to_rdata(geothermal_t01_ow, file = rda_file)


}
