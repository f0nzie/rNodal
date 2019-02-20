library(dplyr)


#' Compute the angle of a deviated well and cumulative displacement
#'
#' @rdname WellDeviationSurvey-functions
#' @param well_table a well deviation survey
#' @param reference angle against the vertical or horizontal
#' @export
compute_angle_deviation_survey <- function(well_table,
                                           reference = c("vertical",
                                                         "horizontal")) {
    # create table with delta MD, TVD, radian, degrees
    # also includes the reference: vertical or horizontal
    # calculate delta MD
    if (!is.data.frame(well_table )) stop("WDS must be a dataframe")
    if (ncol(well_table) != 2) stop("WDS should have at least two columns")
    if (!all(colnames(well_table) %in% c("MD", "TVD")))
        stop("MD and TVD must exist in WDS")
    reference <- match.arg(reference)
    sh <- 1
    .md2 <- well_table[, "MD"]
    md2 <- shift(.md2, sh, default = 0)
    well_table[, "delta.md"] <- well_table[, "MD"] - md2

    # calculate delta TVD
    .tvd2 <- well_table[, "TVD"]
    tvd2 <- shift(.tvd2, sh, default = 0)
    well_table[, "delta.tvd"] <- well_table[, "TVD"] - tvd2

    # calculate angle depending of the reference
    switch(reference,
           vertical   = {arcFun <- acos; dispFun <- sin},
           horizontal = {arcFun <- asin; dispFun <- cos} )

    well_table[, "radians"] <- arcFun(well_table[, "delta.tvd"] / well_table[, "delta.md"])
    well_table[, "disp"]    <- well_table[, "delta.md"] * dispFun(well_table[, "radians"])
    well_table[, "disp"]    <- round(na.zero(well_table[, "disp"]), 3)
    well_table[, "cum_disp"] <- cumsum(well_table[, "disp"])
    # convert angle to degrees
    well_table[, "degrees"] <- well_table[, "radians"] * 180 / pi
    # fill NAs with zeroes
    well_table[, "radians"] <- na.zero(well_table[, "radians"])
    well_table[, "degrees"] <- na.zero(well_table[, "degrees"])
    well_table
}

#' Perform calculations to obtain the angles in a deviation survey
#' @param md_tvd_text the deviation survey as text
#' @param reference if the angle in the wellbore is measured against the vertical
#' or the horizontal
#' @return a dataframe with MD, TVD, angle, displacement
#'
#' @importFrom dplyr mutate select lag
#' @export
calc_deviation_survey <- function(md_tvd_text,
                                  reference = c("vertical",
                                                        "horizontal")) {
    # global variables check
    point <- NULL; TVD <- NULL; MD <- NULL; delta.md <- NULL; radians <- NULL;
    disp <- NULL; delta.tvd <- NULL

    epsilon <- 1e-8   # make global

    deviation_survey <- set_deviation_survey(md_tvd_text)
    stopifnot(class(deviation_survey)=="data.frame")

    switch(reference,
           vertical   = {arcFun <- acos; dispFun <- sin},
           horizontal = {arcFun <- asin; dispFun <- cos} )

    ang_deviation_survey <- deviation_survey %>%
        mutate(point = as.integer(row_number() - 1)) %>%
        select(point, everything()) %>%
        mutate(tvd = TVD) %>%
        mutate(md = MD) %>%
        mutate(delta.md = MD - lag(MD, default = 0)) %>%
        mutate(delta.tvd = TVD - lag(TVD, default = 0)) %>%
        mutate(radians = ifelse(delta.md==0, 0, arcFun(delta.tvd / delta.md))) %>%
        mutate(disp = ifelse(dispFun(radians) <= epsilon, # if sin or cos of the angle
                             0,                       # is zero or very near zero
                             delta.md * dispFun(radians))) %>%
        mutate(cum_disp = cumsum(disp))

    ang_deviation_survey
}


#' Build an iteration table adding new points to the original deviation survey
#'
#' @param ang_deviation_survey the deviation survey including angles. dataframe
#' @param geotherm_df geothermal data as dataframe
#' @param depth_points number of rows of depth points in the table
#' @param step_size the size of the delta L
#' @export
#' @importFrom dplyr mutate select full_join right_join row_number everything pull
build_iteration_table <- function(ang_deviation_survey, geotherm_df,
                                  depth_points=NULL, step_size=NULL) {
    # global variables check
    . <-  NULL
    TVD <- NULL; tvd <- NULL; point <- NULL; radians <- NULL; geo_grad <- NULL;
    dTVD <- NULL; dtemp <- NULL; temp <- NULL; given <- NULL; delta.tvd <- NULL;
    delta.md <- NULL; new_point <- NULL; md <- NULL; MD <- NULL; disp <- NULL

    epsilon <- 1e-8

    # number_segments <- depth_points

    # TODO: validate columns in ang_deviation_survey. It must contain angles

    # if depth_points < num_rows deviation_survey, only add geotherm temp

    # TODO: validate the name of the columns of geotherm_df
    ang_deviation_survey <- full_join(ang_deviation_survey, geotherm_df)
    print(ang_deviation_survey)

    # add extra rows for calculations
    # the original depths are included in the table as part of the total number of rows
    ang_deviation_survey %>%
        select(TVD) %>%      # select TVD
        pull(TVD) %>%
        # {. -> tmp } %>%
        { tmp <- .                                  # assign to tmp
        mn <- min(tmp); mx <- max(tmp)          # create two objects min, max
        {
            if (is.null(step_size)) seq(mn, mx, length.out = depth_points)
            else if (is.null(depth_points)) seq(mn, mx, by = step_size)
            else stop() } %>%
            c(., tmp) %>%
            unique %>%
            sort()
        } %>%
        data.frame(tvd = .) %>%                  # convert vector to dataframe
        mutate(new_point = 99+row_number()) %>%  # add a column
        print() -> grown                         # create dataframe grown

    # join the two tables (small original and grown with more rows) by TVD
    # table with lots of NAs
    right_join(ang_deviation_survey, grown, by = "tvd") %>%
        # mutate(delta.tvd = TVD - lag(TVD, default = 0))  %>%
        mutate(delta.tvd = tvd - lag(tvd, default = 0))  %>%
        print() -> joined

    # get MD, delta.md given TVD and angle at new intervals
    # & nrow(geotherm_df) < nrow(ang_deviation_survey)
    iteration_table <- joined %>%
        mutate(given = ifelse(is.na(TVD), FALSE, TRUE)) %>%
        mutate(point = replace_na_with_last(point)) %>%
        mutate(radians = replace_na_from_bottom(radians)) %>%
        mutate(geo_grad = replace_na_from_bottom(geo_grad)) %>%
        mutate(dTVD = replace_na_from_bottom(dTVD)) %>%
        mutate(dtemp = replace_na_from_bottom(dtemp)) %>%
        mutate(temp = replace_na_with_last(temp)) %>%
        mutate(temp = dtemp + geo_grad * (tvd+dTVD) ) %>% # temp[[1]]: temperature at the top
        # if the TVD was given, do not calculate it, keep the original
        mutate(temp = ifelse(!given, dtemp + geo_grad * (tvd+dTVD), temp)) %>%
        mutate(delta.md = ifelse(cos(radians) <= epsilon,
                                 delta.tvd,
                                 delta.tvd / cos(radians))) %>%
        mutate(md = cumsum(delta.md)) %>%
        mutate(disp = delta.md * sin(radians)) %>%
        mutate(cum_disp = cumsum(disp)) %>%
        # mutate(temp = ifelse(is.na(temp), temp[[1]] + geo_grad * tvd, temp)) %>%
        select(new_point, tvd, md, radians, temp, geo_grad, given, TVD, MD,
               everything())

    iteration_table
}






replace_na_with_last<-function(x, a=!is.na(x)){
    x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

# replaces NAs in a vector by using a value starting at the bottom
replace_na_from_bottom<-function(x, a=!is.na(x)){
    x <- rev(x)
    rev(x[which(a)[c(1,1:sum(a))][cumsum(a)+1]])
}
