## ----rows.print=30-------------------------------------------------------
library(rNodal)
library(tibble)
# Example from Prosper oil well 01. Dry version

# roughness = 0.0006
input_example <-  setWellInput(field.name = "HAGBR.MOD", 
                               well.name = "Oilwell_01_Dry", 
                               depth.wh = 0, depth.bh = 9275, 
                               diam.in = 4.052, 
                               GLR = 800, liq.rt = 983, wcut = 0.0, 
                               thp = 100, tht = 60, bht = 210, 
                               API = 37, oil.visc = 5.0, 
                               gas.sg = 0.76, wat.sg = 1.07, if.tens = 30, 
                               salinity = 23000
                                 )

well_model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.00001)

as.tibble(runVLP(well.input = input_example, well_model))

## ------------------------------------------------------------------------
# in Prosper the angle is measured againt the vertical
library(rNodal)

md_tvd_01 <- "
MD      TVD 
0	     0	 
600	    600
1005	 1000
4075	 4000
7700	 7500
9275	 9000
"

md_tvd <- set_deviation_survey(md_tvd_01)
md_tvd
deviation_survey <- compute_angle_deviation_survey(md_tvd, reference = "vertical")
dataFrame <- deviation_survey
dataFrame

## ------------------------------------------------------------------------
# split deviated well in two ways: by and length.out
library(rNodal)

md <- deviation_survey[["MD"]]   # get MD vector

add_md_by <- rNodal:::split_well_in_deltas(md, by = 50)
add_md_by

## ------------------------------------------------------------------------
# split deviated well in two ways: by and length.out
library(rNodal)

md <- deviation_survey[["MD"]]   # get MD vector

add_md_lo <- rNodal:::split_well_in_deltas(md, length.out = 40)
add_md_lo


## ------------------------------------------------------------------------

rNodal:::build_survey_with_deltas(deviation_survey, add_md_by)
rNodal:::build_survey_with_deltas(deviation_survey, add_md_lo)

## ------------------------------------------------------------------------
# split the MD of the well in equal parts but a total of "n" segments
split <- seq.int(deviation_survey[1, "MD"], deviation_survey[nrow(deviation_survey), "MD"], 
                 length.out = 100)

# add the known MD values to the sequence. Now the length is little bit longer
md <- deviation_survey[["MD"]]
add_md <- sort(unique(c(md, split)))
add_md


# reconstruct MD v TVD but for the partitioned well in delta-x
df <- data.frame()     # new long dataframe
index <- 1             # index the small dataframe
tvd <- 0 
for (j in 1:length(add_md)) {  # iterate through the sequence
    row = dataFrame[index, ]   # get a row of the deviation survey
    # cat(index)
    df[j, "md"]  <- add_md[j]  # assign MD in sequence to md in long dataframe
    df[j, "seg"] <- index      # assign 
    if (j == 1)                 # if it is the first row
        df[j, "delta.md"] <- add_md[j]
    else
        df[j, "delta.md"] <- add_md[j] - df[j-1, "md"]
    
    df[j, "radians"] <- row[["radians"]]
    df[j, "degrees"] <- row[["degrees"]]
    df[j, "delta.tvd"] <- cos(row[["radians"]]) * df[j, "delta.md"] # calculate delta TVD
    tvd <- tvd + df[j, "delta.tvd"]        # add delta.tvd
    df[j, "tvd"] <- tvd                    # tvd column
    if (add_md[j] >= row[["MD"]]) {        # switch to next deviation branch
        index <- index + 1
    }
}
df

## ------------------------------------------------------------------------
# iterate through dataframe
 for (index in 1:nrow(dataFrame)) { 
     row = dataFrame[index, ] 
     # do stuff with the row 
     # print(row[["MD"]])
     cat(row[["MD"]], "\n")
 } 

## ------------------------------------------------------------------------
for (index in 1:nrow(dataFrame)) { 
    row = dataFrame[index, ] 
    # cat(row, "\n")
    for (j in add_md) {
        if (j <= row[["MD"]]) {
            cat(sprintf("%12f %12f \n", j, row[["MD"]]))
            # print(row[["MD"]][index] * sin(row[["radians"]][index]))
        }
    }
}

## ------------------------------------------------------------------------
# split the tubing in dx pieces
apply(deviation_survey, 1, function(x) x["MD"] 
)

