




#' Check if a HDF5 specific group exists already in the file
#'
#' @param    hFile    a full path to a HDF5 file
#' @param    hGroup   any group
#' @return   TRUE if group already exists
#'           FALSE if group has not been created yet
#' @export
hGroup.exists <- function(hFile, hGroup) {
    # we have to add the `/` to the group, otherwise the search of the group is ignored
    hGroup <- ifelse(!grepl("^/", hGroup),    # if hGroup does not start with `/`
                     paste0("/", hGroup),     # then add the `/` to the beginning
                     hGroup)                  # otherwise, just leave it alone

    groups <- get.group.paths(hFile)          # get a vector of group paths

    if (hGroup %in% groups)                   # if the group is in return TRUE
        return(TRUE)
    else
        return(FALSE)
}



#' Add a record to HDF5 file of well inputs and outputs
#'
#' @param    hFile      HDF5 file, full path
#' @param    fieldName  the field name
#' @param    wellName   the well name
#' @return   a string with /field/well/datetime if creation successful
#'           otherwise, NULL
# #' @importFrom rhdf5 h5ls h5createGroup H5close H5Fcreate
#' @import rhdf5
#' @include hdf5.R settings.R
#' @export
get.well.slot <- function(hFile, fieldName, wellName) {
    fid <- NULL
    stopifnot(!is.null(hFile))
    if (!dir.exists(dirname(hFile))) {
        setHDF5DumpFile()
        hFile <- getHDF5DumpFile()
        warning("\n ./data folder does not exist. Creating a temporary file\n")
        warning("Creating a local ./data folder is advised\n")
    }
    # stopifnot(file.exists(hFile))
    saveToProjectEnv("data.file.hdf5", hFile)       # save hdf5 to prj-env

    # check if hFile exists otherwise create a new hdf5 file
    if (!file.exists(hFile))
        fid <- H5Fcreate(hFile)

    .fieldName <- paste0("/", fieldName)                  # add / to fieldName
    field.well <- paste(.fieldName, wellName, sep = "/")  # build the /field/well path

    group.paths <- get.group.paths(hFile)                 # get a vector of unique group paths

    # check if /field/well group exists
    if (!field.well %in% group.paths) {
        cat(hFile, fid, "\n")
        cat("\nGroup: ", field.well, " DOES NOT exist \n")  # if it doesn't, say it
        if (!hGroup.exists(hFile, fieldName)) {             # if /field does not exist
            h5createGroup(hFile, fieldName)                 # create /field
            cat("Group: ", fieldName, "just created\n")
        } else {                                            # otherwise say already exists
            cat("\t but group ", fieldName, " already exists\n")
        }
        # now that we know that /field group is ready we can add  a /well to it
        h5createGroup(hFile, field.well)                    # if /field exists, then
        cat("\t", wellName, "well just added")              # we create /field/well group
    } else  {
        # /field/well group already exists
        cat("\n Group", field.well," already exists. Ready to add observation ...") # just say it
    }
    # add datetime group to /field/well/
    stamp <- format(Sys.time(), "%Y%m%d.%H%M%S")            # get datetime up to seconds
    wellObservation <- paste(field.well, stamp, sep = "/")  # join /field/well with /datetime
    hG <- h5createGroup(hFile, wellObservation)             # add  /datetime  /field/well
    H5close()                                               # close file
    return(ifelse(hG, wellObservation, NULL))               # return /field/well/datetime if hG TRUE
}




#' Get a vector of unique groups in HDF5 file
#'
#' @param   hFile    a HDF5 file to inspect
#' @return  a vector with unique full path groups
#' @importFrom rhdf5 h5ls H5close
#' @importFrom dplyr select filter %>% mutate distinct
#' @export
get.group.paths <- function(hFile) {
    # declare variable names of HDF5 dataset before being read
    dclass <- otype <- group <- name <- paths <- casePaths <- NULL
    if (!file.exists(hFile)) stop("\nHDF5 file does not exist. Provide a valid file name")

    df <- h5ls(hFile, recursive = 3)              # create a dataframe of three levels down

    df2 <- df %>%
        filter(otype == "H5I_GROUP") %>%           # get only group items
        select(-c(dclass, dim)) %>%                # exclude columns
        mutate(paths = ifelse(nchar(group) == 1,   # do not add / if group is root
                              paste0(group, name), #
                              paste(group, name, sep = "/"))) %>%     # join group and name
        mutate(casePaths = gsub("(.*)/[0-9]{4}.*",   # after the slash reject all digits bu {4-1}
                                "\\1", paths)) %>%
        select(-c(otype, name)) %>%                 # exclude columns
        distinct(casePaths, .keep_all = TRUE)       # only unique groups for all variables

    H5close()                                       # close HDF5
    return(df2$casePaths)                           # get column vector of filtered dataframe

}


#' Get well observations. dataframe and count per /field/well
#' @param hFile name of the HDF5 file
#' @param fieldName field name
#' @param wellName well name
#' @importFrom rhdf5 h5ls
#' @importFrom dplyr select filter %>%
#' @export
get.well.cases <- function(hFile, fieldName, wellName) {
    group <- name <- NULL                         # variables from HDF5 dataset
    df <- h5ls(hFile, recursive = 3)

    .fieldName <- paste0("/", fieldName)                  # add / to fieldName
    field.well <- paste(.fieldName, wellName, sep = "/")  # build the /field/well path

    df2 <- df %>%
        select(group, name) %>%
        filter(group == field.well)

    out <- list(df = df2,
                count = nrow(df2)
                )
    return(df2)

}






