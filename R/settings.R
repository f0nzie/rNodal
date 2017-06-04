# create a project environment
project.env <- new.env(parent = emptyenv()) # create new environment for project

setProjectEnvironment <- function() {
    data_file = "default"            # name for the HDF5 file

    project.env[["pkg.root"]]    <- pkg_root <- system.file("..", package = "rNodal")
    project.env[["pkg.extdata"]] <- pkg_extdata <- paste(pkg_root, "inst","extdata", sep = "/")
    project.env[["pkg.data"]]    <- pkg_data <- paste(pkg_root, "data", sep = "/")

    data_folder <- pkg_extdata

    # full file path and extensions for data file
    datafile.h5   <- file.path(data_folder, paste(data_file, "h5", sep = "."))
    datafile.hdf5 <- file.path(data_folder, paste(data_file, "hdf5", sep = "."))
    datafile.rda  <- file.path(data_folder, paste(data_file, "rda", sep = "."))

    project.env[["data.folder"]]     <- data_folder
    project.env[["data.file"]]       <- data_file
    project.env[["data.file.hdf5"]]  <- datafile.hdf5    # HDF5 file
    project.env[["data.file.rda"]]   <- datafile.rda     # RDA file

    current_dir <- getwd()
    print(current_dir)

    # rstudio <- rstudioapi::getActiveDocumentContext()

    # crit_r_pkg    <- rprojroot::is_r_package
    # cat(isRpackage(), "\n")
    # crit_r_studio <- rprojroot::is_rstudio_project

    # this causing error during build
    # if (isSavedSession()) {
    #     cat("saved session")
    #     load(getSessionFilename())
    #     project.env[["data.file.hdf5"]] <- hdf5_file
    # }

    # out <- tryCatch(
    #     {
    #         message("this is the try part")
    #         rprojroot::find_rstudio_root_file(path = "..")
    #     },
    #     error = function(e) {
    #         message(e)
    #         return(NA)
    #     },
    #     warning = function(w) {
    #         message(w)
    #         return(NULL)
    #     },
    #     finally = {
    #         message("finally")
    #     }
    # )


}


isRpackage <- function() {
    # crit_r_pkg    <- rprojroot::is_r_package
    crit_r_pkg <- rprojroot::as.root_criterion("NAMESPACE")
    # expected <-  "contains a file `DESCRIPTION` with contents matching `^Package: `"
    expected <- "contains a file `NAMESPACE`"
    ifelse(crit_r_pkg$desc == expected, TRUE, FALSE)
}


#' Save a variable to the project environment
#' @param var variable to save in environment
#' @param value what is to be saved
#' @keywords internal
saveToProjectEnv <- function(var, value) {
    project.env[[var]] <- value
}

#' Read variable from environment
#' @param var variable to read
#' @keywords internal
readFromProjectEnv <- function(var) {
    project.env[[var]]
}


changeHdf5DefaultExtension <- function(new_extension) {
    # change the extension from hdf5 to h5 or viceversa
        # read current extension
        # apply new_extension and save it to project environment
}
