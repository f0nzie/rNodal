# create new environment for project
project.env <- new.env(parent = emptyenv())

setProjectEnvironment <- function() {
    data_file = "default"            # name for storage file

    pkg_root <- system.file(package = "rNodal")
    project.env[["pkg.root"]]    <- pkg_root
    project.env[["pkg.extdata"]] <- paste(pkg_root, "inst","extdata", sep = "/")
    project.env[["pkg.data"]]    <- paste(pkg_root, "data", sep = "/")

    extdata_folder <- project.env[["pkg.extdata"]]

    # full file path and extensions for data file
    datafile.rda  <- file.path(extdata_folder, paste(data_file, "rda", sep = "."))

    project.env[["data.folder"]]     <- extdata_folder
    project.env[["datafile.rda"]]   <- datafile.rda     # RDA file

    if (!is_checking_package()) {
        # it is a user project folder. Not in building package mode
        if (is_saved_session()) {
            name_of_the_file <- load(getSessionFilename())
        } else {
            cat("A former session file not found\n")
        }
    }
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

