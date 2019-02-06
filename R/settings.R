# create a project environment
project.env <- new.env(parent = emptyenv()) # create new environment for project

setProjectEnvironment <- function() {
    data_file = "default"            # name for storage file

    project.env[["pkg.root"]]    <- pkg_root <- system.file("..",
                                                            package = "rNodal")
    project.env[["pkg.extdata"]] <- pkg_extdata <- paste(
        pkg_root, "inst","extdata", sep = "/")
    project.env[["pkg.data"]]    <- pkg_data <- paste(pkg_root, "data", sep = "/")

    data_folder <- pkg_extdata

    # full file path and extensions for data file
    datafile.rda  <- file.path(data_folder, paste(data_file, "rda", sep = "."))

    project.env[["data.folder"]]     <- data_folder
    project.env[["data.file.rda"]]   <- datafile.rda     # RDA file



    if (!is_checking_package()) {
        # it is a user project folder. Not in building package mode
        if (is_saved_session()) {
            name_of_the_file <- load(getSessionFilename())
        } else {
            # cat("Save session not found\n")
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

