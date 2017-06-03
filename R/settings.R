# create a project environment
project.env <- new.env(parent = emptyenv()) # create new environment for project

setProjectEnvironment <- function() {
    data.file = "default"

    # project.env[["project.root"]] <- proj_root <- rprojroot::find_rstudio_root_file()
    project.env[["project.root"]] <- proj_root <- system.file("..", package = "rNodal")
    project.env[["project.extdata"]] <- proj_extdata <- paste(proj_root, "inst","extdata", sep = "/")
    project.env[["project.data"]] <- proj_data <- paste(proj_root, "data", sep = "/")

    data.folder <- ReadFromProjectEnv("project.data")
    data.folder <- proj_extdata

    # file extensions for data file
    datafile.h5 <- file.path(data.folder, paste(data.file, "h5", sep = "."))
    datafile.hdf5 <- file.path(data.folder, paste(data.file, "hdf5", sep = "."))
    datafile.rda  <- file.path(data.folder, paste(data.file, "rda", sep = "."))

    project.env[["data.folder"]] <- data.folder
    project.env[["data.file"]]   <- data.file
    project.env[["data.file.hdf5"]]  <- datafile.hdf5   # HDF5 file
    project.env[["data.file.rda"]]   <- datafile.rda     # RDA file
}

#' Save a variable to the project environment
#' @param var variable to save in environment
#' @param value what is to be saved
#' @export
saveToProjectEnv <- function(var, value) {
    project.env[[var]] <- value
}

#' Read variable from environment
#' @param var variable to read
#' @export
ReadFromProjectEnv <- function(var) {
    project.env[[var]]
}
