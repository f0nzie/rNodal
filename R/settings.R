# create a project environment
project.env <- new.env(parent = emptyenv()) # create new environment for project

setProjectEnvironment <- function(data.folder = "./data",
                                  data.file = "default") {
    # file extensions for data file
    datafile.hdf5 <- file.path(data.folder, paste(data.file, "hdf5", sep = "."))
    datafile.rda  <- file.path(data.folder, paste(data.file, "rda", sep = "."))

    project.env[["data.folder"]] <- data.folder
    project.env[["data.file"]]   <- data.file
    project.env[["data.file.hdf5"]]  <- datafile.hdf5   # HDF5 file
    # project.env[["data.file.hdf5"]]  <- getHDF5DumpFile
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
