#' @include settings.R
.onLoad <- function(libname, pkgname)
{
    # setHDF5DumpFile()
    # setHDF5DumpName()
    # print("loading ...")

    setProjectEnvironment()

    # # code to copy a h5 file to a user folder
    # theFolder <- "extdata"
    # .source <- system.file(package = "rNodal", "extdata", "default.hdf5")
    # .target <- paste(theFolder, "default.hdf5", sep = "/")
    #
    # if (!dir.exists(theFolder)) dir.create(theFolder)
    # if (!file.exists(.target)) file.copy(from = .source, to = .target)
}



