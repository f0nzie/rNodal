# #' @importFrom S4Vectors isSingleString
# #' @importFrom rhdf5 h5write h5ls

.check_HDF5_dump_file <- function(file)
{
    if (!S4Vectors::isSingleString(file))
        stop(S4Vectors::wmsg("'file' must be a single string specifying the path ",
                             "to an existing HDF5 file or to a new file"))
    if (file.exists(file))
        return(h5ls(file))
    rhdf5::h5createFile(file)    # will create a file with the provided name
    return(NULL)
}

.check_HDF5_dump_name <- function(name)
{
    if (!S4Vectors::isSingleString(name))
        stop(S4Vectors::wmsg("'name' must be a single string specifying the name ",
                             "of the HDF5 dataset to write"))
    if (name == "")
        stop(S4Vectors::wmsg("'name' cannot be the empty string"))
}

HDF5_dump_settings_envir <- new.env(parent=emptyenv())

### Called by .onLoad() hook (see zzz.R file).
setHDF5DumpFile <- function(file=paste0(tempfile(), ".h5"))
{
    file_content <- .check_HDF5_dump_file(file)
    assign("file", file, envir=HDF5_dump_settings_envir)
    if (is.null(file_content))
        return(invisible(file_content))
    return(file_content)
}

#' Get dump HDF5 file
#' @export
getHDF5DumpFile <- function()
    get("file", envir=HDF5_dump_settings_envir)

### A convenience wrapper.
lsHDF5DumpFile <- function() h5ls(getHDF5DumpFile())

assign("auto_inc_ID", 0L, envir=HDF5_dump_settings_envir)

.get_auto_inc_ID <- function()
{
    get("auto_inc_ID", envir=HDF5_dump_settings_envir)
}

.set_HDF5_dump_name_to_next_auto_inc_ID <- function()
{
    suppressWarnings(rm(list="name", envir=HDF5_dump_settings_envir))
    auto_inc_ID <- .get_auto_inc_ID() + 1L
    assign("auto_inc_ID", auto_inc_ID, envir=HDF5_dump_settings_envir)
}

setHDF5DumpName <- function(name)
{
    if (missing(name))
        return(.set_HDF5_dump_name_to_next_auto_inc_ID())
    .check_HDF5_dump_name(name)
    assign("name", name, envir=HDF5_dump_settings_envir)
}

#' Get the temp name for HDF5 file
#' @export
#' @importFrom methods is
getHDF5DumpName <- function()
{
    name <- try(get("name", envir=HDF5_dump_settings_envir), silent=TRUE)
    if (is(name, "try-error")) {
        auto_inc_ID <- .get_auto_inc_ID()
        name <- sprintf("/HDF5ArrayAUTO%05d", auto_inc_ID)
    }
    name
}

