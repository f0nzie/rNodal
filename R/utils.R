get_extdataDir <- function(variables) {
    system.file("extdata", package = "rNodal")
}


# this causing error during build
isSavedSession <- function(session_file = "session.rda") {
    session_dir <- getProjectDir()
    .session_file <- paste(session_dir, session_file, sep = "/")
    ifelse(file.exists(.session_file), TRUE, FALSE)
}

getSessionFilename <- function(session_file = "session.rda") {
    stopifnot(isSavedSession(session_file))
    paste(getProjectDir(), session_file, sep = "/")
}

#' Save the user HDF5 file to a persistant file
#' @keywords internal
saveSession <- function() {
    hdf5_file <- readFromProjectEnv("data.file.hdf5")
    save(hdf5_file, file = "session.rda")

}

# this was causing an error during the build
getProjectDir <- function() {
    # get the project folder
    out <- tryCatch(
        {
            message("this is the try part")
            rprojroot::find_rstudio_root_file()  # points to project dir
        },
        error = function(cond) {
            message("No root dir found by rprojroot")
            message(cond)
            return(get_extdataDir())
        },
        warning = function(cond) {
            message("rprojroot caused a warning")
            message("here is the original warning msg")
            message(cond)
            return(get_extdataDir())
        },
        finally = {
            message("final message")
        }
    )
    return(out)
}


#' What is the status of the package
#'
#' @export
nodal_status <- function() {
    readFromProjectEnv("data.file.hdf5")
}


#' Copy a HDF5 data container to user's folder
#'
#' It will check if a HDF5 file already exist at the destination.
#' If the file exist it will throw a warning.
#' To overwrite it with a fresh copy use overwrite=TRUE
#'
#' @param overwrite it is FALSE to prevent overwriting an existing data file
copyDataContainer <- function(overwrite = FALSE) {
    hdf5_filename <- "default.hdf5"
    source_dir  <- system.file("extdata", package = "rNodal")
    source_file <- paste(source_dir, hdf5_filename, sep = "/")
    stopifnot(file.exists(source_file))

    target_dir  <- getProjectDir()
    target_file <- paste(target_dir, hdf5_filename, sep = "/")
    if (file.exists(target_file) && overwrite == FALSE)
        warning("HDF5 data container already exists.\n Use overwrite=TRUE")

    if (file.copy(from = source_file, to = target_dir, overwrite = overwrite)) {
        saveToProjectEnv("data.file.hdf5", target_file)
        saveSession()
    }
    else
        warning("File copy operation failed")
}


#' Logical response to presence of HDF5 files anywhere under user root folder
#' @keywords internal
isHdf5Files <- function(where = "local") {
    stopifnot(where == "local" || where == "package")
    ifelse(length(listAllHdf5(where)) > 0, TRUE, FALSE)
}


#' List all HDF5 files
#' @keywords internal
listAllHdf5 <- function(where = "local") {
    if (where == "local")
        root_folder <- getProjectDir()
    else if (where == "package")
        root_folder <- get_extdataDir()
    stopif(nchar(root_folder) == 0)
    list.files(path = root_folder, pattern = "*.h5$|*.hdf5$",
               all.files = FALSE, full.names = TRUE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
}


#' File info for all HDF5 files
#' @param where "local" for project. The other option is "package"
#' @keywords internal
fileInfoHdf5 <- function(where = "local") {
    stopifnot(where == "local" || where == "package")
    file.info(listAllHdf5(where))
}


#' Find the bigger HDF5 file
#'
#' @param where "local" for project. The other option is "package"
#' @keywords internal
biggerHdf5 <- function(where = "local") {
    # find which is the bigger hdf5 file to use that one
    stopifnot(where == "local" || where == "package")
    df <- fileInfoHdf5(where)
    row.names(df[which(max(df$size) == df$size),])
}




#' Ensure that R expressions are false in unit tests
#'
#' @param ...
#' Any number of (logical) R expressions,
#' which should evaluate to \code{TRUE}.
#' @examples
#' \dontrun{
#' stopif(is.empty(c(2,1)), 4 < 3) # all FALSE
#' stopif(is.empty(numeric(0)))
#' }
#' @keywords internal
stopif <- function(...)
    {
        n <- length(ll <- list(...))
        if (n == 0L)
            return(invisible())
        mc <- match.call()
        for (i in 1L:n) {
            if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && all(!r))) {
                ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
                if (length(ch) > 1L)
                    ch <- paste(ch[1L], "....")
                stop(sprintf(ngettext(length(r), "%s is TRUE", "%s are not all FALSE"), ch),
                     call. = FALSE, domain = NA)
            }
        }
        invisible()
    }


#' Take objects and create a list using their names
#'
#' This avoids retyping the name of the object as in a list
#'
#' @param ... any additional parameters
#' @importFrom stats setNames
#' @keywords internal
named.list <- function(...) {
    nl <- setNames( list(...) , as.character( match.call()[-1]) )
    # nl <- setNames( list(...) , as.character( match.call()[-1]) )
    nl
}
