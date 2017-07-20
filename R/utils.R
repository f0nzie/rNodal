# function required to move a row up or down
shift <- function(x, n, invert=FALSE, default=NA){
    stopifnot(length(x)>=n)
    if(n==0){
        return(x)
    }
    n <- ifelse(invert, n*(-1), n)
    if(n<0){
        n <- abs(n)
        forward=FALSE
    }else{
        forward=TRUE
    }
    if(forward){
        return(c(rep(default, n), x[seq_len(length(x)-n)]))
    }
    if(!forward){
        return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
    }
}


# function to fill NAs with zeroes
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}



is_checking_package <- function() {
    # works in check() and test_rNodal

    # go back two directories up if running a devtools::check()
    prj_dir <- R.utils::getParent(R.utils::getParent(getwd()))
    # from check:
    #      "C:/Users/msfz751/Documents/rNodal.Rcheck"
    # from test:
    #      "C:/Users/msfz751/Documents/rNodal"

    # if it find keyword ".Rcheck", then it means we are in check mode
    ifelse(grepl(".Rcheck", prj_dir), TRUE, FALSE)
    # expect_equal(prj_dir, "C:/Users/msfz751/Documents/rNodal")
}


get_extdata_dir <- function(variables) {
    system.file("extdata", package = "rNodal")
}


isRpackage <- function() {
    # not working in project mode: test_rNodal
    # crit_r_pkg    <- rprojroot::is_r_package
    crit_r_pkg <- rprojroot::as.root_criterion("NAMESPACE")
    # expected <-  "contains a file `DESCRIPTION` with contents matching `^Package: `"
    expected <- "contains a file `NAMESPACE`"
    ifelse(crit_r_pkg$desc == expected, TRUE, FALSE)
}


# this causing error during build
is_saved_session <- function(session_file = "session.rda") {
    session_dir <- getProjectDir()
    .session_file <- paste(session_dir, session_file, sep = "/")
    ifelse(file.exists(.session_file), TRUE, FALSE)
}

getSessionFilename <- function(session_file = "session.rda") {
    if (!is_saved_session(session_file)) {
        warning("NO session file yet created")
        return(NULL)
    } else {
        return(paste(getProjectDir(), session_file, sep = "/"))
    }
}

#' Save the user HDF5 file to a persistant file
#' @keywords internal
saveSession <- function() {
    hdf5_file <- readFromProjectEnv("data.file.hdf5")
    save(hdf5_file, file = "session.rda")

}

#' Get the name of the default DataContainer
#' @keywords internal
getDefaultDataContainerName <- function() {
    readFromProjectEnv("data.file.hdf5")
}

# this was causing an error during the build
getProjectDir <- function() {
    # get the project folder
    getwd()
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
        warning(sprintf("HDF5 data container already exists.\n Use overwrite=TRUE.
                There are %d HDF5 files", length(listAllHdf5(where = "local"))))

    if (file.copy(from = source_file, to = target_dir, overwrite = overwrite)) {
        saveToProjectEnv("data.file.hdf5", target_file)
        saveSession()
    }
    else
        warning("File copy operation failed")
}


#' Logical response to presence of HDF5 files anywhere under user root folder
#' @keywords internal
is_hdf5_files <- function(where = "local") {
    stopifnot(where == "local" || where == "package")
    ifelse(length(listAllHdf5(where)) > 0, TRUE, FALSE)
}


#' List all HDF5 files
#' @keywords internal
listAllHdf5 <- function(where = "local") {
    if (where == "local")
        root_folder <- getProjectDir()
    else if (where == "package")
        root_folder <- get_extdata_dir()
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



get_list_examples <- function(folder = "examples", aPackage, ...) {
    # this is where examples live
    examples_dir <- system.file("examples", package = aPackage)

    # get all the scripts that `App` in them
    list.files(path = examples_dir, pattern = "*App", all.files = FALSE,
               full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
               include.dirs = FALSE, no.. = FALSE)

}
