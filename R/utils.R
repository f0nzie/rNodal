

#' Logical response to presence of HDF5 files anywhere under user root folder
#' @export
isHdf5Files <- function() {
    ifelse(length(listAllHdf5()) > 0, TRUE, FALSE)
}

#' List all HDF5 files
#'
#' @export
listAllHdf5 <- function() {
    user_root_folder <- rprojroot::find_rstudio_root_file()
    list.files(path = user_root_folder, pattern = "*.h5$|*.hdf5$",
               all.files = FALSE, full.names = FALSE, recursive = TRUE,
               ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)
}



#' Take objects and create a list using their names
#'
#' @param ... any additional parameter
#' @importFrom stats setNames
#' @export
named.list <- function(...) {
    nl <- setNames( list(...) , as.character( match.call()[-1]) )
    # nl <- setNames( list(...) , as.character( match.call()[-1]) )
    nl
}


