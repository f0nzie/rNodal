


useLatestHdf5 <- function() {
    if (isHdf5Files())
        hdf5_files <- listAllHdf5()
    else
        stop("No HDF5 files found")

}

#' Logical response to presence of HDF5 files anywhere under user root folder
#' @export
isHdf5Files <- function() {
    ifelse(length(listAllHdf5()) > 0, TRUE, FALSE)
}

#' List all HDF5 files
#'
#' @export
listAllHdf5 <- function() {
    root_folder <- system.file("extdata", package = "rNodal")
    stopif(nchar(root_folder) == 0)
    # stopifnot(length(user_root_folder))
    list.files(path = root_folder, pattern = "*.h5$|*.hdf5$",
               all.files = FALSE, full.names = TRUE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
}


#' File info for all HDF5 files
#' @export
fileInfoHdf5 <- function() {
    file.info(listAllHdf5())
}

#' Bigger HDF5
#' @export
biggerHdf5 <- function() {
    df <- fileInfoHdf5()
    row.names(df[which(max(df$size) == df$size),])
}




#' Ensure that R expressions are false
#'
#' @param ...
#' Any number of (logical) R expressions,
#' which should evaluate to \code{TRUE}.
#' @export
#' @examples
#' \dontrun{
#' stopif(is.empty(c(2,1)), 4 < 3) # all FALSE
#' stopif(is.empty(numeric(0)))
#' }
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
#' @param ... any additional parameter
#' @importFrom stats setNames
#' @export
named.list <- function(...) {
    nl <- setNames( list(...) , as.character( match.call()[-1]) )
    # nl <- setNames( list(...) , as.character( match.call()[-1]) )
    nl
}
