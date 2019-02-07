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
    session_dir <- getwd()
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


# this was causing an error during the build
getProjectDir <- function() {
    # get the project folder
    getwd()
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
    stopifnot(!is.null(aPackage)) # package must be provided
    # this is where examples live
    examples_dir <- system.file("examples", package = aPackage)

    # get all the scripts that `App` in them
    list.files(path = examples_dir, pattern = "*App", all.files = FALSE,
               full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
               include.dirs = FALSE, no.. = FALSE)

}
