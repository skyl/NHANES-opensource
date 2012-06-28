Pause <- function () { 
    cat("Hit <enter> to continue...")
    readline()
    invisible()
}
withoutFemales <- function(dframe) {
    n <- dframe[is.element(dframe$riagendr, c("1")), ]
    return(n)
}
withoutMales <- function(dframe) {
    n <- dframe[is.element(dframe$riagendr, c("2")), ]
    return(n)
}
keepByAge <- function(dframe, min, max) {
    n <- dframe[is.element(dframe$ridageyr, seq(min, max)), ]
    return(n)
}
