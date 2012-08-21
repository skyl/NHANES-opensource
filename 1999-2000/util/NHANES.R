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

byDrugStatus <- function(dframe, destination, codes) {
    trueids <- dframe[
        is.element(dframe$fdacode1, codes)
        | is.element(dframe$fdacode2, codes)
        | is.element(dframe$fdacode3, codes)
        | is.element(dframe$fdacode4, codes)
        | is.element(dframe$fdacode5, codes) 
    , "seqn"]
    dframe <- dframe[which(!duplicated(dframe$seqn)), ]
    f <- factor(ifelse(is.element(dframe$seqn, trueids), TRUE, FALSE))
    #dframe[[destination]] <- f
    dframe[, destination] <-f
    return(dframe)
}
