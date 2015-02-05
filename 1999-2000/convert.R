library(Hmisc)

convertXPTtoCSV <- function(from_path, to_path) {
    xpt <- sasxport.get(from_path)
    write.csv(xpt, file=to_path)
}

