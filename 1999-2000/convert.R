library(Hmisc)

moveAndDumpRelative <- function(wd, td) {
  setwd(wd)
  for (filename in list.files(pattern="*.xpt")) {
    print(filename)
    xpt <- sasxport.get(filename)
    fl <- strsplit(filename, "\\.")
    target <- paste(td, fl[[1]][1], ".csv", sep="")
    print(target)
    write.csv(xpt, file=target)
  }
}

moveAndDumpRelative("xpt", "../csv/")
moveAndDumpRelative("diet", "../../csv/diet/")
moveAndDumpRelative("../exam/", "../../csv/exam/")
moveAndDumpRelative("../lab/", "../../csv/lab/")
moveAndDumpRelative("../quest/", "../../csv/quest/")

