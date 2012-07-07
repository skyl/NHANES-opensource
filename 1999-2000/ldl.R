"
LDL as an independent variable.
Predictive of CVD/Diabetes/etc?
"

library(car)
library(epicalc)
source("util/NHANES.R")

#demographics
demo = read.csv("csv/DEMO.csv")
#lab
lab13am = read.csv("csv/lab/LAB13AM.csv")
#Cardio Dis
cdq = read.csv("csv/quest/CDQ.csv")
#med condition
mcq = read.csv("csv/quest/MCQ.csv")
#cogfunc
cfq = read.csv("csv/quest/CFQ.csv")
#diabetes
diq = read.csv("csv/quest/DIQ.csv")
# RX
rxq = read.csv("csv/quest/RXQ_RX.csv")

demo$X <- NULL
lab13am$X <- NULL
cdq$X <- NULL
mcq$X <- NULL
cfq$X <- NULL
diq$X <- NULL
rxq$X <- NULL

full = merge(demo, lab13am, by="seqn", all=TRUE)
full = merge(full, cdq, by="seqn", all=TRUE)
full = merge(full, mcq, by="seqn", all=TRUE)
full = merge(full, cfq, by="seqn", all=TRUE)
full = merge(full, diq, by="seqn", all=TRUE)

print(length(full$ridageyr))
print(summary(full$ridageyr))
blah = merge(full, rxq, by="seqn", all=TRUE)
print(length(blah$ridageyr))
print(summary(blah$ridageyr))


over_20 = full[full$ridageyr >= 20,]
over_40 = full[full$ridageyr >= 40,]
over_60 = full[full$ridageyr >= 60,]
under_40 = full[full$ridageyr < 40,]


loadVariables <- function() {

    ldl <<- as.numeric(lbdldlsi)
    ldl_summary <<- summary(ldl)
    ldl_median <<- ldl_summary[3]
    ldlhalves <<- cut(ldl, c(-Inf, ldl_median, Inf), labels=c("Lower", "Higher"))
    ldlquartiles <<- cut(ldl, c(-Inf, ldl_summary[2], ldl_median, ldl_summary[5], Inf),
                     labels=c("1st", "2nd", "3rd", "4th"))
    # short of breath, stairs/inclines
    sobsi <<- factor(cdq010, c("2", "1"), labels=c("Hasn't", "Has"))
    asthma <<- factor(mcq010, c("2", "1"), labels=c("Hasn't", "Has"))
    diabetes <<- factor(diq010, c("2", "1"), labels=c("Hasn't", "Has"))
    swelling <<- factor(cdq080, c(2, 1), labels=c("Hasn't", "Has"))
}


print("OVER 40")
attach(over_40)
loadVariables()
cc(diabetes, ldlhalves)
Pause()
cc(diabetes, ldlquartiles)
Pause()
print(summary(glm(diabetes ~ ldl + over_40$ridageyr, family=binomial)))
Pause()
detach(over_40)


