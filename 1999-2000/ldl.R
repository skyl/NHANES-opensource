"
LDL as an independent variable.
Predictive of CVD/Diabetes/etc?
"

library(car)
library(epicalc)
source("util/NHANES.R")

#demographics
demo = read.csv("csv/DEMO.csv")

# LAB
lab13am = read.csv("csv/lab/LAB13AM.csv")
lab13 = read.csv("csv/lab/Lab13.csv")

# QUEST
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

# EXAM
#body Measures
bmx = read.csv("csv/exam/BMX.csv")
#blood pressure
bpx = read.csv("csv/exam/BPX.csv")
#muscle strength
msx = read.csv("csv/exam/MSX.csv")

demo$X <- NULL
lab13am$X <- NULL
lab13$X <- NULL
cdq$X <- NULL
mcq$X <- NULL
cfq$X <- NULL
diq$X <- NULL
rxq$X <- NULL
bmx$X <- NULL
bpx$X <- NULL
msx$X <- NULL

full <- merge(demo, lab13am, by="seqn", all=TRUE)
full <- merge(full, lab13, by="seqn", all=TRUE)
full <- merge(full, cdq, by="seqn", all=TRUE)
full <- merge(full, mcq, by="seqn", all=TRUE)
full <- merge(full, cfq, by="seqn", all=TRUE)
full <- merge(full, diq, by="seqn", all=TRUE)
full <- merge(full, bmx, by="seqn", all=TRUE)
full <- merge(full, bpx, by="seqn", all=TRUE)
full <- merge(full, msx, by="seqn", all=TRUE)

# multiple rows per key in RX data
#blah = merge(full, rxq, by="seqn", all=TRUE)


over_20 = full[full$ridageyr >= 20,]
over_40 = full[full$ridageyr >= 40,]
over_60 = full[full$ridageyr >= 60,]
under_40 = full[full$ridageyr < 40,]


loadVariables <- function() {

    ldl <<- as.numeric(lbdldlsi)
    ldl_summary <<- summary(ldl)
    ldl_median <<- ldl_summary[3]
    ldlhalves <<- cut(ldl,
        c(-Inf, ldl_median - .0001, Inf),
        labels=c("Lower", "Higher"))
    ldlquartiles <<- cut(
        ldl,
        c(-Inf, ldl_summary[2] - .0001,
          ldl_median - .0001, ldl_summary[5] - .0001, Inf),
        labels=c("1st", "2nd", "3rd", "4th")
    )
    tc <<- as.numeric(lbdtcsi)
    tc_summary <<- summary(tc)
    tc_median <<- tc_summary[3]
    tchalves <<- cut(tc,
        c(-Inf, tc_median - .0001, Inf),
        labels=c("Lower", "Higher"))
    tcquartiles <<- cut(
        tc,
        c(-Inf, tc_summary[2] - .0001,
          tc_median - .0001, tc_summary[5] - .0001, Inf),
        labels=c("1st", "2nd", "3rd", "4th")
    )

    # short of breath, stairs/inclines
    sobsi <<- factor(cdq010, c("2", "1"), labels=c("Hasn't", "Has"))
    asthma <<- factor(mcq010, c("2", "1"), labels=c("Hasn't", "Has"))
    diabetes <<- factor(diq010, c("2", "1"), labels=c("Hasn't", "Has"))
    swelling <<- factor(cdq080, c(2, 1), labels=c("Hasn't", "Has"))

    weight <<- bmxwt

}


print("OVER 40")
print("HAS DIABETES?")
attach(over_40)
loadVariables()
cc(diabetes, ldlhalves, main="Over-median LDL predicts Not Diabetes for over 40s",
    xlab="LDL", ylab="Odds Has Diabetes")
Pause()
cc(diabetes, ldlquartiles, main="Less Diabetes Among the Highest LDL",
    xlab="LDL", ylab="Odds Has Diabetes")
Pause()
print(summary(glm(diabetes ~ ldl + over_40$ridageyr, family=binomial)))
Pause()

print("WEIGHT")
print(summary(lm(weight ~ ldl + over_40$ridageyr)))
Pause()
print("BODY MASS INDEX")
print(summary(lm(over_40$bmxbmi ~ ldl + over_40$ridageyr)))
Pause()
print(summary(lm(over_40$bmxwaist ~ ldl + over_40$ridageyr)))





detach(over_40)


