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
#RX
rxq = read.csv("csv/quest/RXQ_RX.csv")
#med condition
mcq = read.csv("csv/quest/MCQ.csv")
#cogfunc
cfq = read.csv("csv/quest/CFQ.csv")
#diabetes
diq = read.csv("csv/quest/DIQ.csv")

demo$X <- NULL
lab13am$X <- NULL
cdq$X <- NULL
rxq$X <- NULL
mcq$X <- NULL
cfq$X <- NULL
diq$X <- NULL

mydata = merge(demo, lab13am, by="seqn")
mydata = merge(mydata, cdq, by="seqn")
mydata = merge(mydata, rxq, by="seqn")
mydata = merge(mydata, mcq, by="seqn")
mydata = merge(mydata, cfq, by="seqn")
mydata = merge(mydata, diq, by="seqn")

#male_data <- withoutFemales(mydata)
#female_data <- withoutMales(mydata)
#byage <- keepByAge(male_data, 1, 9)

attach(mydata)

#sobsi <- factor(cdq010, c("1", "2"), labels=c("1", "0"))
#sobsi <- factor(ifelse(cdq010 == 2, 0, cdq010), c("0", "1"))
sobsi <- factor(cdq010, c("2", "1"), labels=c("Hasn't", "Has"))
ldl <- as.numeric(lbdldlsi)
c <- cut(ldl, c(-Inf, 3.15, Inf), labels=c("Lower", "Higher"))
num_rx <- as.numeric(rxd295)
asthma <- factor(mcq010, c("2", "1"), labels=c("Hasn't", "Has"))
diabetes <- factor(diq010, c("2", "1"), labels=c("Hasn't", "Has"))
quartiles <- cut(ldl, c(-Inf, 2.64, 3.15, 3.83, Inf), labels=c("1st", "2nd", "3rd", "4th"))

# ldl (c) as +/- median
png(filename="img/ldl_sob_cc.png")
cc(sobsi, c,
   ylab="Shortness of Breath Odds", xlab="LDL (mmol/L)",
   main="LDL Protective Against Shortness of Breath"
)
dev.off()
Pause()
# ldl in quartiles for SOB
png(filename="img/ldl_sob_cc_quartiles.png")
cc(sobsi, quartiles,
   ylab="Odds of Shortness of Breath", xlab="LDL Quartile",
   main="Low LDL Associated with Shortness of Breath"
)
dev.off()
Pause()
plot(sobsi, c, xlab="Shortness of Breath", ylab="LDL")
Pause()
plot(sobsi, ldl, xlab="Shortness of Breath", ylab="LDL",
     main="Trend is slight")
Pause()
scatter.smooth(ldl, sobsi)

Pause()
model = glm(sobsi ~ c + asthma, family=binomial)
print(summary(model))
Pause()
numeric_model = glm(sobsi ~ ldl + asthma, family=binomial)
print(summary(numeric_model))
Pause()


# swelling of the extremities (feet/ankles)
swelling <- factor(cdq080, c(2, 1), labels=c("Hasn't", "Has"))
model = glm(swelling ~ c, family=binomial)
print(summary(model))
png(filename="img/ldl_swelling_cc.png")
cc(swelling, c, main="Higher LDL Protective Against Swelling Extremities?",
    ylab="Odds of Swollen Extremities", xlab="LDL")
dev.off()
Pause()
png(filename="img/ldl_swelling_cc_quartiles.png")
cc(swelling, quartiles, main="Lowest LDL has Greatest Odds of Swelling Feet/Ankles", ylab="Swelling Odds", xlab="LDL")
dev.off()
Pause()
model = glm(swelling ~ ldl, family=binomial)
print(summary(model))

# diabetes
# +/- median ldl not quite statistically significant predictor of diabetes

Pause()
png(filename="img/ldl_diabetes_quartiles.png")
plot(diabetes, quartiles, main="Highest LDL Least Likely to Have Diabetes", ylab="LDL Quartiles", xlab="Diabetes")
dev.off()
Pause()

png(filename="img/ldl_diabetes_cc.png")
cc(diabetes, c, main="Trend Towards Less Diabetes Risk with Higher LDL", ylab="Odds of Diabetes", xlab="LDL")
dev.off()
Pause()

png(filename="img/ldl_diabetes_cc_quartiles.png")
cc(diabetes, quartiles, xlab="LDL", ylab="Odds of Diabetes", main="Diabetes and LDL Quartiles")
dev.off()
model = glm(diabetes ~ ldl, family=binomial)
print(summary(model))
Pause()

#ldl         -0.26967    0.08123  -3.320  0.00090 ***
model = glm(diabetes ~ ldl, family=binomial)
print(summary(model))



