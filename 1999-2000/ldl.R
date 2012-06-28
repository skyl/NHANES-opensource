"
Hello

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
mcqd = read.csv("csv/quest/MCQ.csv")
#cogfunc
cfqd = read.csv("csv/quest/CFQ.csv")

mydata = merge(demo, lab13am, by="seqn")
mydata = merge(mydata, cdq, by="seqn")
mydata = merge(mydata, rxq, by="seqn")
mydata = merge(mydata, mcqd, by="seqn")
mydata = merge(mydata, cfqd, by="seqn")

#male_data <- withoutFemales(mydata)
#female_data <- withoutMales(mydata)
#byage <- keepByAge(male_data, 1, 9)

attach(mydata)

#sobsi <- factor(cdq010, c("1", "2"), labels=c("1", "0"))
#sobsi <- factor(ifelse(cdq010 == 2, 0, cdq010), c("0", "1"))
sobsi <- factor(cdq010, c("2", "1"), labels=c("Hasn't", "Has"))
ldl <- as.numeric(lbdldlsi)
c <- cut(ldl, c(-Inf, 3.15, Inf), labels=c("Lower", "Higher"))
#num_rx <- as.numeric(rxd295)
asthma <- factor(mcq010, c("2", "1"), labels=c("Hasn't", "Has"))

# ldl (c) as +/- median
cc(sobsi, c,
   ylab="Shortness of Breath Odds", xlab="LDL (mmol/L)",
   main="LDL Protective Against Shortness of Breath"
)
Pause()
plot(sobsi, c, xlab="Shortness of Breath", ylab="LDL")
Pause()
plot(sobsi, ldl, xlab="Shortness of Breath", ylab="LDL",
     main="Trend is slight")
Pause()
scatter.smooth(ldl, sobsi)

Pause()
model = glm(sobsi ~ c + asthma, family=binomial)
numeric_model = glm(sobsi ~ ldl + asthma, family=binomial)

print(summary(model))
print(summary(numeric_model))

print("Those with higher than median LDL have")
print(exp(coef(model)[2]))
print("the odds of shortness of breath")
print("for each 1 mmol/L LDL increase, odds of shortness of breath decrease to")
print(exp(coef(numeric_model)[2]))


Pause()
swelling <- factor(cdq080, c(1,2), labels=c("Has", "Hasn't"))
model = glm(swelling ~ c, family=binomial)
summary(model)
cc(swelling, c)

Pause()
model = glm(swelling ~ ldl, family=binomial)
summary(model)


