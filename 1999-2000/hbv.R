library(epicalc)
source("./util/NHANES.R")


#immunization
imqd = read.csv("csv/quest/IMQ.csv")
#demographics
demod = read.csv("csv/DEMO.csv")
#medical condition
mcqd = read.csv("csv/quest/MCQ.csv")
#physical function
pfqd = read.csv("csv/quest/PFQ.csv")
#early childhood
ecqd = read.csv("csv/quest/ECQ.csv")
#hospital utilization
huqd = read.csv("csv/quest/HUQ.csv")
#prescriptions
rxq_rxd = read.csv("csv/quest/RXQ_RX.csv")
# some versions of R don't need this
# is there a better way? TODO
imqd$X <- NULL
demod$X <- NULL
mcqd$X <- NULL
pfqd$X <- NULL
ecqd$X <- NULL
huqd$X <- NULL
rxq_rxd$X <- NULL
mydata <- merge(demod, imqd, by="seqn", all=TRUE)
mydata <- merge(mydata, mcqd, by="seqn", all=TRUE)
mydata <- merge(mydata, pfqd, by="seqn", all=TRUE)
mydata <- merge(mydata, ecqd, by="seqn", all=TRUE)
mydata <- merge(mydata, huqd, by="seqn", all=TRUE)
# with duplicate seqn's
with_rx_data <- merge(mydata, rxq_rxd, by="seqn", all=TRUE)
with_rx_data_no_dupes <- with_rx_data[which(!duplicated(with_rx_data$seqn)), ]


male_data <- withoutFemales(mydata)
female_data <- withoutMales(mydata)


# Use males only between 1 and 9
print("Males Only Between 1 and 9")
byage <- keepByAge(male_data, 1, 9)
attach(byage)

pdf("hbv.pdf")
#options(device="pdf")
#layout(1:4)



'
Naively investigate HBV and SpecEd
'
spec_ed <- factor(pfq040, c("1", "2"), labels=c("SpecialEd", "Not"))
hbv <- factor(imq020, c("1", "3"), labels=c("Vaccinated", "UnVaccinated"))
ecq <- factor(ecq060, c("1", "2"), labels=c("NICU", "Not"))
add <- factor(mcq060, c("2", "1"), labels=c("Hasn't", "Has"))
age <- as.numeric(factor(ridageyr, 1:9))
health <- as.numeric(factor(huq010, 1:5))
print("======= Not quite statistically related 1 v 3")
plot(spec_ed, hbv, ylab="Vaccination", xlab="Special Ed",
     main="Vaccinated Males Slightly More Likely to be in Special Ed")
Pause()
print(cc(spec_ed, hbv))
Pause()
print("======= Significant if add NICU, age, health to glm")
model = glm(spec_ed ~ hbv + ecq + age + health, family=binomial)
print(summary(model))
Pause()
print("======= Add ADD status and not significant")
model = glm(spec_ed ~ hbv + ecq + age + health + add, family=binomial)
print(summary(model))
Pause()

# Use the mix of partial unvaccinated with unvaccinated
#  2+3 unvaccinated v 1 vaccinated
print("======= Unvaccinated and partially unvaccinated ")
hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
hbv <- factor(hbv, c("2or3", "1"), labels=c("Not Fully", "Fully Vaccinated"))
plot(spec_ed, hbv, main="Fully Vaccinated Males Slightly More Likely in SpecEd")
Pause()
spec_ed <- factor(pfq040, c("2", "1"), labels=c("Not", "SpecEd"))
print(cc(spec_ed, hbv, main="Fully Vaccinated Males 2.3 times more Likely in SpecEd"))

Pause()
print("======= Full HBV vaccination mildly associated with SpecEd")
model = glm(spec_ed ~ hbv + ecq + age + health, family=binomial)
print(summary(model))
Pause()
print("======= With ADD, HBV not significant predictor of SpecEd at .05")
model = glm(spec_ed ~ hbv + ecq + age + health + add, family=binomial)
print(summary(model))
Pause()

#Learning disability & ADD
print("======= ADD Diagnosis Quite Related to Special Ed")
plot(spec_ed, add, xlab="SpecialEd", ylab="ADD", main="Over 1/2 of Males with ADD in SpecEd")
Pause()
print("======= More than half of those with ADD in SpecEd")
print(cc(spec_ed, add, main="Boys with ADD, 17 times more likely in SpecEd"))
Pause()
print("======= Those with ADD 20 times more likely in SpecEd")
print(summary(glm(spec_ed~add, family=binomial)))
Pause()

ld <- factor(mcq083, c("2", "1"), labels=c("No", "Yes"))
#plot(spec_ed, ld)
#Pause()
#print(cc(spec_ed, ld))
#Pause()
#print(summary(glm(spec_ed~ld, family=binomial)))
#Pause()

#Full
#model = glm(spec_ed ~ hbv + ecq + age + health + add + ld, family=binomial)
#print(summary(model))

#Pause()
#print(cs(spec_ed, hbv))
#Pause()

print("Those with learning disabilities over 30 times more likely to be in SpecEd")
print(cc(spec_ed, ld, main="Learning Disability 30 times more likely in Special Ed",
      ylab="Odds of Special Ed", xlab="Learning Disability"))
Pause()
print(cc(ld, hbv, main="HBV, non-significant trend towards LD",
      ylab="Odds of LD", xlab="Fully Vaccinated or Not?"))
Pause()
print(cc(add, hbv, main="HBV, non-significant trend towards ADD",
      ylab="Odds of ADD", xlab="Fully Vaccinated or Not?"))
Pause()
print(cc(add, ld, main="LD Very Predictive of ADD", ylab="Odds of ADD", xlab="Has LD"))
Pause()


#detach(byage)
#byage <- male_data
#attach(byage)
#hbv <- factor(imq020, c("1", "3"), labels=c("Vaccinated", "UnVaccinated"))
#hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
#hbv <- factor(hbv, c("2or3", "1"))


print("FEMALE")
Pause()
# protective female effect


detach(byage)
byage <- keepByAge(female_data, 1, 9)
attach(byage)

spec_ed <- factor(pfq040, c("2", "1"), labels=c("No", "Yes"))
hbv <- factor(imq020, c("3", "1"), labels=c("No", "Yes"))
#ecq <- factor(ecq060, c("2", "1"), labels=c("No", "Yes"))
add <- factor(mcq060, c("2", "1"), labels=c("No", "Yes"))
age <- as.numeric(factor(ridageyr, 1:9))
#health <- as.numeric(factor(huq010, 1:5))

#model = glm(spec_ed ~ hbv + ecq + age + health, family=binomial)
model = glm(spec_ed ~ hbv + age, family=binomial)
print(summary(model))
Pause()
print(cc(spec_ed, hbv, main="Vaccinated Females Less Likely to be in SpecEd"))
Pause()


detach(byage)
byage <- keepByAge(with_rx_data_no_dupes, 1, 9)
attach(byage)
print("MALES AND FEMALES")
taking_rx <- factor(rxd030, c("2", "1"), labels=c("No RX", "RX"))
hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
hbv <- factor(hbv, c("2or3", "1"))
print("HBV exposure for those 1-9 leads to slightly greater risk of taking a prescription")
print(
cc(taking_rx, hbv,
   main="Fully Vaccinated Males & Females more likely to take RX")
)
Pause()

detach(byage)
males <- withoutFemales(byage)
attach(males)
print("MALES")
taking_rx <- factor(rxd030, c("2", "1"), labels=c("No RX", "RX"))
hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
hbv <- factor(hbv, c("2or3", "1"))
print(
    cc(taking_rx, hbv, main="Males Not Significantly More Likely to Take RX")
)
Pause()

detach(males)
females <- withoutMales(byage)
attach(females)
print("FEMALES")
taking_rx <- factor(rxd030, c("2", "1"), labels=c("No RX", "RX"))
hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
hbv <- factor(hbv, c("2or3", "1"))
print(
    cc(taking_rx, hbv,
       main="Fully Vaccinated Females 2.2X More Likely to Take RX")
)
detach(females)
Pause()


dev.off()


print("Drugs for hyperlipidemia less likely for the vaccinated")

# gender 0=both, 1=male, 2=female
ccVhbv <- function(name, codes, gender=0) {
    detachAllData()
    dframe <- byDrugStatus(with_rx_data, name, codes)
    dframe <- keepByAge(dframe, 1, 9)
    if (gender == 1) {
        dframe <- withoutFemales(dframe)
    }
    if (gender == 2) {
        dframe <- withoutMales(dframe)
    }
    attach(dframe)
    hbv <<- factor(ifelse(imq020=="2" | imq020=="3", "2or3", imq020))
    hbv <<- factor(hbv, c("2or3", "1"), labels=c("Not Fully", "Fully Vaccinated"))
    hbvnot <<- factor(imq020, c("3", "1"), labels=c("No", "Yes"))
    hbvrev <<- factor(ifelse(imq020=="2" | imq020=="1", "1or2", imq020))
    hbvrev <<- factor(hbvrev, c("3", "1or2"), labels=c("Not", "Some"))
    print("===== WITH PARTIAL")
    print(cc(dframe[[name]], hbv))
    Pause()
    print("===== 1 v 3")
    print(cc(dframe[[name]], hbvnot))
    Pause()
    print("SOME?NOT")
    print(cc(dframe[[name]], hbvrev))
    Pause()
    return(dframe)
}

# doPoisson(c(300, 1, 1226, 23), c("No", "No", "Yes", "Yes"), c("No", "Yes", "No", "Yes"))
# doPoisson(c(264, 37, 1102, 291), c("No", "No", "Yes", "Yes"), c("No", "Yes", "No", "Yes"))

doPoisson <- function(Y, exposure, outcome) {
    exposure <- factor(exposure)
    outcome <- factor(outcome)
    N <- sum(Y)
    pE <- c( (Y[1] + Y[2]) / N, (Y[3] + Y[4]) / N )
    pO <- c( (Y[1] + Y[3]) / N, (Y[2] + Y[4]) / N )
    E <- N * c(pE[1]*pO[1], pE[1]*pO[2], pE[2]*pO[1], pE[2]*pO[2])
    print(E)
    #X2 = sum((Y - E)^2/E)
    #G1 = sum(2*Y*log(Y/E))
    #G2 = sum(2*(Y*log(Y/E)+Y-E))
    #c(X2,G1,G2)
    print(chisq.test(matrix(Y,2,2), correct=F))
    Pause()
    print(chisq.test(matrix(Y,2,2)))
    Pause()
    cont.glm = glm(Y ~ exposure + outcome, family=poisson())
    print(summary(cont.glm))
    return(cont.glm)
}

