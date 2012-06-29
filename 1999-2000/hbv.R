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
imqd$X <- NULL
demod$X <- NULL
mcqd$X <- NULL
pfqd$X <- NULL
ecqd$X <- NULL
huqd$X <- NULL
mydata = merge(demod, imqd, by="seqn")
mydata = merge(mydata, mcqd, by="seqn")
mydata = merge(mydata, pfqd, by="seqn")
mydata = merge(mydata, ecqd, by="seqn")
mydata = merge(mydata, huqd, by="seqn")


male_data <- withoutFemales(mydata)
female_data <- withoutMales(mydata)


# Use males only between 1 and 9
byage <- keepByAge(male_data, 1, 9)
attach(byage)

'
Naively investigate HBV and SpecEd
'
spec_ed <- factor(pfq040, c("1", "2"), labels=c("SpecialEd", "Not"))
hbv <- factor(imq020, c("1", "3"), labels=c("Vaccinated", "UnVaccinated"))
ecq <- factor(ecq060, c("1", "2"), labels=c("NICU", "Not"))
add <- factor(mcq060, c("1", "2"), labels=c("Has", "Hasn't"))
age <- as.numeric(factor(ridageyr, 1:9))
health <- as.numeric(factor(huq010, 1:5))
#Not quite statistically related on its own
plot(spec_ed, hbv)
print(cc(spec_ed, hbv))
Pause()
#Significant if add NICU, age, health to glm
model = glm(spec_ed ~ hbv + ecq + age + health, family=binomial)
print(summary(model))
Pause()
#Add ADD status and not significant
model = glm(spec_ed ~ hbv + ecq + age + health + add, family=binomial)
print(summary(model))
Pause()

'
Use the mix of 2+3 unvaccinated VS 1
and try to make look bad
'
hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
hbv <- factor(hbv, c("2or3", "1"))
spec_ed <- factor(pfq040, c("2", "1"), labels=c("Not", "SpecEd"))

cc(spec_ed, hbv)
Pause()
model = glm(spec_ed ~ hbv + ecq + age + health, family=binomial)
print(summary(model))
model = glm(spec_ed ~ hbv + ecq + age + health + add, family=binomial)
print(summary(model))
Pause()


'
print("FEMALE")

# protective female effect


detach(byage)
byage <- keepByAge(female_data, 1, 9)
attach(byage)

spec_ed <- as.numeric(factor(ifelse(is.element(pfq040, seq("1","2")), pfq040 - 1, NA)))
hbv <- factor(imq020, c("1", "3"))
#hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
#hbv <- factor(hbv, c("1", "2or3"))

#factor(factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020)), c("1", "2or3"))

ecq <- factor(ecq060, c("1", "2"))
add <- factor(mcq060, c("1", "2"))
age <- as.numeric(factor(ridageyr, 1:9))
health <- as.numeric(factor(huq010, 1:5))

model = lm(spec_ed ~ hbv + ecq + age + health)
print(summary(model))
print(anova(model))
print(cs(spec_ed, hbv))
print(cc(spec_ed, hbv))
'


