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
# some versions of R don't need this
# is there a better way? TODO
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
add <- factor(mcq060, c("2", "1"), labels=c("Hasn't", "Has"))
age <- as.numeric(factor(ridageyr, 1:9))
health <- as.numeric(factor(huq010, 1:5))
print("Not quite statistically related on its own")
plot(spec_ed, hbv)
Pause()
print(cc(spec_ed, hbv))
Pause()
print("Significant if add NICU, age, health to glm")
model = glm(spec_ed ~ hbv + ecq + age + health, family=binomial)
print(summary(model))
Pause()
print("Add ADD status and not significant")
model = glm(spec_ed ~ hbv + ecq + age + health + add, family=binomial)
print(summary(model))
Pause()

# Use the mix of 2+3 unvaccinated VS 1 and try to make look bad
hbv <- factor(ifelse(imq020=="2" | imq020=="3" ,"2or3", imq020))
hbv <- factor(hbv, c("2or3", "1"))
spec_ed <- factor(pfq040, c("2", "1"), labels=c("Not", "SpecEd"))

print(cc(spec_ed, hbv))
Pause()
model = glm(spec_ed ~ hbv + ecq + age + health, family=binomial)
print(summary(model))
Pause()
model = glm(spec_ed ~ hbv + ecq + age + health + add, family=binomial)
print(summary(model))
Pause()

#Learning disability & ADD
plot(spec_ed, add, xlab="SpecialEd", ylab="ADD")
Pause()
print(cc(spec_ed, add))
Pause()
print(summary(glm(spec_ed~add, family=binomial)))
Pause()

ld <- factor(mcq083, c("2", "1"), labels=c("No", "Yes"))
plot(spec_ed, ld)
Pause()
print(cc(spec_ed, ld))
Pause()
print(summary(glm(spec_ed~ld, family=binomial)))
Pause()

#Full
model = glm(spec_ed ~ hbv + ecq + age + health + add + ld, family=binomial)
print(summary(model))

Pause()
print(cs(spec_ed, hbv))
Pause()

'
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
print(cs(spec_ed, hbv))
Pause()
print(cc(spec_ed, hbv))
'



