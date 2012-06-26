library(epicalc)


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


mydata = merge(imqd, demod, by="seqn")
mydata = merge(mydata, mcqd, by="seqn")
mydata = merge(mydata, pfqd, by="seqn")
mydata = merge(mydata, ecqd, by="seqn")
mydata = merge(mydata, huqd, by="seqn")

#attach(mydata)

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

male_data <- withoutFemales(mydata)
female_data <- withoutMales(mydata)


#add <- as.numeric(factor(mcq060, c("1", "2")))
#hbv <- factor(imq020, c("1", "3"))
#gender <- factor(riagendr, c("1", "2"))
#age <- as.numeric(ridageyr)
#spec_ed <- as.numeric(factor(pfq040, c("1", "2")))


#model = lm(add ~ hbv + gender)
#model = lm(spec_ed ~ hbv)
#model = lm(spec_ed ~ hbv + gender)
#model = lm(spec_ed ~ hbv + gender + age)



#male_spec_ed <- as.numeric(factor(male_data$pfq040, c("1","2")))
#male_hbv <- factor(male_data$imq020, c("1", "3"))
#model = lm(male_spec_ed ~ male_hbv)

byage <- keepByAge(male_data, 1, 9)

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



