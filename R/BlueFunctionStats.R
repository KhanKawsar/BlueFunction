
blueFunctionData <- read_csv("Data/blueFunctionData.csv")
View(blueFunctionData)



###############Discriminibality analyses###############

##To calculate if the data is normally distributed we need to perfom qqnorm plot and shapiro.test

hist(blueFunctionData$chromaticMale)
hist(blueFunctionData$chromaticFemale)
hist(blueFunctionData$achromaticMale)
hist(blueFunctionData$achromaticFemale)

qqnorm(blueFunctionData$chromaticMale);qqline(blueFunctionData$chromaticMale, col = 2)
qqnorm(blueFunctionData$chromaticFemale);qqline(blueFunctionData$chromaticFemale, col = 2)

shapiro.test(blueFunctionData$chromaticMale)
shapiro.test(blueFunctionData$chromaticFemale)

qqnorm(blueFunctionData$achromaticMale);qqline(blueFunctionData$achromaticMale, col = 2)
qqnorm(blueFunctionData$achromaticFemale);qqline(blueFunctionData$achromaticFemale, col = 2)

shapiro.test(blueFunctionData$achromaticMale)
shapiro.test(blueFunctionData$achromaticFemale)

##The null hypothesis of shapiro.test is that the data are normally distributed
#If p valus of shapiro.test is less than 0.05 then data are not normally distributed 


##To calculate varience of normal distribution data we need to perfom F-test

chromatic_ftest <- var.test(blueFunctionData$chromaticMale,blueFunctionData$chromaticFemale)
achromatic_ftest <- var.test(blueFunctionData$achromaticMale,blueFunctionData$achromaticFemale)

#the null hypothesis of the test is that there is no difference of variance between two compared groups
#if p value is less than 0.05 there is significant difference of variance between the two compared groups


### My data suggests that the data are normally distributed and there in significant difference between groups
## I need to follow Welch two sample t test to determine if there is significant difference between groups

chromatic_wlch_t_ests<- t.test(blueFunctionData$chromaticMale, blueFunctionData$chromaticFemale)
chromatic_wlch_t_ests

achromatic_wlch_t_ests<- t.test(blueFunctionData$achromaticMale, blueFunctionData$achromaticFemale)
achromatic_wlch_t_ests

##Chromatic difference between groups is significant but achromatic differnece is not



########################female preferences anlyses##################

##histogram of the dataset

hist(blueFunctionData$controlTandem)
hist(blueFunctionData$manipulatedTandem)
hist(blueFunctionData$controlTandem2wheel)
hist(blueFunctionData$manipulatedTandem2wheel)
hist(blueFunctionData$controlWheel)
hist(blueFunctionData$manipulatedWheel)

##qqnorm plor and line of the dataset

qqnorm(blueFunctionData$controlTandem);qqline(blueFunctionData$controlTandem, col = 2)
qqnorm(blueFunctionData$manipulatedTandem);qqline(blueFunctionData$manipulatedTandem, col = 2)
qqnorm(blueFunctionData$controlTandem2wheel);qqline(blueFunctionData$controlTandem2wheel, col = 2)
qqnorm(blueFunctionData$manipulatedTandem2wheel);qqline(blueFunctionData$manipulatedTandem2wheel, col = 2)
qqnorm(blueFunctionData$controlWheel);qqline(blueFunctionData$controlWheel, col = 2)

#qqnorm and qqline suggest tandem and tandem to wheel data are not normally distributed however wheel data are normally distributed


##shapiro.test of the dataset

shapiro.test(blueFunctionData$controlTandem)
shapiro.test(blueFunctionData$manipulatedTandem)
shapiro.test(blueFunctionData$controlTandem2wheel)
shapiro.test(blueFunctionData$manipulatedTandem2wheel)
shapiro.test(blueFunctionData$controlWheel)
shapiro.test(blueFunctionData$manipulatedWheel)

#shapiro.test confirm tandem and tandem to wheel data are not normally distributed however wheel data are normally distributed




### variation betwwen groups: wheel formation F test as data are normally distributed

var.test(blueFunctionData$controlWheel,blueFunctionData$manipulatedWheel)


### As the tandem duration and tandem to wheel duratrion are not equal we will follow Mann Whitney U test
##unpair wilcox.test is Mann whitney U test. by default the wilco.test is unpair

wilcox.test(blueFunctionData$controlTandem, blueFunctionData$manipulatedTandem)
wilcox.test(blueFunctionData$controlTandem2wheel, blueFunctionData$manipulatedTandem2wheel)

###As wheel duration follow normal distribution and equal varience we will follow two sample t-test
t.test(blueFunctionData$controlWheel, blueFunctionData$manipulatedWheel, var.equal = TRUE)
##by default t.test consider variance is not equal and perform welch t-test. as we wants to perfom two sample t-test, we need to write var.equal=TRUE

##by default t-test is Welch t.test. to calculate two sample t test we have to select var.equal= TRUE)


#######################chi square test #####################



##Chi-square test. A total of 40 mating trial was conducted to determine if the male 
#prefer control or manipulated female. Male mated with 27 control females and 13 manipulated females

#######female preferances######
R1 = c(27, 13)
R2 = c(13, 27) 

data = matrix(c(R1, R2), nrow= 2, byrow=TRUE)

rownames(data) = c("control", "S8-9 manipulated")          
colnames(data) = c("mate", "nomate")

data

chisq.test(data, correct = FALSE)

#no need to use Yates correction, expected value not 
#less than 5,10 and people often recomend not to use.
###############S4  manipulated #################
R1 = c(22, 18)
R2 = c(18, 22) 

data = matrix(c(R1, R2), nrow= 2, byrow=TRUE)

rownames(data) = c("control", "S4 manipulated")          
colnames(data) = c("mate", "nomate")

data

chisq.test(data, correct = FALSE)

#########tandem blue no blue ############

R1 = c(31, 32)
R2 = c(33, 30) 

data = matrix(c(R1, R2), nrow= 2, byrow=TRUE)

rownames(data) = c("control", "manipulated")          
colnames(data) = c("mate", "nomate")

data

chisq.test(data, correct = FALSE)

########################wheel blue no blue #############
R1 = c(17, 14)
R2 = c(14, 19) 

data = matrix(c(R1, R2), nrow= 2, byrow=TRUE)

rownames(data) = c("control", "manipulated")          
colnames(data) = c("mate", "nomate")

data

chisq.test(data, correct = FALSE)
