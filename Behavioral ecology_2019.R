
##################################### Analysis ############

library(pavo)
par(mar=c(5,4,1,1), mgp= c(2.5,1,0))
aggblue<- read.csv("Data/bluespec3.1.csv")
aggvlue <- as.rspec(aggblue) 

sam <- summary(aggvlue, subset= c('H1', 'B1'))
write.csv(sam, "summarypeak.csv")
peak <-peakshape(aggvlue, lim = c(300, 600), plot = TRUE)
write.csv(peak, "peak.csv")

aggsmooth <- procspec(aggvlue, opt = 'smooth', span= 0.5)
group <- gsub('\\.[0-9].*$', '', names(aggsmooth))[-1]
table(group)
aggplot(aggsmooth, group, lwd= 2, 
        lcol= c("red", "royalblue", "magenta", "purple", "black"), 
        shadecol= c("red", "royalblue", "magenta",  "purple", "black"), 
        xlab= "Wavelength(nm)", ylab= "Reflactance(%)", 
        cex.lab = 1.5, cex.axis= 1.5)

mtext("(c)", side=3, line=0.5, adj=0, cex=1.0)

#######  visual modeling to calculate color distance using RNM#######

##in latest pavo version relative densities is ignored, instead need 
#to input weber.ref- the receptor used to calculate the weber
###for color distance in RNM "relative = FALSE" 
###use von kries  = TRUE only in case of color hexagon, when qcatch = Ei, deaults = FALSE
###when von kries  = FALSE bkg have no effect
####in color space model relative = TRUE ; in RNM and color hexagon relative = FALSE


### for RNM its important to know the n, weber, weber.achro, weber.ref

sensdamsel <- sensmodel(c(360, 450, 525))
visdamsel <- vismodel(aggvlue, visual=sensdamsel, 
                      achromatic = 'l', illum= 'D65', 
                      qcatch = 'fi', scale =10000, 
                      relative= FALSE)
coldisdamsel<- coldist(visdamsel, noise = 'neural', 
                       achro= 'TRUE', weber = 0.12,
                       n=c(1.0,0.071,4.412))
#cone density is ignored in new version
write.csv(coldisdamsel, "coldistanceblue_3.0.csv")


##########Internal contrast using RNM in tetrachromatic vision #######

### vonkries = TRUE in color space model; bkg have no effect
####weber, weber.achro, needed

sensdamsel.tetra <- sensmodel(c(370, 440, 540, 600))
m <- sensdamsel.tetra$lmax540

visdamsel.tetra <- vismodel(aggvlue, visual=sensdamsel.tetra, 
                            achromatic = m, illum= 'D65', 
                            qcatch = 'fi', scale =10000, 
                            relative= FALSE)

coldisdamsel.tetra<- coldist(visdamsel.tetra, noise = 'neural', 
                             achro= 'TRUE', weber = 0.12,
                             n=c(2, 2.5, 2.5, 1))
write.csv(coldisdamsel.tetra, "coldistanceblue.tetra_3.2.csv")



######Supplementary figure 1 #######

pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 0.5
pagePaper <- 'special'
fontFamily <- 'Times'
png("output/Supplementary Figure 1.png", width=pageWidthLarge, height= pageHeightLarge,
    units= "in",type= "cairo", res = 600)

par(mar=c(5,4,1,1), mgp= c(2.5,1,0))
bluepaint<- read.csv("Data/BluPaint.csv")
bluepaint <- as.rspec(bluepaint) 
blupaintsam <- summary(bluepaint, subset= c('H1', 'B1'))
write.csv(blupaintsam, "blupaintsummary.csv")
blusmooth <- procspec(bluepaint, opt = 'smooth', span= 0.5)
group <- gsub('\\.[0-9].*$', '', names(blusmooth))[-1]
table(group)
aggplot(blusmooth, group, lwd= 3.0, legend = FALSE, 
        lcol= c("blue", "cyan3"), 
        shadecol= c("blue", "cyan3"), 
        xlab= "Wavelength(nm)", ylab= "Reflactance(%)", 
        cex.lab = 1.5, cex.axis= 1.5)
legend("topleft", legend= c("Male S8",  "Blue paint"),
       col=c("blue", "cyan3"), lty=1, lwd= 2, cex=1.0, bty = "n")



dev.off ()

########################Figure 1##########################################################



pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 1.5
pagePaper <- 'special'
fontFamily <- 'Times'
png("output/Figure_3.1.png", width=pageWidthLarge, height= pageHeightLarge,
    units= "in",type= "cairo", res = 600)

#pdf("output/Figure_3.1.pdf", width=pageWidthLarge, 
    #height= pageHeightLarge, family=fontFamily, paper=pagePaper)

layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE), heights = c(0.6,1))


#####To upload a photo in R #### (A)
library(jpeg)
image<- readJPEG("Data/collage.jpg") 
pixelWidth <- dim(image)[2]
pixelHeight <- dim(image)[1]
par(mar=c(0,0,0,0))
#Setup a plot region with domain and range from 0 to 2, and no axes or labels
plot(NULL, xlim=c(0,pixelWidth), ylim=c(0,pixelHeight), axes=FALSE,xlab= "" , ylab="", asp= 1)
#Draw the raster image across (half y space and whole x space); 
#trial and error based, the photo has long x axis but small y axis
#....rasterImage(image, xleft, ybottom, xright, ytop,)
rasterImage(image, 0,0,pixelWidth,pixelHeight) 
mtext("(a)", side = 3, line = -3, adj= 0.09, cex=1.0)
mtext("(b)", side = 3, line = -3, adj= 0.55, cex=1.0)
##For aggspectra (B)

library(pavo)
par(mar=c(5,4,1,1), mgp= c(2.5,1,0))
aggblue<- read.csv("Data/bluespec3.1.csv")
aggvlue <- as.rspec(aggblue) 

aggsmooth <- procspec(aggvlue, opt = 'smooth', span= 0.5)
group <- gsub('\\.[0-9].*$', '', names(aggsmooth))[-1]
table(group)
aggplot(aggsmooth, group, lwd= 3.0, legend = FALSE, 
        lcol= c("red", "orange","royalblue", "yellow", "black"), 
        shadecol= c("red", "orange", "royalblue", "yellow", "black"), 
        xlab= "Wavelength(nm)", ylab= "Reflactance(%)", 
        cex.lab = 1.5, cex.axis= 1.5)
legend("topleft", legend= c("Male S8",  "Female S8", 
                            "Background", "Male S7", "Female S7"),
       col=c("red", "orange","royalblue", 
             "yellow", "black"), lty=1, lwd= 2, cex=1.0, bty = "n")
mtext("(c)", side=3, line=0.5, adj=0, cex=1.0)


dev.off()










#####Figure 2 ####

pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 1.5
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_3.4.pdf", width=pageWidthLarge, 
    height= pageHeightLarge, family=fontFamily, paper=pagePaper)
#png("output/Figure_3.4.png", width=pageWidthLarge, height= pageHeightLarge,
  #  units= "in",type= "cairo", res = 600)

layout(matrix(c(1,2,3,4,5,6,7,8), nrow=2,ncol=4,byrow =  TRUE), heights = c(1,1))
par(oma=c(6,0,0,0))

blueFunctionData<- read.csv("Data/BlueFunctionDataset_2019.csv")

par(mar=c(2,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,1:2], 
        ylab=expression('Ds'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,
        col= c("lightpink4", "lavenderblush3"),las = 2)
#mtext(c("Male S8", "Female S8"), 
      #at= c(1, 2), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(a)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)


par(mar=c(2,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,5:6], 
        ylab=expression('DL'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,ylim = c(-0.5, 8),
        col= c("lightpink4", "lavenderblush3"),las = 2)
##mtext(c("Male S8", "Female S8"), 
      ##at= c(1, 2), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(b)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)


boxplot(blueFunctionData$tri_male_contrat_ds, blueFunctionData$tri_female_contrast_ds, 
        ylab=expression('Chromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,6), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 4.5, "*", cex=2.0)
##mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(c)", side=3, line=0.5, adj=0, cex=1.0)


boxplot(blueFunctionData$tri_male_contrat_dl, blueFunctionData$tri_female_contrast_dl, 
        ylab=expression('Achromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,18), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 16, "*", cex=2.0)
###mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(d)", side=3, line=0.5, adj=0, cex=1.0)


###
par(mar=c(2,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,9:10], 
        ylab=expression('Ds'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,
        col= c("lightpink4", "lavenderblush3"),las = 2)
##mtext(c("Male S8", "Female S8"), 
  ##    at= c(1, 2), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(e)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)



par(mar=c(2,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,13:14], 
        ylab=expression('DL'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,
        col= c("lightpink4", "lavenderblush3"),las = 2)
##mtext(c("Male S8", "Female S8"), 
  ##    at= c(1, 2), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(f)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)

par(mar=c(2,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData$tetra_male_contrat_ds, blueFunctionData$tetra_female_contrast_ds, 
        ylab=expression('Chromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,15), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 14, "*", cex=2.0)
##mtext(c("Male", "Female"), at= c(0.1, 0.4), side =1, las = 2, line = 1, cex = 1.5*par()$cex)
mtext("(g)", side=3, line=0.5, adj=0, cex=1.0)


par(mar=c(2,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData$tetra_male_contrat_dl, blueFunctionData$tetra_female_contrast_dl, 
        ylab=expression('Achromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,18), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 16, "*", cex=2.0)
##mtext(c("Male", "Female"), at= c(0.1, 0.4), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(h)", side=3, line=0.5, adj=0, cex=1.0)


###outer margin 

mtext("Male S8", at= 0.13 , side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

mtext("Female S8", at= 0.19 , side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

mtext("Male S8", at= 0.38, side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

mtext("Female S8", at= 0.44 , side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

mtext("Male", at= 0.63 , side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

mtext("Female", at= 0.69 , side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

mtext("Male", at= 0.87 , side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

mtext("Female", at= 0.94 , side =1 , las= 2, line = -1, 
      cex = 1.5*par()$cex, outer = TRUE)

dev.off()



######################################### Figure 3 ##########################################



pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 0.85
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_3.5.pdf", width=pageWidthLarge, 
    height= pageHeightLarge, family=fontFamily, paper=pagePaper)
#png("output/Figure_3.5.png", width=pageWidthLarge, height= pageHeightLarge,
    #units= "in",type= "cairo", res = 600)
layout(matrix(c(1,2,3,3,4,4,5,5), nrow=2,ncol=4,byrow =  TRUE))
par(lheight= 0.8)

FemalePreferance<- read.csv("Data/FemalePreferances_2019.csv")
FemalePreferance

##Tandem number bar plot

par(mar= c(6,4,4,2)+0.1, mgp= c(2.5,1,0))
barplot(c(51, 57),ylim= c(0, 60), las = 2.0, space = 0.6, col= c("indianred3", "rosybrown"), 
        ylab= "Number of tandem", cex.lab= 1.5, cex.axis = 1.5,
        names.arg = c("Control\nmale", "Manipulated\nmale"), cex.names = 1.5)
mtext("(a)", side = 3, line = 1, adj= 0.09, cex=1.0)

##Wheel number barplot

par(mar= c(6,4,4,2)+0.1, mgp= c(2.5,1,0))
barplot(c(23, 19), ylim= c(0, 25), space= 0.6, las = 2.0, col= c("indianred3", "rosybrown"), 
        ylab= "Number of wheel", cex.lab= 1.5,cex.axis = 1.5,
        names.arg = c("Control\nmale", "Manipulated\nmale"), cex.names = 1.5)

mtext("(b)", side = 3, line = 1, adj=0.09, cex=1.0)
##tandem duration plot

par(mar= c(6,4,4,2)+0.1, mgp= c(2.5,1,0))
boxplot(FemalePreferance$controlTandem, FemalePreferance$manipulatedTandem, 
        ylab= "Tandem duration (min)", cex.lab= 1.5,cex.axis = 1.5,
        names = FALSE, outline= FALSE, col= c("indianred3", "rosybrown"),
        boxwex=0.15,staplewex= 0.3, at= c(0.1,0.4), ylim= c(0,13), xlim= c(0, 0.5))
mtext(c("Control\nmale", "Manipulated\nmale"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(c)", side = 3, line = 1, adj=0.07, cex=1.0)


## Tandem2wheel duration plot

boxplot(FemalePreferance$controlTandem2wheel, FemalePreferance$manipulatedTandem2wheel, 
        ylab="Tandem to wheel duration (min)",cex.lab = 1.5,cex.axis = 1.5, 
        names = FALSE, outline= FALSE, col= c("indianred3", "rosybrown"), 
        boxwex=0.15,staplewex= 0.3, at= c(0.1,0.4), ylim= c(0,20), xlim= c(0, 0.5))
mtext(c("Control\nmale", "Manipulated\nmale"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(d)", side = 3, line = 1, adj=0.07, cex=1.0)

## Wheel duration plot 

boxplot(FemalePreferance$controlWheel, FemalePreferance$manipulatedWheel, 
        ylab="Wheel duration (min)", cex.lab = 1.5,cex.axis = 1.5, 
        names = FALSE, outline= FALSE, col= c("indianred3", "rosybrown"), 
        boxwex=0.15,staplewex= 0.3, at= c(0.1,0.4), ylim= c(2,12), xlim= c(0, 0.5))
mtext(c("Control\nmale", "Manipulated\nmale"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(e)", side = 3, line = 1, adj=0.07, cex=1.0)

dev.off()


##################################Figure 4##################################################


pageWidthSmall <- 3.14961
pageHeightSmall <- pageWidthSmall * 1.0
pagePaper <- 'special'
fontFamily <- 'Times'
png("output/Figure_3.6.png", width=pageWidthLarge, height= pageHeightLarge,
units= "in",type= "cairo", res = 600)

#pdf("output/Figure_3.6.pdf", width=pageWidthSmall, 
 #  height= pageHeightSmall, family=fontFamily, paper=pagePaper)


layout(matrix(c(1,2), nrow=1,ncol=2,byrow =  TRUE))
par(lheight= 0.8)
par(mar= c(6.5,3,1.5,1), mgp = c(1.2,0.5,0))
barplot(c(27, 13),ylim= c(0, 30), space = 0.6, las= 2.0, col= c("lightblue4", "gray54"), 
        ylab= "Number of tandem", cex.lab= 0.8, cex.axis = 0.8,
        names.arg = c("Control\nfemale", "S8-S9 Manipulated\nfemale"), cex.names = 0.8)
mtext("(a)", side = 3, line = 0.5, adj= 0.01, cex = 0.8) 

par(mar= c(6.5,3,1.5,1), mgp = c(1.2,0.5,0))
barplot(c(22, 18),ylim= c(0, 25), space = 0.6, las = 2.0, col= c("lightblue4", "gray54"), 
        ylab= "Number of tandem", cex.lab= 0.8, cex.axis = 0.8,
        names.arg = c("Control\nfemale", "S4 Manipulated\nfemale"), cex.names = 0.8)
mtext("(b)", side = 3, line = 0.5, adj= 0.01, cex= 0.8)

dev.off()






#################################Stats#####################

############################# Female preferances statistics #############

##histogram of the dataset
FemalePreferance <- read.csv("Data/FemalePreferances_2019.csv")

hist(FemalePreferance$controlTandem)
hist(FemalePreferance$manipulatedTandem)
hist(FemalePreferance$controlTandem2wheel)
hist(FemalePreferance$manipulatedTandem2wheel)
hist(FemalePreferance$controlWheel)
hist(FemalePreferance$manipulatedWheel)

##qqnorm plor and line of the dataset

qqnorm(FemalePreferance$controlTandem);qqline(FemalePreferance$controlTandem, col = 2)
qqnorm(FemalePreferance$manipulatedTandem);qqline(FemalePreferance$manipulatedTandem, col = 2)
qqnorm(FemalePreferance$manipulatedTandem2wheel);qqline(FemalePreferance$manipulatedTandem2wheel, col = 2)
qqnorm(FemalePreferance$controlWheel);qqline(FemalePreferance$controlWheel, col = 2)

#qqnorm and qqline suggest tandem and tandem to wheel data are not normally distributed however wheel data are normally distributed


##shapiro.test of the dataset

shapiro.test(FemalePreferance$controlTandem)
shapiro.test(FemalePreferance$manipulatedTandem)
shapiro.test(FemalePreferance$controlTandem2wheel)
shapiro.test(FemalePreferance$manipulatedTandem2wheel)
shapiro.test(FemalePreferance$controlWheel)
shapiro.test(FemalePreferance$manipulatedWheel)

#shapiro.test confirm tandem and tandem to wheel data are not normally distributed however wheel data are normally distributed

### variation betwwen groups: wheel formation F test as data are normally distributed
#the null hypothesis of the test is that there is no difference of variance between two compared groups
#if p value is less than 0.05 there is significant difference of variance between the two compared groups

var.test(FemalePreferance$controlWheel,FemalePreferance$manipulatedWheel)


### As the tandem duration and tandem to wheel duratrion are not equally distributed we will follow Mann Whitney U test
##unpair wilcox.test is Mann whitney U test. by default the wilco.test is unpair

wilcox.test(FemalePreferance$controlTandem, FemalePreferance$manipulatedTandem)
wilcox.test(FemalePreferance$controlTandem2wheel, FemalePreferance$manipulatedTandem2wheel)

###As wheel duration follow normal distribution and equal varience we will follow two sample t-test
t.test(FemalePreferance$controlWheel, FemalePreferance$manipulatedWheel, var.equal = TRUE)
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

R1 = c(51, 57)
R2 = c(57, 51) 

data = matrix(c(R1, R2), nrow= 2, byrow=TRUE)

rownames(data) = c("control", "manipulated")          
colnames(data) = c("mate", "nomate")

data

chisq.test(data, correct = FALSE)

########################wheel blue no blue #############
R1 = c(23, 28)
R2 = c(19, 38) 

data = matrix(c(R1, R2), nrow= 2, byrow=TRUE)

rownames(data) = c("control", "manipulated")          
colnames(data) = c("wheel", "nowheel")

data

chisq.test(data, correct = FALSE)


##############Discriminability stat ###############



blueFunctionData <- read.csv("Data/BlueFunctionDataset_2019.csv")
View(blueFunctionData)

############Tri chromatic vision cdiscriminibility ###########################

##To calculate if the data is normally distributed we need to perfom qqnorm plot and shapiro.test

hist(blueFunctionData$tri_maleblue_ds)
hist(blueFunctionData$tri_femaleblue_ds)
hist(blueFunctionData$tri_maleblue_dl)
hist(blueFunctionData$tri_femaleblue_dl)


qqnorm(blueFunctionData$tri_maleblue_ds);qqline(blueFunctionData$tri_maleblue_dl, col = 2)
qqnorm(blueFunctionData$tri_femaleblue_ds);qqline(blueFunctionData$tri_femaleblue_dl, col = 2)

shapiro.test(blueFunctionData$tri_maleblue_ds)
shapiro.test(blueFunctionData$tri_femaleblue_ds)

qqnorm(blueFunctionData$tri_male_contrat_dl);qqline(blueFunctionData$tri_male_contrat_dl, col = 2)
qqnorm(blueFunctionData$tri_female_contrast_dl);qqline(blueFunctionData$tri_female_contrast_dl, col = 2)

shapiro.test(blueFunctionData$tri_maleblue_dl)
shapiro.test(blueFunctionData$tri_femaleblue_dl)

##The null hypothesis of shapiro.test is that the data are normally distributed
#If p valus of shapiro.test is less than 0.05 then data are not normally distributed 
#As the data are not normally distributed we will follow Mann Whitney U test
##unpair wilcox.test is Mann whitney U test. by default the wilco.test is unpair

wilcox.test(blueFunctionData$tri_maleblue_ds, blueFunctionData$tri_femaleblue_ds)
wilcox.test(blueFunctionData$tri_maleblue_dl, blueFunctionData$tri_femaleblue_dl)



#############Tri chromatic vision contrast###########################

##To calculate if the data is normally distributed we need to perfom qqnorm plot and shapiro.test

hist(blueFunctionData$tri_male_contrat_ds)
hist(blueFunctionData$tri_female_contrast_ds)
hist(blueFunctionData$tri_male_contrat_dl)
hist(blueFunctionData$tri_female_contrast_dl)


qqnorm(blueFunctionData$tri_male_contrat_ds);qqline(blueFunctionData$tri_male_contrat_ds, col = 2)
qqnorm(blueFunctionData$tri_female_contrast_ds);qqline(blueFunctionData$tri_female_contrast_ds, col = 2)

shapiro.test(blueFunctionData$tri_male_contrat_ds)
shapiro.test(blueFunctionData$tri_female_contrast_ds)

qqnorm(blueFunctionData$tri_male_contrat_dl);qqline(blueFunctionData$tri_male_contrat_dl, col = 2)
qqnorm(blueFunctionData$tri_female_contrast_dl);qqline(blueFunctionData$tri_female_contrast_dl, col = 2)

shapiro.test(blueFunctionData$tri_male_contrat_dl)
shapiro.test(blueFunctionData$tri_female_contrast_dl)

##The null hypothesis of shapiro.test is that the data are normally distributed
#If p valus of shapiro.test is less than 0.05 then data are not normally distributed 
#As the data are not normally distributed we will follow Mann Whitney U test
##unpair wilcox.test is Mann whitney U test. by default the wilco.test is unpair

wilcox.test(blueFunctionData$tri_male_contrat_ds, blueFunctionData$tri_female_contrast_ds)
wilcox.test(blueFunctionData$tri_male_contrat_dl, blueFunctionData$tri_female_contrast_dl)





#############Tetrachromatic vision discriminability ###########################

##To calculate if the data is normally distributed we need to perfom qqnorm plot and shapiro.test

hist(blueFunctionData$tetra_male_contrat_ds)
hist(blueFunctionData$tetra_female_contrast_ds)
hist(blueFunctionData$tetra_male_contrat_dl)
hist(blueFunctionData$tetra_female_contrast_dl)


qqnorm(blueFunctionData$tetra_male_contrat_ds);qqline(blueFunctionData$tetra_male_contrat_ds, col = 2)
qqnorm(blueFunctionData$tetra_female_contrast_ds);qqline(blueFunctionData$tetra_female_contrast_ds, col = 2)

shapiro.test(blueFunctionData$tetra_male_contrat_ds)
shapiro.test(blueFunctionData$tetra_female_contrast_ds)

qqnorm(blueFunctionData$tri_male_contrat_dl);qqline(blueFunctionData$tri_male_contrat_dl, col = 2)
qqnorm(blueFunctionData$tri_female_contrast_dl);qqline(blueFunctionData$tri_female_contrast_dl, col = 2)

shapiro.test(blueFunctionData$tetra_male_contrat_dl)
shapiro.test(blueFunctionData$tetra_female_contrast_dl)

##The null hypothesis of shapiro.test is that the data are normally distributed
#If p valus of shapiro.test is less than 0.05 then data are not normally distributed 
#As the data are not normally distributed we will follow Mann Whitney U test
##unpair wilcox.test is Mann whitney U test. by default the wilco.test is unpair

wilcox.test(blueFunctionData$tetra_male_contrat_ds, blueFunctionData$tetra_female_contrast_ds)
wilcox.test(blueFunctionData$tetra_male_contrat_dl, blueFunctionData$tetra_female_contrast_dl)


################# tetra chromatic vision contrast ###########


hist(blueFunctionData$tetra_male_contrat_ds)
hist(blueFunctionData$tetra_female_contrast_ds)
hist(blueFunctionData$tetra_male_contrat_dl)
hist(blueFunctionData$tetra_female_contrast_dl)


qqnorm(blueFunctionData$tetra_male_contrat_ds);qqline(blueFunctionData$tetra_male_contrat_ds, col = 2)
qqnorm(blueFunctionData$tetra_female_contrast_ds);qqline(blueFunctionData$tetra_female_contrast_ds, col = 2)

shapiro.test(blueFunctionData$tetra_male_contrat_ds)
shapiro.test(blueFunctionData$tetra_female_contrast_ds)

qqnorm(blueFunctionData$tetra_male_contrat_dl);qqline(blueFunctionData$tetra_male_contrat_dl, col = 2)
qqnorm(blueFunctionData$tetra_female_contrast_dl);qqline(blueFunctionData$tetra_female_contrast_dl, col = 2)

shapiro.test(blueFunctionData$tetra_male_contrat_dl)
shapiro.test(blueFunctionData$tetra_female_contrast_dl)

##The null hypothesis of shapiro.test is that the data are normally distributed
#If p valus of shapiro.test is less than 0.05 then data are not normally distributed 
#As the data are not normally distributed we will follow Mann Whitney U test
##unpair wilcox.test is Mann whitney U test. by default the wilco.test is unpair

wilcox.test(blueFunctionData$tetra_male_contrat_ds, blueFunctionData$tetra_female_contrast_ds)
wilcox.test(blueFunctionData$tetra_male_contrat_dl, blueFunctionData$tetra_female_contrast_dl)



#######################stats of the female preferances ########













##############Figure 2##############


pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 1.5
pagePaper <- 'special'
fontFamily <- 'Times'
png("output/Figure_3.2.png", width=pageWidthLarge, height= pageHeightLarge,
    units= "in",type= "cairo", res = 600)

layout(matrix(c(1,2,3,4), nrow=2,ncol=2,byrow =  TRUE), heights = c(1,1))


blueFunctionData<- read.csv("Data/BlueFunctionDataset_2019.csv")

par(mar=c(7,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,1:4], 
        ylab=expression('Chromatic contrast'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,
        col= c("lightpink4", "lavenderblush3"),las = 2)
mtext(c("Male S8", "Female S8", "Male S7", "Female S7"), 
      at= c(1, 2, 3, 4), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(a)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)


par(mar=c(7,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,5:8], 
        ylab=expression('Achromatic contrast'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,ylim = c(-0.5, 8),
        col= c("lightpink4", "lavenderblush3"),las = 2)
mtext(c("Male S8", "Female S8", "Male S7", "Female S7"), 
      at= c(1, 2, 3, 4), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(b)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)


boxplot(blueFunctionData$tri_male_contrat_ds, blueFunctionData$tri_female_contrast_ds, 
        ylab=expression('Chromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,6), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 4.5, "*", cex=2.0)
mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(c)", side=3, line=0.5, adj=0, cex=1.0)


boxplot(blueFunctionData$tri_male_contrat_dl, blueFunctionData$tri_female_contrast_dl, 
        ylab=expression('Achromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,18), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 16, "*", cex=2.0)
mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(d)", side=3, line=0.5, adj=0, cex=1.0)

dev.off()




##############Figure 3##############


pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 1.5
pagePaper <- 'special'
fontFamily <- 'Times'
png("output/Figure_3.3.png", width=pageWidthLarge, height= pageHeightLarge,
    units= "in",type= "cairo", res = 600)

layout(matrix(c(1,2,3,4), nrow=2,ncol=2,byrow =  TRUE), heights = c(1,1))


blueFunctionData<- read.csv("Data/BlueFunctionDataset_2019.csv")

par(mar=c(7,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,9:12], 
        ylab=expression('Chromatic contrast'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,
        col= c("lightpink4", "lavenderblush3"),las = 2)
mtext(c("Male S8", "Female S8", "Male S7", "Female S7"), 
      at= c(1, 2, 3, 4), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(a)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)



par(mar=c(7,4,2,1), mgp= c(2.5,1,0))
boxplot(blueFunctionData[,13:16], 
        ylab=expression('Achromatic contrast'),
        names = FALSE, outline= FALSE, 
        cex.lab= 1.5,cex.axis= 1.5,
        col= c("lightpink4", "lavenderblush3"),las = 2)
mtext(c("Male S8", "Female S8", "Male S7", "Female S7"), 
      at= c(1, 2, 3, 4), side =1, las= 2, line = 1, cex = 1.5*par()$cex)
mtext("(b)", side=3, line=0.5, adj=0, cex=1.0)
abline(h= 0, col= "black", lwd= 2.0, lty= 2)

boxplot(blueFunctionData$tetra_male_contrat_ds, blueFunctionData$tetra_female_contrast_ds, 
        ylab=expression('Chromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,15), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 14, "*", cex=2.0)
mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(c)", side=3, line=0.5, adj=0, cex=1.0)



boxplot(blueFunctionData$tetra_male_contrat_dl, blueFunctionData$tetra_female_contrast_dl, 
        ylab=expression('Achromatic contrast'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("red", "blue"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,18), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 16, "*", cex=2.0)
mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(d)", side=3, line=0.5, adj=0, cex=1.0)

dev.off()







