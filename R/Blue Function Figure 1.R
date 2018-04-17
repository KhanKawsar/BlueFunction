########################Figure 1##########################################################



pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 1.5
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_1.pdf", width=pageWidthLarge, height= pageHeightLarge, family=fontFamily, paper=pagePaper)

layout(matrix(c(1,1,2,2,3,4), nrow=3,ncol=2,byrow =  TRUE), heights = c(0.6,1,1))


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
aggblue<- read.csv("Data/aggblue.csv")
aggvlue <- as.rspec(aggblue) 
aggsmooth <- procspec(aggvlue, opt = 'smooth', span= 0.5)
names<- names(aggsmooth)
bysic1 <- gsub("^sample[0-9].",'', names)
aggplot(aggsmooth, bysic1, lwd= 2, lcol= c("red", "royalblue", "black", "seagreen"), 
        shadecol= c("red", "royalblue", "black", "green"), 
        xlab= "Wavelength(nm)", ylab= "Reflactance(%)", cex.lab = 1.5, cex.axis= 1.5)
mtext("(c)", side=3, line=0.5, adj=0, cex=1.0)
#Fox chromatic and achratic boxplot (C-D)

blueFunctionData<- read.csv("Data/blueFunctionData.csv")

boxplot(blueFunctionData$chromaticMale, blueFunctionData$chromaticFemale, 
        ylab=expression('Chromatic discriminability (D'['S']*')'), 
        names = FALSE, outline= FALSE, cex.lab= 1.5,cex.axis= 1.5,
        col= c("lightpink4", "lavenderblush3"), boxwex=0.15,staplewex= 0.3, 
        at= c(0.1,0.4), ylim= c(0,0.6), xlim= c(0, 0.5), mgp= c(2.5,1,0))
text(0.25, 0.55, "*", cex=2.0)
mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(d)", side=3, line=0.5, adj=0, cex=1.0)


boxplot(blueFunctionData$achromaticMale, blueFunctionData$achromaticFemale, 
        ylab=expression('Achromatic discriminability (D'['L']*')'), names = FALSE, outline= FALSE, 
        col= c("lightpink4", "lavenderblush3"), cex.lab = 1.5,cex.axis= 1.5,
        boxwex=0.15,staplewex= 0.3, at= c(0.1,0.4), ylim= c(0,0.8), xlim= c(0, 0.5), mgp= c(2.5,1,0))
mtext(c("male", "female"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(e)", side=3, line=0.5, adj=0, cex=1.0)

dev.off()
