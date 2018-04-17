######################################### Figure 2 ##########################################



pageWidthLarge<- 7.08661
pageHeightLarge <- paperWidthLarge * 0.85
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_2.pdf", width=pageWidthLarge, height= pageHeightLarge, family=fontFamily, paper=pagePaper)

layout(matrix(c(1,2,3,3,4,4,5,5), nrow=2,ncol=4,byrow =  TRUE))
par(lheight= 0.8)

##Tandem number bar plot

par(mar= c(6,4,4,2)+0.1, mgp= c(2.5,1,0))
barplot(c(31, 33),ylim= c(0, 35), las = 2.0, space = 0.6, col= c("indianred3", "rosybrown"), 
        ylab= "Number of tandem", cex.lab= 1.5, cex.axis = 1.5,
        names.arg = c("Control\nmale", "Manipulated\nmale"), cex.names = 1.5)
mtext("(a)", side = 3, line = 1, adj= 0.09, cex=1.0)

##Wheel number barplot

par(mar= c(6,4,4,2)+0.1, mgp= c(2.5,1,0))
barplot(c(17, 14), ylim= c(0, 20), space= 0.6, las = 2.0, col= c("indianred3", "rosybrown"), 
        ylab= "Number of wheel", cex.lab= 1.5,cex.axis = 1.5,
        names.arg = c("Control\nmale", "Manipulated\nmale"), cex.names = 1.5)

mtext("(b)", side = 3, line = 1, adj=0.09, cex=1.0)
##tandem duration plot

par(mar= c(6,4,4,2)+0.1, mgp= c(2.5,1,0))
boxplot(blueFunctionData$controlTandem, blueFunctionData$manipulatedTandem, 
        ylab= "Tandem duration (min)", cex.lab= 1.5,cex.axis = 1.5,
        names = FALSE, outline= FALSE, col= c("indianred3", "rosybrown"),
        boxwex=0.15,staplewex= 0.3, at= c(0.1,0.4), ylim= c(0,13), xlim= c(0, 0.5))
mtext(c("Control\nmale", "Manipulated\nmale"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(c)", side = 3, line = 1, adj=0.07, cex=1.0)


## Tandem2wheel duration plot

boxplot(blueFunctionData$controlTandem2wheel, blueFunctionData$manipulatedTandem2wheel, 
        ylab="Tandem to wheel duration (min)",cex.lab = 1.5,cex.axis = 1.5, 
        names = FALSE, outline= FALSE, col= c("indianred3", "rosybrown"), 
        boxwex=0.15,staplewex= 0.3, at= c(0.1,0.4), ylim= c(0,18), xlim= c(0, 0.5))
mtext(c("Control\nmale", "Manipulated\nmale"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(d)", side = 3, line = 1, adj=0.07, cex=1.0)

## Wheel duration plot 

boxplot(blueFunctionData$controlWheel, blueFunctionData$manipulatedWheel, 
        ylab="Wheel duration (min)", cex.lab = 1.5,cex.axis = 1.5, 
        names = FALSE, outline= FALSE, col= c("indianred3", "rosybrown"), 
        boxwex=0.15,staplewex= 0.3, at= c(0.1,0.4), ylim= c(2,12), xlim= c(0, 0.5))
mtext(c("Control\nmale", "Manipulated\nmale"), at= c(0.1, 0.4), side =1, line = 2, cex = 1.5*par()$cex)
mtext("(e)", side = 3, line = 1, adj=0.07, cex=1.0)
dev.off()