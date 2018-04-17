##################################Figure 3##################################################


pageWidthSmall <- 3.14961
pageHeightSmall <- pageWidthSmall * 1.0
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_3.pdf", width=pageWidthSmall, height= pageHeightSmall, family=fontFamily, paper=pagePaper)



layout(matrix(c(1,2), nrow=1,ncol=2,byrow =  TRUE))
par(lheight= 0.8)
par(mar= c(6.5,3,1.5,1), mgp = c(1.2,0.5,0))
barplot(c(27, 13),ylim= c(0, 30), space = 0.6, las= 2.0, col= c("gray8", "gray24"), 
        ylab= "Number of tandem", cex.lab= 0.8, cex.axis = 0.8,
        names.arg = c("Control\nfemale", "S8-S9 Manipulated\nfemale"), cex.names = 0.8)
mtext("(a)", side = 3, line = 0.5, adj= 0.01, cex = 0.8)

par(mar= c(6.5,3,1.5,1), mgp = c(1.2,0.5,0))
barplot(c(22, 18),ylim= c(0, 25), space = 0.6, las = 2.0, col= c("gray8", "gray24"), 
        ylab= "Number of tandem", cex.lab= 0.8, cex.axis = 0.8,
        names.arg = c("Control\nfemale", "S4 Manipulated\nfemale"), cex.names = 0.8)
mtext("(b)", side = 3, line = 0.5, adj= 0.01, cex= 0.8)
dev.off()
