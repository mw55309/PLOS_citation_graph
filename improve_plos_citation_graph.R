# assumes we have a data.frame "results" from https://github.com/mw55309/PLOS_citation_graph/blob/master/README.md

# get a better palette from RColorBrewer
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(13, "Dark2"))(13)

png("improved_citation_plot.png", height=1200, width=800)

# 4 rows, 2 columns
split.screen(c(4,2))

# iterate over each row
for (i in 1:nrow(results)) {

	# select panel and set parameters
	screen(i)
	par(mar=c(4,2.5,2,1), las=2, cex.lab=0.5)

	# draw initial barplot
	bx <- barplot(as.numeric(results[i,]), col=cols, xaxt="n", yaxt="n", ylim=c(-1,1), border=NA, main="")

	# add titles and axes
	title(main=rownames(results)[i], cex.main=2)
	axis(side=1, at=bx[,1], labels=colnames(results), lty=0, cex.axis=1, line=-0.5)
	axis(side=2, at=seq(-1,1,by=0.4), labels=seq(-1,1,by=0.4), cex.axis=1)


	# vertical lines
	for (x in bx[,1]) {
   		lines(c(x,x),c(-1,1), col="grey")
	}

	# horizontal lines
	for (y in seq(-1,1,by=0.2)) {
    		lines(c(0,112), c(y,y), col="grey")
	}

	# redraw barplot
	barplot(as.numeric(results[i,]), col=cols, xaxt="n", yaxt="n", ylim=c(-1,1), border=NA, main="", add=TRUE)

}

# close and save image
close.screen(all=TRUE)
dev.off()
