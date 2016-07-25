# Boyack response

# read in data
library(readxl)
d <- read_excel("journal.pbio.1002501.s001.xlsx")

# assume NP6013 is "NP" from paper
colnames(d)[9] <- "NP"

# calculate CPP (citations per paper)
d$CPP <- d$NC / d$NP

# Comment from Kevin Boyack:
# I used log-transformed values for np, h, hm, s, sf, sfl for the correlations
# In(n+1)/ln(max(n)+1) for each of the 6 original indicators puts them each within a [0,1] range. 
# The sum of these gives the composite value. 

# transform
idx <- c("H","Hm","NS","NSF", "NSFL","NP","CPP")
maxs <- apply(d[,idx], 2, max)

orig.d <- d
for (index in idx) {
	d[,index] <- log(d[,index]+1) / log(maxs[index]+1)
}

# set up index and field vectors
indices <- c("H","Hm","NS","NSF","NSFL","NP","CPP","COMP") 
fields  <- c("ALL","PHYS","MATH","CS","CHEM","ENG","EARTH","BIO","INFDIS","MED","BRAIN","HEALTH","SOC")

# create data frame for results
results <- matrix(rep(0,length(indices)*length(fields)), nrow=length(indices))
rownames(results) <- indices
colnames(results) <- fields
results <- as.data.frame(results, stringsAsFactors=FALSE)

# iterate over fields
for (field in fields) {

    # field specific data frame
    sd <- d[d$Field==field,]

    # iterate over indices and calculate cor with NC
    for (index in indices) {
        results[index,field] <- cor(sd[,index],sd[,"NC"], method="spearman")
    }

}

# add "ALL" data
for (index in indices) {
    results[index, "ALL"] <- cor(d[,index],d[,"NC"])
}

# write data
write.csv(results, "boyack_results_pearson.csv", row.names=FALSE)

# try and recreate PLOS plot

# get some colours
cols <- colors()[c(30, 95, 601, 450, 26, 109, 61, 81, 32, 404, 148, 505, 4)]

png("boyack_results_pearson.png", width=1200, height=800)
par(mar=c(4,3,1,0))
bp <- barplot(as.matrix(t(results)), beside=TRUE, col=cols, ylim=c(-1,1), border=NA)

# vertical lines
for (x in bp[13,]+1) {
    lines(c(x,x),c(-1,1), col="grey")
}

# horizontal lines
for (y in seq(-1,1,by=0.2)) {
    lines(c(0,112), c(y,y), col="grey")
}

# redraw
barplot(as.matrix(t(results)), beside=TRUE, col=cols, ylim=c(-1,1), border=NA, add=TRUE)
dev.off()
