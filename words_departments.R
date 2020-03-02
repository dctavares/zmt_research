#script for plots presented at the talk at ZMT
#Date: 27.02.2020
#Author: Davi Castro Tavares

#plotting a word cloud to represent the most common methods used in my peer reviewed publications
require(wordcloud)
research_zmt = read.table("research_zmt.csv", sep = ";", header = TRUE)#loading the dataset with titles and keywords of papers per workgroup.
research_zmt$merged = paste(research_zmt[,4],research_zmt[,5])#creating a new column merging words from titles and keywords for producing the word plot
wordcloud(research_zmt[,6], min.freq = 6, scale = c(2.4,0.5), colors=brewer.pal(8, "Dark2"))#plotting the wordcloud based on words recorded on titles and keywords, which are merged in the new column number 6.


#creating table of frequencies for words and plotting a ordination diagram comparing the similarity of words among different departements. The Bray-Curtis similarity index is used because it fits well to count data and it distributes weights well among rare and abundant words.

library(splitstackshape)
df3 <- cSplit(research_zmt, "merged", sep = " ", direction = "long")#separating words from titles and keywords (merged column number 6) in different rows
df3 = data.frame(df3)#transforming the frequency table in a data frame
tbl <- table(df3$merged, df3$department)
res <- as.data.frame.matrix(tbl)
require(vegan)
nmds = metaMDS(res, distance = "bray")#fitting the NMDS

#plotting the NMDS. For changing colours, and words, you can check: https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/ordiplot.
fig <- ordiplot(nmds, type = "none",ylim = c(-1.2,1.6), xlim = c(-1.8,1.8))
points(fig, "species", pch=21, bg="yellow")#
text(fig, "species", col="blue", cex=0.9)



