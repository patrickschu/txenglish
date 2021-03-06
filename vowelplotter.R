
library(ggplot2)
install.packages('ggplot2')


#this needs to take number of differentiators, then output plots
ggvowels=function(dataset, title, normalized_F1, normalized_F2, separator=...)
{
# we turn aorund the levels of AgeGrp so we can read the plot better
dataset[["AgeGrp"]]=factor(dataset[["AgeGrp"]], c("Younger", "Middle", "Older"));

#these are the levels we iterate over
seps=levels(dataset[[separator]]);

#we set up a dataframe to fill with stuff
totalmeanies=data.frame(
VOWEL=character(), 
F1means=numeric(), 
F2means=numeric(), 
SEPARATOR=character(),
F1sd=numeric(),
F2sd=numeric()
);
# a second dataframe with global means for labeling purposes
labelf1=tapply(dataset[[normalized_F1]], dataset[["VOWEL"]], mean);
labelf2=tapply(dataset[[normalized_F2]], dataset[["VOWEL"]], mean);
labelmeans=data.frame(
VOWEL=names(labelf1), F1=labelf1, F2=labelf2)

#we're done with the dataframe and set up the plot
gg=ggplot(data=dataset, aes_string(x="F2", y="F1"));

#we collect means for each group in the dataframe we set up above
for (i in seps){ 
print (i);
#we construct a dataset for each separator
subseti=dataset[dataset[,separator]==i,];
#print(summary(subseti[subseti$VOWEL=="KIT",])); 


#with the tapplys, we calculate the means and sds for every vowel
averagef1=tapply(subseti[[normalized_F1]], subseti[["VOWEL"]], mean);
averagef2=tapply(subseti[[normalized_F2]], subseti[["VOWEL"]], mean);
stdevf1=tapply(subseti[[normalized_F1]], subseti[["VOWEL"]], sd);
stdevf2=tapply(subseti[[normalized_F2]], subseti[["VOWEL"]], sd);
#we put the whole shebang in a dataframe
#print(averagef1);
#print (averagef2);
meanies=data.frame(
VOWEL=names(averagef1), F1means=averagef1, F2means=averagef2, SEPARATOR=i, F1sd=stdevf1, F2sd=stdevf2);
totalmeanies=rbind(meanies, totalmeanies);
}
write.csv(totalmeanies);
##PLOTTING##
gg=ggplot(data=dataset, aes_string(x="F2", y="F1"));
gg+
scale_y_reverse()+
scale_x_reverse()+
theme_classic()+
#we try to add points for the mean for each vowel, just for labeling purposes
geom_text(data=labelmeans, aes(x=F2, y=F1, label=VOWEL), size=3)+
#we add actual datapoints
geom_point(data=totalmeanies, aes(x=F2means, y=F1means, colour=SEPARATOR, label=VOWEL, group=VOWEL), size=6)+
#ah! we need a title
ggtitle(paste(title, "\n"));
ggsave(paste(title,"_",separator,"_",i, ".png", sep=""), width=8, height=5);

return(totalmeanies)
}

cc=ggvowels(can, "The Canadian Shift", "F1labovstandard", "F2labovstandard", "AgeGrp")
