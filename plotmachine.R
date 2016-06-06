#install.packages(c("vowels", "ggplot2", "lme4"))

library(vowels)
library(ggplot2)
library(lme4)


plotmachine=function(dataset, vowel, F1, F2,  ..., plot_title="") 

{
cat ("\n\n\nSTART");

#deparsing for further use
vowel=deparse(substitute(vowel));
F1=deparse(substitute(F1));
F2=deparse(substitute(F2));
user_vars=substitute(list(...));
user_vars_list=as.list(as.character(user_vars[-1L]));
header="\n------\n"

#summary of dataset
vowels=levels(dataset[[vowel]]);
cat (sprintf ("%sWe are working with %d vowel(s):\n", header, length(vowels)))
print (levels(dataset[[vowel]]));
cat (sprintf ('%sWe are working with %s independent variable(s):\n', header, length(user_vars_list)));
for (variable in user_vars_list){print (variable)};
cat(header);
outputframe=data.frame(as.list(as.character(user_vars[-1L])),"F1means", "F1sd", "F2means", "F2sd", vowel, check.names=FALSE)[character(0),];

#print (str(outputframe));

##CREATING DATA
for (v in vowels){
	print (v);
	voweldataset=dataset[dataset[[vowel]]==v,];
	print (paste("Tokens:", nrow(voweldataset)));
	indi_variables=eval(substitute(list(...)), voweldataset);
	#computing means
	F1means=with(voweldataset, aggregate(F1, indi_variables, mean));
	colnames(F1means)=c(as.character(user_vars[-1L]), "F1means");

	F1sd=with(voweldataset, aggregate(F1, indi_variables, sd));
	colnames(F1sd)=c(as.character(user_vars[-1L]), "F1sd");

	F2means=with(voweldataset, aggregate(F2, indi_variables, mean));
	colnames(F2means)=c(as.character(user_vars[-1L]), "F2means");

	F2sd=with(voweldataset, aggregate(F2, indi_variables, sd));
	colnames(F2sd)=c(as.character(user_vars[-1L]), "F2sd");
	
	#print (F2sd);
	#print (length(F2sd));
	#nicer alternatives here: http://stackoverflow.com/questions/14096814/r-merging-a-lot-of-data-frames

	F1data=merge(F1means, F1sd, by=as.character(user_vars[-1L]));
	F2data=merge(F2means, F2sd, by=as.character(user_vars[-1L]));
	totaldata=merge(F1data, F2data, by=as.character(user_vars[-1L]))
	totaldata[,vowel]=v;
	outputframe=rbind(outputframe, totaldata);
	
	}
#this is dangerous, but good enough for now	
names(outputframe)=names(totaldata) ;
cat(sprintf("%sFormant means\n", header));
print (outputframe);	



meanies=aggregate(list('F1means'=outputframe[['F1means']],'F2means'=outputframe[['F2means']]),list('VOWEL'=outputframe[[vowel]]), mean);
cat(sprintf("%sPopulation means\n", header));
print (meanies);
##PLOTTING

if (length(user_vars_list) == 1)
{
for (entry in user_vars_list)
	{
	canvas=ggplot(outputframe, aes(F2means, F1means));
	canvas=
	canvas+
	theme(legend.title=element_blank())+
	scale_y_reverse()+
	scale_x_reverse()+
	geom_text(data=meanies, aes(F2means, F1means, label=VOWEL))+
	geom_point(aes(colour=outputframe[[entry]]), size=4.5)+
	labs(title =plot_title) 
	print(canvas);
	ggsave(paste(plot_title,"_",entry, ".png", sep=""), width=8, height=5);
	cat (sprintf("%sPlot %s saved\n", header,paste(plot_title,"_",entry,"_",next_entry, ".png", sep="")));
	}	
}

if (length(user_vars_list) > 1)
{
for (entry in user_vars_list[-length(user_vars_list)])
	{
	entry_index=match(entry, as.list(as.character(user_vars[-1L])))
	next_entry=as.character(user_vars[-1L])[entry_index+1];
	canvas=ggplot(outputframe, aes(F2means, F1means));
	canvas=
	canvas+
	theme(legend.title=element_blank())+
	scale_y_reverse()+
	scale_x_reverse()+
	geom_text(data=meanies, aes(F2means, F1means, label=VOWEL))+
	geom_point(aes(colour=outputframe[[entry]], shape=outputframe[[next_entry]]), size=4.5)+
	labs(title =plot_title) 
	print(canvas);
	ggsave(paste(plot_title,"_",entry,"_",next_entry, ".png", sep=""), width=8, height=5);
	cat (sprintf("%s Plot %s saved", header, paste(plot_title,"_",entry,"_",next_entry, ".png", sep="")));
	}
}	

}




