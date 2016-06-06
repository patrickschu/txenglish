#install.packages(c("vowels", "ggplot2", "lme4"))

library(vowels)
library(ggplot2)
library(lme4)


test=function(thing, ...)
{
t=data.frame('vowel'=1, 'spast'=100)

one=substitute(list(...));
	y=as.character(one[-1L]);
colnames(t)=y
print(y);
print (length(y));
print (t);
	print (length(deparse(one)));
	print (deparse(one));
	#r=unlist(one);
	#print (r)
	
}

test(vollassi, superassi, spast)



ds=read.csv('/Users/ps22344/Desktop/canada_final_1121.csv');
# ds=read.csv('C:\\Users\\ps\\My Documents\\Github\\canadavowels\\canada_final_1121.csv')
print (nrow(ds));

ggvowels=function(dataset, vowel, F1, F2,  ..., plot_title="") 

{
cat ("\n\n\nSTART");

#deparsing for further use
vowel=deparse(substitute(vowel));
F1=deparse(substitute(F1));
F2=deparse(substitute(F2));
user_vars=substitute(list(...));
header="\n------\n"

#summary of dataset
vowels=levels(dataset[[vowel]]);
#cat (sprintf ("%sWe are working with %d vowel(s):\n", header, length(vowels)))
print (levels(dataset[[vowel]]));
#cat (sprintf ('%sWe are working with %s independent variable(s):\n', header, length(deparse(list(...)))));

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
	#print (str(totaldata));
		#this is dangerous, but good enough for now
	
	outputframe=rbind(outputframe, totaldata);
	
	}
names(outputframe)=names(totaldata) ;
print (outputframe);	
##PLOTTING
##do we make a do.call??
meanies=aggregate(list('F1means'=outputframe[['F1means']],'F2means'=outputframe[['F2means']]),list('VOWEL'=outputframe[[vowel]]), mean);
print (meanies);
user_vars_list=as.list(as.character(user_vars[-1L]))

if (length(user_vars_list) == 1)
{
for (entry in user_vars_list)
	{
	print (entry);
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
	ggsave(paste(plot_title,"_",entry,"_",next_entry, ".png", sep=""), width=8, height=5);
	}	
}

if (length(user_vars_list) > 1)
{
for (entry in user_vars_list[-length(user_vars_list)])
	{
	print (entry);
	#print (as.list(as.character(user_vars[-1L]))[1])
	entry_index=match(entry, as.list(as.character(user_vars[-1L])))
	print (entry_index)
	next_entry=as.character(user_vars[-1L])[entry_index+1];
	print (next_entry);
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
	}
}	

}


ggvowels(ds, VOWEL, F1labov, F2labov, AgeGrp, LOCATION, GENDER,plot_title="ASSIKIND")


#take dataset plus variables, calculate all interactions plus plot
#how to set plot settings 


