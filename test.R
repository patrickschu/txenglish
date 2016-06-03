

test=function(thing, ...)
{
	one=as.list(substitute(list(...)))[-1L];
	print (str(one));
	#two=list(...)
}
r=test(var1=ETHNICITY, var2=GENDER, var3=ALL);





ds=read.csv('/Users/ps22344/Desktop/canada_final_1121.csv');
print (nrow(ds));

ggvowels=function(dataset, vowel, F1, F2,  ...) 

{
cat ("\n\n\nSTART");	
vowel=deparse(substitute(vowel));
F1=deparse(substitute(F1));
F2=deparse(substitute(F2));
indi_variables=as.list(substitute(list(...)))[-1L];
header="\n------\n"
#summary of dataset
vowels=levels(dataset[[vowel]]);
cat (sprintf ("%sWe are working with %d vowel(s):\n", header, length(vowels)));
print (levels(dataset[[vowel]]));
print (class(indi_variables));
cat (sprintf ('%sWe are working with %s independent variable(s):\n', header, length(indi_variables)));
print(str(quote(indi_variables)));
for (var in indi_variables)
 	{
	 print (var)
	 }
totalmeanies=data.frame(
VOWEL=character(), 
F1means=numeric(), 
F2means=numeric(), 
SEPARATOR=character(),
F1sd=numeric(),
F2sd=numeric()
);
for (v in vowels){
	print (v);
	voweldataset=dataset[dataset[[vowel]]==v,];
	print (paste("Tokens:", nrow(voweldataset)));
	#computing means
	F1means=with (voweldataset, aggregate(F1, indi_variables, FUN=mean));
	# print (indi_variables);
	# colnames(F1means)=c("assi");
	# print (F1means);
	# F2means=with (voweldataset, aggregate(F2, eval(indi_variables, voweldataset), FUN=mean))
	# print (F2means);
	# #and stdevs
	# F1sd=with (voweldataset, aggregate(F1, eval(indi_variables, voweldataset), FUN=sd));
	# print (F1sd);
	# F2sd=with (voweldataset, aggregate(F2, eval(indi_variables, voweldataset), FUN=sd))
	# print (F2sd)
	}	
}
#these are the levels we iterate over

ggvowels(ds, VOWEL, F1labov, F2labov, ETHNICITY, GENDER)


#take dataset plus variables, calculate all interactions plus plot
#how to set plot settings 


