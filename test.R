

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

#deparsing for further use
vowel=deparse(substitute(vowel));
F1=deparse(substitute(F1));
F2=deparse(substitute(F2));
header="\n------\n"

#summary of dataset
vowels=levels(dataset[[vowel]]);
cat (sprintf ("%sWe are working with %d vowel(s):\n", header, length(vowels)));
print (levels(dataset[[vowel]]));
#cat (sprintf ('%sWe are working with %s independent variable(s):\n', header, length(deparse(list(...)))));

#iterate over vowels
for (v in vowels){
	print (v);
	voweldataset=dataset[dataset[[vowel]]==v,];
	print (paste("Tokens:", nrow(voweldataset)));
	indi_variables=eval(substitute(list(...)), voweldataset);
	#computing means
	F1means=with(voweldataset, aggregate(F1, indi_variables, mean));
	F1sd=with(voweldataset, aggregate(F1, indi_variables, sd));
	F2means=with(voweldataset, aggregate(F2, indi_variables, mean))
	F2sd=with(voweldataset, aggregate(F2, indi_variables, sd))
	#make a dataset per vowel, plot 
	}	
}
#these are the levels we iterate over

ggvowels(ds, VOWEL, F1labov, F2labov, ETHNICITY, GENDER)


#take dataset plus variables, calculate all interactions plus plot
#how to set plot settings 


