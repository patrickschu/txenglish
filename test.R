

test=function(thing, ...)
{
	substitute(alist(...))
}


test('dfdf', assi, muschi)

ds=read.csv('/Users/ps22344/Desktop/canada_final_1121.csv');
print (nrow(ds));
cat ("\n\n\nSTART");
ggvowels=function(dataset, vowel, F1, F2,  ...) 

{
	
vowel=deparse(substitute(vowel))
F1=deparse(substitute(F1))
F2=deparse(substitute(F2))
indi_variables=substitute(list(...));
header="\n------\n"
#summary of dataset
vowels=levels(dataset[[vowel]]);
cat (sprintf ("%sWe are working with %d vowel(s):\n", header, length(vowels)));
print (levels(dataset[[vowel]]));

cat (sprintf ('%sWe are working with %s independent variable(s):\n', header, "XXXXX"));
for (var in deparse(indi_variables))
 	{
	 writeLines(unlist(lapply(var, paste, collapse=" ")));
	 }

for (v in vowels){
	print (v);
	voweldataset=dataset[dataset[[vowel]]==v,];
	print (paste("Tokens:", nrow(voweldataset)));
	#computing means
	F1means=with (voweldataset, aggregate(F1, eval(indi_variables,voweldataset), FUN=mean));
	print (F1means);
	F2means=with (voweldataset, aggregate(F2, eval(indi_variables, voweldataset), FUN=mean))
	print (F2means);
	#and stdevs
	F1sd=with (voweldataset, aggregate(F1, eval(indi_variables, voweldataset), FUN=sd));
	print (F1sd);
	F2sd=with (voweldataset, aggregate(F2, eval(indi_variables, voweldataset), FUN=sd))
	print (F2sd)
	}	
}
#these are the levels we iterate over

ggvowels(ds, VOWEL, F1labov, F2labov, ETHNICITY)


#take dataset plus variables, calculate all interactions plus plot
#how to set plot settings 


