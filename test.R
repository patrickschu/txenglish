

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



#ds=read.csv('/Users/ps22344/Desktop/canada_final_1121.csv');
ds=read.csv('C:\\Users\\ps\\My Documents\\Github\\canadavowels\\canada_final_1121.csv')
print (nrow(ds));

ggvowels=function(dataset, vowel, F1, F2,  ...) 

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

outputframe=data.frame(as.list(as.character(user_vars[-1L])))[factor(0),];
print ("OUTTTI");
print (str(outputframe));
#iterate over vowels
for (v in vowels){
	print (v);
	voweldataset=dataset[dataset[[vowel]]==v,];
	print (paste("Tokens:", nrow(voweldataset)));
	indi_variables=eval(substitute(list(...)), voweldataset);
	#computing means
	F1means=with(voweldataset, aggregate(F1, indi_variables, mean));
	F1means$vowel=v;
	colnames(F1means)=c(as.character(user_vars[-1L]), "F1means");

	F1sd=with(voweldataset, aggregate(F1, indi_variables, sd));
	colnames(F1sd)=c(as.character(user_vars[-1L]), "F1sd");

	F2means=with(voweldataset, aggregate(F2, indi_variables, mean));
	colnames(F2means)=c(as.character(user_vars[-1L]), "F2means");

	F2sd=with(voweldataset, aggregate(F2, indi_variables, sd));
	

	colnames(F2sd)=c(as.character(user_vars[-1L]), "F2sd");
	F2sd[,vowel]=v;
	print (F2sd);
	print (length(F2sd));
	merge(F1means, F1sd, by=as.character(user_vars[-1L]))
	}	
}
#these are the levels we iterate over

ggvowels(ds, VOWEL, F1labov, F2labov, ETHNICITY, GENDER)


#take dataset plus variables, calculate all interactions plus plot
#how to set plot settings 


