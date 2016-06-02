

ds=read.csv('/Users/ps22344/Desktop/canada_final_1121.csv');
print (nrow(ds));
cat ("\n\n\nSTART");
ggvowels=function(dataset, vowel, F1, F2,  ...) 

{


header="\n------\n"
#summary of dataset
vowels=levels(dataset[[vowel]]);
cat (sprintf ("%sWe are working with %d vowel(s):\n", header, length(vowels)));
print (levels(dataset[[vowel]]));
indi_variables=alist(...);
print(indi_variables);
eval(indi_variables);




#cat (sprintf ('%sWe are working with %d independent variable(s):\n', header, length(indi_variables)));
# for (var in indi_variables)
# {
	# print(as.character(var));
	# cat (levels(dataset[[as.character(var)]]), "\n")
# }

#computing means
#we need a tapply over all the indies
#r=with (ds, tapply(F1labov, list(ETHNICITY), mean) )
#F1table=with (dataset, tapply(F1labov, indi_variables, mean));

#F2table=tapply(F2~indi)
	
}
#these are the levels we iterate over

ggvowels(ds, 'VOWEL', 'F1labov', 'F2labov', 'ETHNICITY')


#take dataset plus variables, calculate all interactions plus plot
#how to set plot settings 


