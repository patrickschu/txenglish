#Using the plotmachine

The plotmachine computes F1 and F2 means for any number of predictors and then plots those using ggplot.

Download it [here](https://github.com/patrickschu/txenglish/blob/master/plotmachine/plotmachine.R). Click on *Raw*, download as a text file and run it in R. 

The plotmachine takes the following input:

    plotmachine=function(dataset, vowel, F1, F2, ..., plot_title=" ") 
    
    Arguments:
    dataset --- the dataset you're working with. Needs to be created first, e.g. by reading in a spreadsheet with read.csv().
    vowel --- the name of the column within this dataset that contains the vowel labels.
    F1 --- the name of the column within this dataset that contains the F1 measurements.
    F2 --- the name of the column within this dataset that contains the F2 measurements.
    ... --- any number of independent variables that you want to plot .
    title --- the title of your plot, also part of the name of the file your plot will be saved as. Defaults to " ".

For one independent variable, it prints out the means by category, outputs and saves a plot as a png file. For several independent variables, it prints out means and creates plots for all combinations of variables. It returns the dataframe of means.

Thus, for a dataset called *texasenglish*, this plots the formants by gender for all vowels that are in a column called *VOWEL*. 

    plotmachine(texasenglish, VOWEL, F1NORMALIZED, F2NORMALIZED, GENDER, plot_title="Gender differences in Texas English")

It uses the data found in *F1NORMALIZED* and *F2NORMALIZED*. Note that none of the input besides the `plot_title` should be in quotation marks. 
Note that you can feed it as many independent variables as you like. This example will calculate means by gender, ethnicity and agegroup: 
    
    plotmachine(texasenglish, VOWEL, F1NORMALIZED, F2NORMALIZED, GENDER, ETHNICITY, AGEGROUP, plot_title="Random vowel chart")

It will create these plots: gender x ethnicity, gender x age group, ethnicity x agegroup. 

Note that you need to have the packages vowels, ggplot2 and lme4 installed for this to work. 



---
To do: return dataframe, check for libraries
