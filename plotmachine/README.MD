#Using the plotmachine
The plot machine takes the following input:

    plotmachine=function(dataset, vowel, F1, F2,  ..., plot_title=" ") 
    
    Arguments:
    dataset --- the dataset you're working with. Needs to be created first, e.g. by reading in a spreadsheet with read.csv()
    vowel --- the name of the column within this dataset that contains the vowel labels
    F1 --- the name of the column within this dataset that contains the F1 measurements
    F2 --- the name of the column within this dataset that contains the F2 measurements
    ... --- any number of independent variables that you want to plot 
    title --- the title of your plot, also part of the name of the file your plot will be saved as. Defaults to " ".

Thus, for a dataset called *texasenglish*, this plots the formants by gender for all vowels that are in a column called *VOWEL*. It uses the data found in *F1NORMALIZED* and *F2NORMALIZED*.

    plotmachine(texasenglish, VOWEL, F1NORMALIZED, F2NORMALIZED, GENDER, plot_title="Gender differences in Texas English")
    
    plotmachine(texasenglish, VOWEL, F1NORMALIZED, F2NORMALIZED, GENDER, plot_title="Gender differences in Texas English")

Note that you need to have the packages vowels, ggplot2 and lme4 installed for this to work. 
