#Using the plotmachine
The plot machine takes the following inpu

    plotmachine=function(dataset, vowel, F1, F2,  ..., plot_title=" ") 
    
    Arguments:
    dataset --- the dataset you're working with. Needs to be created first, e.g. by reading in a spreadsheet with read.csv()
    vowel --- the name of the column within this dataset that contains the vowel labels
    F1 --- the name of the column within this dataset that contains the F1 measurements
    F2 --- the name of the column within this dataset that contains the F2 measurements
    ... --- any number of independent variables that you want to plot 
    title --- the title of your plot, also part of the name of the file your plot will be saved as. Defaults to " ".

Thus

    

Note that you need to have the packages vowels, ggplot2 and lme4 installed for this to work. 
