#Verro Ejiba
#Homework 8 
#Group C
#Think about the ways things could go wrong and write some defensive code to gracefully handle exceptions. 
#(for example,what if the first parameter is not a dataframe?)

source("Explore_Ejiba_Verro.R")
explore(dataframe, plot_switch = 'ON', threshold = 0, bins = 2)
#Users can forget to enter all the parameters
#We need an error message when the dataframe parameter is missing
if (missing(dataframe))
  stop("Need to input a valid data frame.")
#Need to have error message when an parameter that has been entered is not a dataframe
if(!is.data.frame(dataframe))
  stop("Please enter a valid dataframe")
