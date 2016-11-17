#Verro Ejiba
#Homework 8 
#Group C
#Think about the ways things could go wrong and write some defensive code to gracefully handle exceptions. 
#(for example,what if the first parameter is not a dataframe?)

#Refers to the main Explore function from a different R Scrit
source("Explore_Ejiba_Verro.R")
explore(dataframe, plot_switch, threshold, bins){

  #We need an error message when the function parameters are missing
  if (missing(dataframe))
    stop("Need to input a valid data frame.")
  if (missing(plot_switch))
    stop("Need to enter the mode for plot in order to generate graphs.")
  if (missing(threshold))
    stop("Need to enter the correct threshold for correlation computation")
  if (missing(bins))
    stop("Need to enter the right number of bins for the plots")
  #Need to have error message when an parameter that has been entered is not a data frame
  if(is.data.frame(dataframe) == FALSE)
    stop("Please enter a valid dataframe")
  #wrong value for plot_switch
  if(plot_switch != "on" & plot_switch != "off" & plot_switch != "grid")
    stop("Please enter 'on', 'off', or 'grid' for the plot mode")
  #Non numerical input for threshold and bins
  if(is.numeric(threshold) == FALSE)
    stop("Enter a numberical value for the threshold")
  if(is.numeric(bins) == FALSE)
    stop("Please enter a numerical bin value") 
}
