#Verro Ejiba
#Homework 7 Explore function
#Group C

#Main function
explore <- function(dataframe, plot_switch = 'O', threshold, bins = NULL) {
 
  tab <- func_table(dataframe) #prints the frequency table of the dataframe
  summ <- func_summary(dataframe) #prints the summary of the data
  r_square <- func_rsquare(dataframe) #prints the r-square values of numerical column
  corr <- correlation(dataframe, threshold) #correlation
  
  return (c(tab, summ, r_square, corr))
}

#Sample dataframe
install.packages("vcd")
library("vcd")
data(Arthritis)

func_table <- function(dataframe) {
  #A Function that takes a dataframe as a parameter and returns a list of :
  #frequency table categorical and logical variable
  #frequency table for every categorical and logical variable
   t <- c(dataframe[sapply(dataframe, is.factor)], dataframe[sapply(dataframe, is.logical)])
   return(lapply(vec, table))
}

func_sum <- function(dataframe){
  #A function that takes a dataframe and returns in a list:
  #For numerical variable:
  # a) prints a summary statistics table for each numerical variable
  t <- sapply(dataframe, is.numeric)
  return(lapply(dataframe[,t], summary))
}

func_rsquare <- function(dataframe){
  # b) A dataframe that contains each pair of column names in the first column and the associated
  #r-square value in the second column.
  #
  dataframe <- sapply(dataframe, is.numeric)
  #Create a data frame from all combinations of factor variable
  #indx <- expand.grid(colnames(dataframe), colnames(dataframe), stringsAsFactors=FALSE) 
  #new column vectors
  pvars <- c()
  rpairs <- c()
  for (i in 1:(length(colnames(dataframe))-1)) {
      fit <- lm(dataframe[,i]~dataframe[,i+1], data= dataframe) #Creates a linear model
      rsfit <- summary(fit)$r.squared #get r-squared values
      pvars <- c(pvars, paste(colnames(dataframe),colnames(dataframe),sep="-")) #Gets pair of column names
      rpairs <- c(rpairs,rsfit) #vectorize the r-squared values
  }
  newdata <- data.frame(pvars,rpairs) #put both the column names and r-squares into dataframe
  colnames(newdata) <- c("Variable Pairs", "R-squared") #rename column variables
  return(newdata)
}


correlation <- function(dataframe, threshold = 0){
    #A dataframe that contains each pair of columns in the first column and correlation coefficient (Pearson) for all coefficient
    #threshold in the second column a function that calculates correlation coefficients
    #Parameters: A dataframe
    #Returns: A dataframe with pair of column names and its correlation using pearson method
    
    #remove missing values in the data
    dataframe <- na.omit(dataframe)
    #Get numerical variables
    dataframe <- dataframe[sapply(dataframe, is.numeric)]
    #Create combination of all factor variables
    comb <- expand.grid(colnames(dataframe), colnames(dataframe), stringsAsFactors = FALSE)
    #get the pairs separated by -
    pairs <- paste(comb$Var1,comb$Var2, sep = "-")
    #Creates a correlation matrix
    corr_mtx <- cor(dataframe, method = "pearson")
    #Just consider the lower triangle part of the matrix
    corr <- c(which(lower.tri(corr_mtx)))
    #creates a dataframe with pairs of variable names and their correlations 
    ndata <- data.frame(pairs, corr) #Error on different number of rows
    ndata <- subset(ndata, corr > threshold) #overwrites the new data with correlation that exceeds the threshold
    colnames(ndata) <- c("Variables","Cor")

    return(ndata)
}

plots <- function(df, plotswitch, bins = NULL){
  df <- df[sapply(df, is.numeric)]
  
}