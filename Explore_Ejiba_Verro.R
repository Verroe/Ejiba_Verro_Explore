#Verro Ejiba
#Homework 7 Explore function
#Group C

#Main function
explore <- function(a, plot_switch, cor, vec) {
  
}

#Sample dataframe
install.packages("vcd")
library("vcd")
data(Arthritis)

func_table <- function(dataframe) {
  #A Function that takes a dataframe as a parameter and returns a list of :
  #frequency table categorical and logical variable
  #frequency table for every categorical and logical variable
   t <- sapply(dataframe, is.factor)
   return(lapply(dataframe[,t], table))
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
  D.lm <- lm() #build this model
  return(lapply(dataframe[,sapply(dataframe, is.numeric)], summary(D.lm)$r.squared))
  
}

correlation <- function(dataframe){
    #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
    #and their corresponding Pearson correlation coefficient in the second column.
    #Parameters: A dataframe
    #Returns: A dataframe with pair of column names and its correlation using pearson method
    
    #remove NA in the data
    ndata <- na.omit(dataframe)
    #Creates a correlation matrix
    corr_mtx <- cor(ndata[sapply(ndata, is.numeric)], method = "pearson")
    #Prepare to remove duplicates by ignoring the lower triangle part of the matrix
    corr_mtx[lower.tri(corr_mtx, diag = TRUE)] = NA
    #creates a dataframe of 3 column table 
    corr_mtx <- as.data.frame(as.table(corr_mtx))
    #Remove the duplicates 
    corr_mtx <- na.omit(corr_mtx)
    #Write the result as with each pair of the column name separated by "-"
    #corr_mtx <- corr_mtx[paste(corr_mtx$Var1, corr_mtx$Var2, sep = "-"), ]
    colnames(corr_mtx) <- c("Var1","Var2","Cor Coef")
    row.names(corr_mtx) <- c(1:nrow(corr_mtx))
    
    return(corr_mtx)
}
# c) A dataframe that contains each pair of columns in the first column and correlation coefficient (Pearson) for all coefficient
#threshold in the second column a function that calculates correlation coefficients