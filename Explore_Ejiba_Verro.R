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
  dataframe <- sapply(dataframe, is.numeric)
  indx <- expand.grid(colnames(dataframe), colnames(dataframe), stringsAsFactors=FALSE)
  res <- sapply(seq_len(nrow(indx)),function(i) {i1 <- indx[i,]
  form <-as.formula(paste(i1[,1], i1[,2], sep="~"))
  fit <- lm(formula=form, data= dataframe)
  summary(fit)$r.squared})
  return(res)
}


#Need to add take coefficients whose absolute value is greater than the correlation threshold
correlation <- function(dataframe){
    #c) A dataframe that contains each pair of columns in the first column and correlation coefficient (Pearson) for all coefficient
    #threshold in the second column a function that calculates correlation coefficients
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

#I am still working on number 3 
