#Verro Ejiba
#Homework 7 Explore function
#Group C

#Download ggplot for the plot function
install.packages("ggplot2")
library("ggplot2")

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
    for (j in (i+1):length(colnames(dataframe))) {
      fit <- lm(dataframe[,i]~dataframe[,j]) #Creates a linear model
      rsfit <- summary(fit)$r.squared #get r-squared values
      pvars <- c(pvars, paste(colnames(dataframe[i]),colnames(dataframe[j]),sep="-")) #Gets pair of column names
      rpairs <- c(rpairs,rsfit) #vectorize the r-squared values
    }
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

## Copied from group member Jack
#Need to update with comment line
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # this function draws multiple graphs in one page.
  # reference: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  plots <- c(list(...), plotlist)
  #
  numPlots = length(plots)
  if (is.null(layout)) {
    # 
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # 
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      # 
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plots <- function(df, plotswitch= 'off', bins = NULL){
  
  df <- df[sapply(df, is.numeric)]
  
  if(plotswitch == "on"){
    if(!is.null(vector)){ # 
      for(j in 1:length(bins)){ 
        for(i in 1:ncol(num)){
          mean <- mean(num[,i]) 
          # 
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+ 
            geom_histogram(fill="blue",bins=bins[j])+
            ggtitle(paste(colnames(num[i]),bins[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") 
          # 
          # 
          
          p2 <- ggplot(num,aes(x=num[i],..density..))+
            geom_histogram(fill="blue",bins=bins[j])+
            ggtitle(paste(colnames(num[i]),bins[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") 
          #
          
          grid.newpage()
          #
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          title <- paste(colnames(num[i]),bins[j],sep=" bin=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
          # 
          
        }
      }
    }
    else{ #
      for(i in 1:ncol(num)){
        mean <- mean(num[,i]) 
        # 
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+  
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        # 
        
        p2 <- ggplot(num,aes(x=num[i],..density..))+
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        # 
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(num[i]),"default bins",sep=" bins=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))# 
        
      }
      
    }
    
  }
  else{
    if(plotswitch == "grid"){#  
      for(j in 1:length(bins)){
        grid.newpage()
        his_count <-list()   
        his_density <- list()  
        #
        for(i in 1:ncol(num)){
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(fill="blue", bins = bins[j])+ 
            labs(title= paste(bins[j], "bins")) 
          #
        }
        multiplot(plotlist = his_count, cols = 2)  
        #
        for(i in 1:ncol(num)){
          his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue", bins = bins[j])+ 
            labs(title= paste(bins[j], "bins")) 
          #
        }
        multiplot(plotlist = his_density, cols = 2)  
        #
      }
    }
  }
}