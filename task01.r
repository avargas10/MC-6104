#Necessary libraries and packages
install.packages("data.table", type = "binary")
library(data.table)
install.packages("magrittr")
install.packages("dplyr")
library(magrittr)
library(dplyr)

#Import raw data
raw_data <- fread("C:\\Users\\afeli\\Documents\\GitHub\\MC-6104\\data\\data_tarea_1.csv")

#Remove unused information from raw data
useful_columns = subset(raw_data, select = c(Lot,stopband))

#Determine unique lot names
lot_names <- unique(raw_data$Lot)


#Function that calculates the different statistical metrics
calculate_statistic_value <- function(subset, lot_name)
{
    p_size <- nrow(subset)
    p_mean <- mean(subset$stopband)
    p_median <- median(subset$stopband)
    p_min <- min(subset$stopband)
    p_max <- max(subset$stopband)
    p_sd <- sd(subset$stopband)
    p_range <- range(subset$stopband)
    upper_outlier_limit = p_mean + (3 * p_sd)
    lower_outlier_limit = p_mean - (3 * p_sd)
    lot_analysis_data <- data.frame(
      lot=lot_name,
      mean=p_mean,
      sd=p_sd,
      max=p_max,
      min=p_min,
      median=p_median,
      UOL=upper_outlier_limit,
      LOL=lower_outlier_limit)
      size=p_size
    return(lot_analysis_data)
}

#Function that generates color with specific transparency
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}


#Analysis function used to iterate over the different lots generating the metrics 
#and required graphs for each one.
perform_data_analysis <- function(lot_names, dataset, enable_outlier, UOL, LOL) {
  #Final results data frame
  lots_analysis_data <- data.frame(
                   lot=character(),
                   mean=character(),
                   sd=character(),
                   max=character(),
                   min=character(),
                   median=character(),
                   UOL=character(),
                   LOL=character(),
                   size=character(),
                   stringsAsFactors=FALSE)
  lots_without_analysis_data <- lots_analysis_data
  
  #Empty matrix for data without outliers
  empty_mat = matrix(ncol = 0, nrow = 0)
  no_outliers_dataset=data.frame(empty_mat)

  #iterate over the different lots
  for (lot_name in lot_names) {

    #Get subdataset filtering with the lot name
    subdataset = subset(dataset, Lot == lot_name)
    print("Processing Lot:")
    print(lot_name)

    #Calculate the main metrics for each lot
    lot_analysis_data <- calculate_statistic_value(subdataset,lot_name)

    #Append the final results with the accumulated lot information
    lots_analysis_data <- rbind(lots_analysis_data, lot_analysis_data)

    #Get outliers limits for the lot
    UOL = as.double(lot_analysis_data$UOL[1])
    LOL = as.double(lot_analysis_data$LOL[1])

    #Get lot metrics data without outliers
    outliers_lot_dataset = subset(subdataset, stopband < UOL & stopband > LOL)

    #Calculate the main metrics for each lot without outliers
    lot_without_analysis_data <- calculate_statistic_value(outliers_lot_dataset,lot_name)

     #Append the final results with the accumulated lot no outliers information
    lots_without_analysis_data <- rbind(lots_without_analysis_data, lot_without_analysis_data)

    #Fill the complete no outliers raw data.
    no_outliers_dataset <- rbind(no_outliers_dataset, outliers_lot_dataset)
  }
  
  #Get data subset for each lot with outliers to paint the graphs with different colors
  control = subset(dataset, Lot == "Control")
  exp1 = subset(dataset, Lot == "Exp 1")
  exp2 = subset(dataset, Lot == "Exp 2")
  exp3 = subset(dataset, Lot == "Exp 3")
  exp4 = subset(dataset, Lot == "Exp 4")
  exp5 = subset(dataset, Lot == "Exp 5")

  #Assign different color to each hist command and add them to the final graph
  yellow <- t_col("yellow", perc = 0, name = "lt.yellow")
  hist(control$stopband, col=yellow, main="Stopband with outliers", xlab = "Stopband value")
  blue <- t_col("blue", perc = 50, name = "lt.blue")
  hist(exp1$stopband, col=blue, add=T)
  green <- t_col("green", perc = 50, name = "lt.green")
  hist(exp2$stopband, col=green, add=T)
  red <- t_col("red", perc = 60, name = "lt.red")
  hist(exp3$stopband, col=red, add=T)
  black <- t_col("cyan", perc = 60, name = "lt.cyan")
  hist(exp4$stopband, col=black, add=T)
  blueviolet <- t_col("blueviolet", perc = 60, name = "lt.blueviolet")
  hist(exp5$stopband, col=blueviolet, add=T)
  
  #Get data subset for each lot without outliers to paint the graphs with different colors
  control_out = subset(no_outliers_dataset, Lot == "Control")
  exp1_out = subset(no_outliers_dataset, Lot == "Exp 1")
  exp2_out = subset(no_outliers_dataset, Lot == "Exp 2")
  exp3_out = subset(no_outliers_dataset, Lot == "Exp 3")
  exp4_out = subset(no_outliers_dataset, Lot == "Exp 4")
  exp5_out = subset(no_outliers_dataset, Lot == "Exp 5")

  #Assign different color to each hist command and add them to the final graph
  yellow <- t_col("yellow", perc = 0, name = "lt.yellow")
  hist(control_out$stopband, col=yellow, main="Stopband with outliers", xlab = "Stopband value")
  blue <- t_col("blue", perc = 50, name = "lt.blue")
  hist(exp1_out$stopband, col=blue, add=T)
  green <- t_col("green", perc = 50, name = "lt.green")
  hist(exp2_out$stopband, col=green, add=T)
  red <- t_col("red", perc = 60, name = "lt.red")
  hist(exp3_out$stopband, col=red, add=T)
  black <- t_col("cyan", perc = 60, name = "lt.cyan")
  hist(exp4_out$stopband, col=black, add=T)
  blueviolet <- t_col("blueviolet", perc = 60, name = "lt.blueviolet")
  hist(exp5_out$stopband, col=blueviolet, add=T)

  #Generate the boxplot using the full dataset with outliers
  boxplot(dataset$stopband ~ dataset$Lot, xlab = "Lot Name")

  #Generate the boxplot using the full dataset with outliers but limiting the Y axis between 24 and 30
  boxplot(dataset$stopband ~ dataset$Lot, ylim = c(24, 30), xlab = "Lot Name")

}

#Execute main analysis function
perform_data_analysis(lot_names, useful_columns)

