install.packages("data.table", type = "binary")
library(data.table)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
data3 <- fread("C:\\Users\\afeli\\Documents\\GitHub\\MC-6104\\data\\data_tarea_1.csv")
useful_columns = subset(data3, select = c(Lot,stopband))
lot_names <- unique(data3$Lot)

calculate_statistic_value <- function(subset, lot_name)
{
    p_size <- nrow(subset)
    print("size")
    print(p_size)
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

#Analysis function used to calculate the data mean, median, sd, max, min and range
perform_data_analysis <- function(lot_names, dataset, enable_outlier, UOL, LOL) {
  # loop version 1
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
  empty_mat = matrix(ncol = 0, nrow = 0)
  outliers_dataset=data.frame(empty_mat)
  iterator <- 1
  #iterate over the different lots
  for (lot_name in lot_names) {
    iterator <- iterator + 1
    subdataset = subset(dataset, Lot == lot_name)
    print("Processing Lot:")
    print(lot_name)
    lot_analysis_data <- calculate_statistic_value(subdataset,lot_name)
    lots_analysis_data <- rbind(lots_analysis_data, lot_analysis_data)
    UOL = as.double(lot_analysis_data$UOL[1])
    LOL = as.double(lot_analysis_data$LOL[1])
    outliers_lot_dataset = subset(subdataset, stopband < UOL & stopband > LOL)
    lot_without_analysis_data <- calculate_statistic_value(outliers_lot_dataset,lot_name)
    lots_without_analysis_data <- rbind(lots_without_analysis_data, lot_without_analysis_data)
    outliers_dataset <- rbind(outliers_dataset, outliers_lot_dataset)
  }
  
  control = subset(dataset, Lot == "Control")
  exp1 = subset(dataset, Lot == "Exp 1")
  exp2 = subset(dataset, Lot == "Exp 2")
  exp3 = subset(dataset, Lot == "Exp 3")
  exp4 = subset(dataset, Lot == "Exp 4")
  exp5 = subset(dataset, Lot == "Exp 5")
  hist(control$stopband, col="yellow", main="Stopband with outliers Hist")
  hist(exp1$stopband, col="blue", add=T)
  hist(exp2$stopband, col="green", add=T)
  hist(exp3$stopband, col="brown", add=T)
  hist(exp4$stopband, col="black", add=T)
  hist(exp5$stopband, col="grey", add=T)
  
  control_out = subset(outliers_dataset, Lot == "Control")
  exp1_out = subset(outliers_dataset, Lot == "Exp 1")
  exp2_out = subset(outliers_dataset, Lot == "Exp 2")
  exp3_out = subset(outliers_dataset, Lot == "Exp 3")
  exp4_out = subset(outliers_dataset, Lot == "Exp 4")
  exp5_out = subset(outliers_dataset, Lot == "Exp 5")
  hist(control_out$stopband, col="yellow", main="Stopband without outliers Hist")
  hist(exp1_out$stopband, col="blue", add=T)
  hist(exp2_out$stopband, col="green", add=T)
  hist(exp3_out$stopband, col="brown", add=T)
  hist(exp4_out$stopband, col="black", add=T)
  hist(exp5_out$stopband, col="grey", add=T)
  hist(outliers_dataset$stopband, col="blue", main="Stopband without outliers Hist 2")
  hist(dataset$stopband, col="yellow", main="Stopband with outliers Hist 2")
  write.csv(lots_analysis_data,"C:\\Users\\afeli\\Documents\\GitHub\\MC-6104\\results\\lots_analysis_data.csv", row.names = TRUE)
  write.csv(lots_without_analysis_data,"C:\\Users\\afeli\\Documents\\GitHub\\MC-6104\\results\\lots_without_analysis_data.csv", row.names = TRUE)
}


perform_data_analysis(lot_names, useful_columns)

