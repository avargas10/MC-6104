install.packages("data.table", type = "binary")
library(data.table)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
data3 <- fread("C:\\Users\\andres.vargas\\Documents\\TEC Maestria\\Diseno Experimentos\\tareas\\tarea 1\\Datos_tarea_1\\datos_tarea_1.csv")
useful_columns = subset(data3, select = c(Lot,stopband))
lot_names <- unique(data3$Lot)

calculate_statistic_value <- function(subset, lot_name)
{
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
                   stringsAsFactors=FALSE)

  #iterate over the different lots
  for (lot_name in lot_names) {
    subdataset = subset(dataset, Lot == lot_name)
    print("Processing Lot:")
    print(lot_name)
    lot_analysis_data <- calculate_statistic_value(subdataset,lot_name)
    lots_analysis_data <- rbind(lots_analysis_data, lot_analysis_data)
    UOL = lot_analysis_data['UOL']
    LOL = lot_analysis_data['LOL']
    print("UOL")
    print(UOL)
    print("LOL")
    print(LOL)
    print("with")
    print(nrow(subdataset))
    outliers_dataset = subset(dataset, Lot == lot_name & stopband < UOL & stopband > LOL)
    print("without")
    print(nrow(outliers_dataset))
    print(lots_analysis_data)
    #hist(subdataset$stopband, main = lot_name)
  }
  write.csv(lots_analysis_data,"lots_analysis_data.csv", row.names = TRUE)
}




perform_data_analysis(lot_names, useful_columns)

