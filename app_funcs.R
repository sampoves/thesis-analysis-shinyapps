
# Sampo Vesanen's Master's thesis statistical tests and visualisation
#####################################################################
#### Stats and visualisation functions and essential variables

# "Parking of private cars and spatial accessibility in Helsinki Capital Region"
# by Sampo Vesanen
# 3.3.2020



# Initialise
library(onewaytests)
library(car)
library(plotrix)
library(moments)
library(rlang)


CreateJenksColumn <- function(fortified, postal, datacol, newcolname, classes_n = 5) {
  
  # Use this function to create a column in fortified dataframe that can be
  # used to portray Jenks breaks colouring in a ggplot map. Dplyr note: to
  # enable parameters as column names in dplyr, apply !! and := for the left
  # side and for the right side !!rlang::sym().
  #
  # Adapted from:
  # https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098
  
  classes <- classInt::classIntervals(postal[, datacol], n = classes_n, 
                                      style = "jenks")
  
  # classes$brk has to be wrapped with unique(), otherwise we can't get more
  # than six classes for parktime_median or walktime_median
  result <- fortified %>%
    mutate(!!newcolname := cut(!!rlang::sym(datacol), 
                               unique(classes$brks), 
                               include.lowest = T))
  
  # Reverse column values to enable rising values from bottom to top in ggplot.
  # In ggplot, use scale_fill_brewer(direction = -1) with this operation to flip
  # the legend.
  result[, newcolname] = factor(result[, newcolname], 
                                levels = rev(levels(result[, newcolname])))
  
  return(result)
}



SigTableToShiny <- function(sigTable, hasHeading) {
  
  # Use this function to show significance tables in Shiny. It will be useful
  # with Levene and ANOVA results. 
  
  # Due to the format of the significance table it is difficult to present it
  # in Shiny. The main functionality of this method is to make the significance
  # star available in the app.
  
  # Levene test dataframe requires transposing. Levene table has an attribute 
  # heading while ANOVA doesn't. Use this.
  if (is.null(attributes(sigTable)$heading)) {
    # ANOVA
    res <- as.data.frame(do.call(rbind, sigTable))
  } else {
    # Levene
    res <- t(as.data.frame(do.call(rbind, sigTable)))
  }
  
  # Take into account that the table may have an attribute heading. Ask if this 
  # is the case
  if (hasHeading == FALSE){
    sigTablePosition <- 2
  } else {
    sigTablePosition <- 3
  }
  
  # Get the location of the signif.star
  signif_ncol <- ncol(read.table(textConnection(
    capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE))
  
  # get signif.star
  signif_star <- read.table(textConnection(
    capture.output(sigTable)[sigTablePosition]), 
    fill = TRUE)[[signif_ncol]]
  
  # Detect if signif_star is something else than factor. If so, the function
  # has picked up a value from probability column and the current analysis is 
  # not significant. Change value to " ".
  if(!is.factor(signif_star)){
    signif_star <- " "
  }
  
  # repeated_na takes into account that the significance table may have more
  # rows than two.
  repeated_na <- rep("NA", nrow(res) - 1)
  signif_star <- c(as.character(signif_star), repeated_na)
  
  # Bind column signif_star to result.
  res <- cbind.data.frame(res, signif_star)
  
  # Name rows. Try to detect differences in Levene and ANOVA summary tables.
  if(is.null(rownames(sigTable[[1]]))){
    # Levene
    rownames(res) <- rownames(sigTable)
  } else {
    # ANOVA
    rownames(res) <- rownames(sigTable[[1]])
  }
  
  return(res)
}