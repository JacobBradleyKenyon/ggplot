###########################################################################################################
##  Required Packages                                                                                    ##
###########################################################################################################
  library(ggplot2)
  library(grid)
  library(gridExtra)



###########################################################################################################
##  Collecting and Formatting Time Series Data                                                           ##
###########################################################################################################
  ## Data Of Interest - New York Air Quality Measurments from May to Sept 1973
    DOI <- airquality

  ## Date Variable - Convert numeric month(w/out leading zeros) and day(w/out leading zeros) to Date Format
    DOI$Date <- as.POSIXct(paste("1973",
                              ifelse(nchar(DOI$Month)==1, "-0", "-"), DOI$Month,
                              ifelse(nchar(DOI$Day)==1, "-0", "-"), DOI$Day, sep="") )
  
  ## Creating Variable Description Data Frame to reference
    Var_Desc <- data.frame(Ozone="Ozone (ppb)", Solar.R="Solar.R (langley)",
                           Wind="Wind (mph)", Temp="Temperature (farenheit)",
                           stringsAsFactors = FALSE)

    

########################################################################################################### 
##  GGPLOT CODE - Illustrates plotting time series to indicate missing data and how to format Date axis  ##          
########################################################################################################### 
TS_NA_EDA_ggplot <- function(DATA=DOI, DEPENDENT="Ozone",  
                             DATE_BREAKS="10 days", MINOR_BREAKS="days", DATE_FORMAT=format('%b-%d'),
                             TITLE="New York Air Quality Measurements - 1973",
                             Y_LABEL="Ozone (ppb)", X_LABEL=NULL){
  
  ix.NA  <- subset(DATA, is.na(DATA[,DEPENDENT]))
  n.miss <- nrow(ix.NA)
  p.miss <- round(n.miss/nrow(DATA),2)*100
  
  ## Setting up Base Layer - dictates aesthetics (i.e. how the data is represented in the plots)
    plt <- ggplot(data=DATA, aes(x=Date, y=DATA[,DEPENDENT])) + 
            ## Line Plot Layer
              geom_line(size=1, color=rgb(5/255, 25/255, 215/255, 0.65)) + 
            ## Points Layer
              geom_point(color=rgb(5/255, 25/255, 215/255, 0.95)) 
   
  if(nrow(ix.NA) > 0){
    plt <- plt +  
            ## Missing Value Layer   
              annotate("rect", xmin=ix.NA$Date, xmax=ix.NA$Date, ymin=-Inf, ymax=Inf, color=rgb(1, .01, .02, 0.85))
  }else{
    plt <- plt
  }
    plt <- plt + 
       ## Label Layer    
        labs(caption = paste(n.miss, " Missing Observations  =  ", p.miss, "% Missingness", sep=''),
             title   = TITLE,
             y       = Y_LABEL,
             x       = X_LABEL) + 
      ## Formatting Date which appears on x-axis  
        scale_x_datetime(date_breaks=DATE_BREAKS, date_labels=DATE_FORMAT, minor_breaks=MINOR_BREAKS) +
      ## Setting global theme to black and white
        theme_bw()  
      
list(PLOT=plt)   
}

    
    
########################################################################################################### 
##  Using Function in Loop to print plots for all Variables of Interest within Var_Desc object           ##          
########################################################################################################### 
  ## Creating empty list to collect all plots inside loop
    Plots <- list()
      for(i in 1:length(Var_Desc))
        Plots[[i]] <- TS_NA_EDA_ggplot(DATA=DOI, DEPENDENT=colnames(Var_Desc)[i], TITLE=NULL, Y_LABEL=Var_Desc[i][1,1])    
    
    
  ## Manual Adjust theme - Illustrates adjustments which can be made after an initial plot is stored
    Theme_Adjust <-
        theme(plot.caption = element_text(size=rel(1), face = "bold", hjust=0.5, vjust=-1, color=rgb(1, .01, .02, 0.85),
                             ## MARGINS IN GGPLOT - c(top, right, bottom, left) ##
                                          margin = unit(c(0,1,3,1), "lines")),
              axis.text.x  = element_text(angle=45, vjust=1, hjust=1))
  
  ## Creating Main title for multiple ggplots within one frame using "grid" package
    Main_Title <- grid::textGrob("New York Air Quality Measurements - 1973", gp=grid::gpar(fontsize=18, font=2))
    
  ## Plotting Multiple ggplots using "gridExtra" package
    gridExtra::grid.arrange(Plots[[1]]$PLOT + Theme_Adjust,
                            Plots[[2]]$PLOT + Theme_Adjust,
                            Plots[[3]]$PLOT + Theme_Adjust,
                            Plots[[4]]$PLOT + Theme_Adjust,
                            top= Main_Title, nrow=2)
    
      
    
