########################################################################################################### 
##  Function to plot correlation heat map                                                                ##          
########################################################################################################### 
  Heat_Map <- function(Data=airquality[,1:4], InsertText=FALSE, LowGradient="#3182bd", HighGradient="#de2d26"){
  
  ## Converting Correlation of data from wide to long format
    plot.data <- reshape2::melt(cor(na.omit(Data)))  ;  plot.data <- data.frame(plot.data, stringsAsFactors = F) ; colnames(plot.data)[3] <- "Correlation"
        
  ## Gathering p-values associated with correlations to plot within heat map 
    V.seq <- matrix(seq(1:nrow(plot.data)))
    plot.data$`p-value` <- apply(as.matrix(seq(1:16)), 1, function(ix) 
                                 cor.test(x=Data[,as.character(plot.data[ix,1])],
                                          y=Data[,as.character(plot.data[ix,2])])$p.va)
  
  ## Setting Color Gradient for plotting
    colshift <- colorRampPalette(c(LowGradient, "white", HighGradient))
    Colors   <- colshift(100)
   
  ## Using ggplot to create Heatmap of correlations
    # Initial ggplot layer
plt<-ggplot(plot.data, aes(Var1, Var2)) +
      # Setting Title layer
      ggtitle("Heat Map: New York Air Quality Measurements in 1973")  +
        # Creating tiles according to value (i.e. correlation)
        geom_tile(aes(fill=Correlation)) +
          # Setting color gradient for heatmap
          scale_fill_gradientn(colours=Colors, limits=c(-1,1), name="Correlation Key:   " ) 
    
    if(InsertText==TRUE){

  
      plt <- plt + 
        # Placing Correlation values into plot
            geom_text(aes(label=paste("correlation:", round(Correlation,4), "\np-value: ", round(`p-value`,6))))
    }else{
      plt <- plt
    }
      plt <- plt +       
              # Fixing aspect of plot to 1:1 ratio
              coord_fixed() +  
                # Setting Theme to remove borders, gridlines, set legend, axis text and title size
                theme_bw() +
                theme(panel.grid      = element_blank(),
                      panel.border    = element_blank(),
                      axis.title      = element_blank(),
                      axis.ticks      = element_blank(),
                      axis.text       = element_text(size=rel(1.25)),
                      axis.text.x     = element_text(angle=90, hjust=1, vjust=.3),
                      legend.position = "right",
                      legend.title    = element_text(size=rel(1.2)),
                      legend.text     = element_text(size=rel(.8)),
                      legend.key.size = unit(rel(5),"line"),
                      plot.title      = element_text(size=rel(1.5), hjust=0.5) 
                      ) 
     
    list(Plot=plt)
}

  ## Example with and without Inserted correlation/p-values
    plt.1 <- Heat_Map(InsertText=FALSE)
    plt.2 <- Heat_Map(InsertText=TRUE)

  ## Creating Main title for multiple ggplots within one frame using "grid" package
    Main_Title <- grid::textGrob("New York Air Quality Measurements - 1973", gp=grid::gpar(fontsize=18, font=2))
    

gridExtra::grid.arrange(plt.2$Plot + theme(plot.title = element_blank(), legend.position = ""),
                        plt.1$Plot + theme(plot.title = element_blank(), legend.position = "right", legend.key.size = unit(rel(1),"line")),
                        top=Main_Title, ncol=2)
