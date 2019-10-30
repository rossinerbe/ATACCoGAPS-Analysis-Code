cogapsGeneOntologyPlot <- function(GOEnrichment, top = 5, pattern, col.low = "brown1", 
          col.mid = "floralwhite", col.high = "dodgerblue", 
          min.size = 2, max.size = 10) 
{
  GOdata <- GOEnrichment[1:top,]
  Pattern <- as.factor(paste("Pattern", rep(pattern, top)))
  GOdata <- cbind(GOdata, Pattern)
  
  colorPal <- grDevices::colorRampPalette(c(col.low, col.mid, 
                                            col.high))
  p <- ggplot(data = GOdata, mapping = aes_string(x = Pattern, 
                                                  y = as.factor(GOdata$name))) + geom_point(mapping = aes_string(size = GOdata$Binom_Fold_Enrichment, 
                                                                                                color = GOdata$Binom_Adjp_BH)) + scale_radius(range = c(min.size, 
                                                                                                                                           max.size)) + scale_colour_gradientn(colors = colorPal(10)) + 
    theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                       axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
}
