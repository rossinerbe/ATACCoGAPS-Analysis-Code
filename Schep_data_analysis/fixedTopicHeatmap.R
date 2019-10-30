heatmapTopics = function (object, method = "Z-score", colorBy = NULL, colVars = NULL, 
          col.low = "floralwhite", col.mid = "pink", col.high = "red", 
          select.cells = NULL, ...) 
{
  if (!"fastcluster" %in% installed.packages()) {
    stop("Please, install fastcluster: \n install.packages(\"fastcluster\")")
  }
  else {
    require(fastcluster)
  }
  if (!"ComplexHeatmap" %in% installed.packages()) {
    stop("Please, install ComplexHeatmap: source(\"https://bioconductor.org/biocLite.R\") \nbiocLite(\"ComplexHeatmap\")")
  }
  else {
    require(ComplexHeatmap)
  }
  if (length(object@selected.model) < 1) {
    stop("Please, run selectModel() first.")
  }
  topic.mat <- modelMatSelection(object, "cell", method)
  rownames(topic.mat) <- paste("Topic", seq(1, nrow(topic.mat)))
  colnames(topic.mat) <- object@cell.names #maybe a point of issue
  if (!is.null(select.cells)) {
    if (length(select.cells) > 1) {
      topic.mat <- topic.mat[, select.cells]
      object.cell.data <- object@cell.data[select.cells, 
                                           ]
    }
    else {
      stop("Only a cell name has been provided. Please select a group of cells and provide them as a vector.")
    }
  }
  else {
    object.cell.data <- object@cell.data
  }
  cl.cells <- fastcluster::hclust.vector(t(topic.mat), method = "ward", 
                                         metric = "euclidean")
  dd.cells <- as.dendrogram(cl.cells)
  colorPal <- grDevices::colorRampPalette(c(col.low, col.mid, 
                                            col.high))
  if (is.null(colorBy)) {
    heatmap <- ComplexHeatmap::Heatmap(data.matrix(topic.mat), 
                                       col = colorPal(20), cluster_columns = dd.cells, name = method, 
                                       show_column_names = FALSE, show_row_names = TRUE, 
                                       heatmap_legend_param = list(legend_direction = "horizontal", 
                                                                   legend_width = unit(5, "cm"), title_position = "topcenter"), 
                                       column_title = "Topic contribution per cell", 
                                       column_title_gp = gpar(fontface = "bold"), 
                                       ...)
    ComplexHeatmap::draw(heatmap, heatmap_legend_side = "bottom")
  }
  else {
    for (variable in colorBy) {
      if (is.null(colVars[[variable]])) {
        colVars[[variable]] <- setNames(.distinctColorPalette(length(unique(object@cell.data[, 
                                                                                             variable]))), as.vector(sort(unique(object@cell.data[, 
                                                                                                                                                  variable]))))
        cellColor <- setNames(colVars[[variable]][object.cell.data[, 
                                                                   variable]], rownames(object.cell.data))
      }
    }
    annotation <- ComplexHeatmap::HeatmapAnnotation(df = object.cell.data[,colorBy, drop = FALSE], col = colVars, which = "column")
    heatmap <- ComplexHeatmap::Heatmap(data.matrix(topic.mat), 
                                       col = colorPal(20), cluster_columns = dd.cells, name = method, 
                                       show_column_names = FALSE, show_row_names = TRUE, 
                                       top_annotation = annotation, heatmap_legend_param = list(legend_direction = "horizontal", 
                                                                                                legend_width = unit(5, "cm"), title_position = "topcenter"), 
                                       column_title = "Topic contribution per cell", 
                                       column_title_gp = gpar(fontface = "bold") 
                                    )
    ComplexHeatmap::draw(heatmap, heatmap_legend_side = "bottom", 
                         annotation_legend_side = "right")
  }
}
