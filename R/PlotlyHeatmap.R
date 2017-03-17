PlotlyHeatmap <- function(d) {
  require(awsomics); 
  
  if (nrow(d) > 2) d <- d[hclust(as.dist(1-cor(t(d))))$order, , drop=FALSE];
  
  xa <- list(title = "", zeroline = FALSE, showgrid = FALSE, 
             showline = FALSE, tickangle = -30, tickmode = "array", 
             tickvals = 0:(ncol(d) - 1), ticktext = colnames(d));
  ya <- list(title = "", zeroline = FALSE, showgrid = FALSE, 
             showline = FALSE, tickangle = +30, tickmode = "array", 
             tickvals = 0:(nrow(d) - 1), ticktext = rownames(d));
  
  ml <- max(60, 4 + 8 * max(nchar(rownames(d))));
  mb <- max(60, 4 + 8 * max(nchar(colnames(d)))); 
  
  plot_ly(z = d, type = "heatmap", colors = colorRamp(c("blue", "pink", "red"))) %>%
    layout(xaxis = xa, yaxis = ya, margin = list(l = ml, b = mb, t = 60));
}
