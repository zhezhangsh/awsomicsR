PlotlyPairDiffTypes<-function() c("Scatter Plot", "MA Plot", "Box Plot", "Bar Plot", "Density Plot")

PlotlyPairDiff<-function(d, type, subset=list(), highlight=c()) {
  require(plotly);
  
  type <- type[1]; 
  labs <- colnames(d); 
  text <- rownames(d); 
  
  mx <- max(d, na.rm=TRUE);
  mn <- min(d, na.rm=TRUE); 
  
  high <- intersect(highlight, rownames(d));
  sset <- lapply(subset, function(x) intersect(x, rownames(d))); 
  
  if (type[1] == PlotlyPairDiffTypes()[1]) {
    p <- PlotlyContourScatter(
      d[, 1], d[, 2], labs[1], labs[2], txt=text, col.mark='#C8C8C8', marker.line = FALSE, 
      xlim=c(mn, mx), ylim=c(mn, mx), colorscale = 'Greys', reversescale = TRUE);

    p <- add_lines(p, x=c(mn, mx), y=c(mn, mx), showlegend = FALSE, line=list(color='#888888'));
    
    p$x$attrs[[2]]$showlegend <- FALSE;

    sz <- ceiling(400/max(nchar(c(names(sset), names(high))))); 
    ns <- length(sset)+length(high)
    
    if (length(sset) > 0) {
      for (i in 1:length(sset)) {
        p <- add_trace(p, x=d[sset[[i]], 1], y=d[sset[[i]], 2], type='scatter', 
                         marker=list(size=8, symbol=22), name=names(sset)[i]);
        p <- layout(p, showlegend=TRUE, legend=list(x=0.8, y=0, font=list(size=sz)))
      }
    }
    if (length(high) > 0) {
      for (i in 1:length(high)) {
        p <- add_trace(p, x=d[high[i], 1], y=d[high[i], 2], type='scatter', 
                       marker=list(size=10, symbol=18), name=high[i]);
        p <- layout(p, showlegend=TRUE, legend=list(font=list(size=sz)))
      }
    }
    
    p; 
  } else {
    plotly_empty() %>% layout(title=paste('Unknown plot type:', type), margin=list(t=100));
  }
}