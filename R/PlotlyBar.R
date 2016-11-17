PlotlyBar <- function(d, group=NA, col=NA, title='', xlab='', ylab='') {
  require(plotly);
  
  # Format input data
  if (is.data.frame(d)) d <- as.matrix(d); 
  if (is.vector(d)) {
    m <- names(d); 
    d <- matrix(d, nc=1); 
    rownames(d) <- m;
  }
  
  if (!identical(NA, group) & length(group) == nrow(d)) {
    g <- unique(group);
    x <- lapply(g, function(g) d[group==g, , drop=FALSE]);
    d <- t(sapply(x, function(x) colMeans(x, na.rm=TRUE)));
    s <- lapply(x, function(x) apply(x, 2, function(x) sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)]))));
    rownames(d) <- names(s) <- g;
  } else s <- as.list(rep(0, ncol(d))); 

  # Parameters
  rnm <- rownames(d); 
  cnm <- colnames(d);
  mgb <- 7.5*max(nchar(rnm));
  bgp <- min(0.5, 0.1+round(1/nrow(d), 1)); 
  wid <- min(5, ceiling(100/length(d)));  # width of error bars
  
  col[grep('^#', col)] <- substr(col[grep('^#', col)], 1, 7); 
  if (length(d) > 32) ln <- list() else ln <- list(color = 'rgb(8,48,107,.5)', width = 1); 
  if (identical(NA, col) | length(col)!=ncol(d)) cl <- rep('', ncol(d)) else cl <- col;
  if (!identical(NA, xlab) & xlab!='') mgb <- mgb + 10;
  
  ############################################################################################
  p <- plot_ly(x = rnm, y = d[, 1], type = 'bar', name = cnm[1], text = rnm,
               error_y = list(value = s[[i]], color = '#000000', thickness=1, width=wid, opacity=.9), 
               marker = list(line=ln, color=cl[1])) %>%
    layout(
      barmode = 'group', bargap=bgp, margin = list(b = mgb), title = title,
      xaxis = list(title = xlab, tickangle = -45),
      yaxis = list(title = ylab)
    );
  if (ncol(d) > 1) { 
    for (i in 2:ncol(d)) 
      p <- add_trace(p, y = d[, i], name = cnm[i], text = rnm, 
                     marker = list(line = ln, color = cl[i]), 
                     error_y = list(value = s[[i]]));
  };
  
  p;
}