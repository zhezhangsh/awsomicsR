PlotlyBar <- function(d, group=NA, col=NA, title='', xlab='', ylab='') {
  require(plotly);
  
  # Format input data
  if (is.data.frame(d)) d <- as.matrix(d); 
  if (is.vector(d)) {
    m <- names(d); 
    d <- matrix(d, nc=1); 
    rownames(d) <- m;
  }
  if (is.null(rownames(d))) rownames(d) <- 1:nrow(d);
  
  if (!identical(NA, group) & length(group) == nrow(d)) {
    g <- unique(group);
    x <- lapply(g, function(g) d[group==g, , drop=FALSE]);
    d <- do.call('rbind', lapply(x, function(x) colMeans(x, na.rm=TRUE)));
    s <- do.call('cbind', lapply(x, function(x) apply(x, 2, function(x) sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))));
    w <- min(5, ceiling(100/length(d)));  # width of error bars
    rownames(d) <- g;
    y <- apply(s, 1, function(s) {
      s[is.na(s)] <- 0;
      list(array = s, type = 'data', color = '#000000', thickness = 1, width = w, opacity = .9); 
    });
  } else y <- lapply(1:ncol(d), function(i) list());
  names(y) <- colnames(d);
  
  # Parameters
  rnm <- rownames(d); 
  cnm <- colnames(d);
  mgb <- 2.5 + 7.5*max(nchar(rnm));
  bgp <- min(0.5, 0.1+round(1/nrow(d), 1)); 
  
  col[grep('^#', col)] <- substr(col[grep('^#', col)], 1, 7); 
  if (length(d) > 32) ln <- list() else ln <- list(color = 'rgb(8,48,107,.5)', width = 1); 
  if (!identical(NA, col) & length(col)>=ncol(d)) cl <- col[1:ncol(d)] else cl <- rep('', ncol(d));
  if (!identical(NA, xlab) & xlab!='') mgb <- mgb + 10;
  
  ############################################################################################
  p <- plot_ly(x = rnm, y = d[, 1], type = 'bar', name = cnm[1], text = rnm, error_y = y[[1]],
               marker = list(line=ln, color=cl[1])); 
  p <- layout(p, barmode = 'group', bargap=bgp, margin = list(b = mgb), title = title,
              xaxis = list(title = xlab, tickangle = -45), yaxis = list(title = ylab));

  if (ncol(d) > 1) { 
    for (i in 2:ncol(d)) 
      p <- add_trace(p, y = d[, i], name = cnm[i], text = rnm, error_y = y[[i]],
                     marker = list(line = ln, color = cl[i]));
  };
  
  p;
}