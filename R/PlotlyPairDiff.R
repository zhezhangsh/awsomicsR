PlotlyPairDiffTypes<-function() c("Scatter Plot", "MA Plot", "Bar Plot", "Box Plot", "Density Plot")

PlotlyPairDiff<-function(d, type, subset=list(), highlight=c()) {
  require(plotly);
  
  type <- type[1]; 
  high <- intersect(highlight, rownames(d));
  sset <- lapply(subset, function(x) intersect(x, rownames(d))); 
  
  mx <- max(d, na.rm=TRUE);
  mn <- min(d, na.rm=TRUE); 
  
  if (type[1] %in% PlotlyPairDiffTypes()[1:2]) {
    if (type[1] == PlotlyPairDiffTypes()[1]) {
      labs <- colnames(d);
      x <- d[, 1]; 
      y <- d[, 2]; 
      names(x) <- names(y) <- rownames(d); 
      if (mn != 0) xlim <- ylim <- c(mn-0.05*(mx-mn), mx+0.05*(mx-mn)) else xlim <- ylim <- c(mn, 1.05*mx);
    }  else {
      labs <- c(paste('Mean(', paste(rev(colnames(d)), collapse=' & '), ')', sep=''), paste(rev(colnames(d)), collapse=' - ')); 
      x <- rowMeans(d, na.rm=TRUE);
      y <- d[, 2] - d[, 1]; 
      names(x) <- names(y) <- rownames(d); 
      if (mn != 0) xlim <- c(mn-0.05*(mx-mn), mx+0.05*(mx-mn)) else xlim <- c(mn, 1.05*mx);
      ylim <- c(-1.05*max(y, na.rm=TRUE), 1.05*max(y, na.rm=TRUE)); 
    }
    text <- rownames(d); 

    ns <- length(sset)+length(high); 
    if (ns > 0) {
      nc <- max(nchar(c(names(sset), names(high)))); 
      sz <- min(15, ceiling(400/nc));
      cl <- '#A8A8A8';
      xs <- max(0.6, 1 - nc/250);
    } else {
      sz <- 0;
      cl <- '#A8A8FF';
      xs <- 1; 
    } 
    
    p <- PlotlyContourScatter(x, y, labs[1], labs[2], txt=text, col.mark=cl, marker.line = FALSE, 
                              xlim=xlim, ylim=ylim, colorscale = 'Greys', reversescale = TRUE);

    if (type[1] == PlotlyPairDiffTypes()[1]) {
      p <- add_lines(p, x=xlim, y=ylim, showlegend = FALSE); 
    } else {
      lo  <- lowess(y~x);
      lox <- lo[[1]];
      loy <- lo[[2]]; 
      list(list(lox, xlim), list(loy, c(0, 0))); 
      
      p <- add_lines(p, x=xlim, y=c(0, 0), showlegend = FALSE); 
      p <- add_lines(p, x=lox, y=loy, showlegend = FALSE); 
    };
    
    p$x$attrs[[2]]$showlegend <- FALSE;

    if (length(sset) > 0) {
      for (i in 1:length(sset)) {
        p <- add_trace(p, x=x[sset[[i]]], y=y[sset[[i]]], type='scatter', mode='marker', hoverinfo = 'text', 
                       text=sset[[i]], marker=list(size=8, symbol=22), name=names(sset)[i]);
        p <- layout(p, showlegend=TRUE, legend=list(x=xs, y=0, font=list(size=sz)))
      }
    };
    if (length(high) > 0) {
      for (i in 1:length(high)) {
        p <- add_trace(p, x=x[high[i]], y=y[high[i]], type='scatter', hoverinfo = 'text', 
                       text=high[i], marker=list(size=10, symbol=18), name=high[i]);
        p <- layout(p, showlegend=TRUE, legend=list(x=xs, y=0, font=list(size=sz)))
      }
    };
    
    p; 
  } else if (type[1] == PlotlyPairDiffTypes()[3]) {
    g <- rep('All genes', nrow(d)); 
    v <- d[, 2] - d[, 1]; 
    y <- paste('Mean(', paste(rev(colnames(d)), collapse=' - '), ')', sep=''); 
    names(v) <- rownames(d); 
    
    if (length(sset) > 0) {
      v <- c(v, v[unlist(sset, use.names=FALSE)]); 
      g <- c(g, rep(names(sset), sapply(sset, length))); 
    }
    p <- PlotlyBar(v, group = g, ylab = y); 
    
    if (length(sset) > 0) {
      nc <- max(nchar(c(names(sset)))); 
      sz <- min(15, ceiling(300/nc)); 
      p <- layout(p, xaxis=list(tickfont = list(size=sz)), margin=list(b=nc*sz/2));
    }
    
    if (length(high) > 0) {
      for (i in 1:length(high)) {
        p <- add_trace(p, x=1, y=v[high[i]], type='scatter', hoverinfo = 'text', 
                       text=high[i], marker=list(size=10, symbol=18), name=high[i]);
        p <- layout(p, showlegend=TRUE); 
      };
      p$x$attrs[[1]]$showlegend <- FALSE;
    }

    p; 
  } else if (type[1] == PlotlyPairDiffTypes()[4]) {
    y <- paste(rev(colnames(d)), collapse=' - '); 
    v <- d[, 2] - d[, 1]; 
    names(v) <- rownames(d); 
    tck <- 'All genes'; 
    
    p <- plot_ly(y=v, type='box', name='All genes', showlegend=FALSE, hoverinfo='none'); 
    if (length(sset) > 0) {
      for (i in 1:length(sset)) {
        p <- add_trace(p, y=v[sset[[i]]], name=names(sset)[i], showlegend=FALSE, hoverinfo='none');
      }
      tck <- c(tck, names(sset)); 
    }
    if (length(high) > 0) {
      for (i in 1:length(high)) {
        p <- add_trace(p, y=v[high[i]], type='scatter', showlegend=TRUE, hoverinfo='name',
                       text=high[i], marker=list(size=10, symbol=18), name=high[i]);
      };
      p$x$attrs[[1]]$showlegend <- FALSE;
    }
    
    nc <- max(nchar(tck)); 
    sz <- min(15, ceiling(300/nc));

    p <- layout(p, margin=list(b=min(250, nc*sz/2.5)), yaxis = list(title = y),
                xaxis = list(tickangle = -30, tickmode = 'array', tickvals = tck, tickfont = list(size=sz))); 
    p; 
  } else if (type[1] == PlotlyPairDiffTypes()[5]) {
    x <- paste(rev(colnames(d)), collapse=' - '); 
    v <- d[, 2] - d[, 1]; 
    names(v) <- rownames(d); 
    den <- density(v); 
    if (length(sset) < 3) fll <- 'tozeroy' else fll <- 'none'
    p <- plot_ly(x=den$x, y=den$y, type='scatter', mode='line', fill = fll, name='All genes'); 
    
    ns <- length(sset)+length(high); 
    if (ns > 0) {
      nc <- max(nchar(c(names(sset), names(high)))); 
      sz <- min(15, ceiling(400/nc));
      cl <- '#A8A8A8';
      xs <- max(0.6, 1 - nc/250);
    } else {
      sz <- 0;
      cl <- '#A8A8FF';
      xs <- 1; 
    };
    
    if (length(sset) > 0) {
      for (i in 1:length(sset)) {
        den <- density(v[sset[[i]]]); 
        p <- add_trace(p, x=den$x, y=den$y, name=names(sset)[i], showlegend=TRUE);
      }
    };
    
    if (length(high) > 0) {
      for (i in 1:length(high)) {
        p <- add_trace(p, x=v[high[i]], y=0, type='scatter', hoverinfo = 'text', 
                       text=high[i], marker=list(size=10, symbol=18), name=high[i]);
        p <- layout(p, showlegend=TRUE, legend=list(x=xs, y=1, legendgroup = 'g1', font=list(size=sz)))
      }; 
    }; 
    
    p <- layout(p, xaxis=list(title=x), yaxis=list(title='Density'),
                legend=list(x=xs, y=1, font=list(size=sz), legendgroup = 'g2')); 
    p;
  } else {
    plotly_empty() %>% layout(title=paste('Unknown plot type:', type), margin=list(t=100));
  }
}