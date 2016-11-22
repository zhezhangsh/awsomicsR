PlotlyContourScatter <- 
  function(x, y, xlab, ylab, xlim=range(x, na.rm=TRUE), ylim=range(y, na.rm=TRUE), ncontours=400,  
           colorscale='Greys', reversescale=TRUE, log.axis=c(FALSE, FALSE), line=list(), zero.line=c(TRUE, TRUE), 
           marker=list(symbol=0, txt=names(x), npoints=2000, size=rep(5, length(x)), col.mark='#2222FFFF')) {
  
    require(plotly); 
    require(KernSmooth);
    require(gplots);
    
    npoints <- max(1, marker$npoints);
    ncontours <- max(10, ncontours);
    
    # Select markers
    rng.x <- range(x, na.rm = TRUE);
    rng.y <- range(y, na.rm = TRUE); 
    bandw <- c((rng.x[2]-rng.x[1])/100, (rng.y[2]-rng.y[1])/100);
    est <- bkde2D(cbind(x, y), bandwidth = bandw, gridsize=c(ncontours, ncontours));
    xm <- est$x1;
    ym <- est$x2;
    nx <- length(xm); 
    ixm <- 1 + as.integer((nx - 1) * (x - xm[1])/(xm[nx] - xm[1])); 
    iym <- 1 + as.integer((nx - 1) * (y - ym[1])/(ym[nx] - ym[1])); 
    dens <- est$fhat;
    dens[] <- dens^0.25; 
    sel <- order(dens[cbind(ixm, iym)])[seq_len(npoints)];
    col.mark <- paste('rgb(', paste(col2rgb(marker$col.mark, FALSE)[, 1], collapse=','), ')', sep='');
    mrk <- list(size = marker$size[sel], symbol=marker$symbol, color=col.mark, opacity=0.5,
                line=list(width=.5, color='rgba(0, 0, 0, .5)'));
    
    # axes
    xa <- list(title=xlab, range=xlim, zeroline=zero.line[1], showgrid=TRUE, showline=TRUE, showticklabels=TRUE);
    ya <- list(title=ylab, range=ylim, zeroline=zero.line[2], showgrid=TRUE, showline=TRUE, showticklabels=TRUE);
    if (log.axis[1]) xa$type <- 'log';
    if (log.axis[2]) ya$type <- 'log';
    
    p <- plot_ly(x=x, y=y, type='histogram2dcontour', ncontours=ncontours, showscale=FALSE, hoverinfo='none',
                 colorscale = colorscale, reversescale=reversescale, contours=list(showlines=FALSE));
    p <- add_markers(p, x = x[sel], y = y[sel], type='scatter', text=marker$txt[sel], hoverinfo="text", marker=mrk);
    
    # Add line
    if (length(line)==2) {
      if (length(line[[1]])==length(line[[2]])) {
        p <- add_lines(p, x=line[[1]], y=line[[2]], text='');
      }
    };
    
    p <- layout(p, xaxis=xa, yaxis=ya, showlegend=FALSE); 
    p; 
  }
  