PlotlyContourScatter <- 
  function(x, y, xlab, ylab, xlim=range(x, na.rm=TRUE), ylim=range(y, na.rm=TRUE), ncontours=400,  
           colorscale='Greys', reversescale=TRUE, log.axis=c(FALSE, FALSE), lines=list(),  
           symbol=0, txt=names(x), npoints=2000, size=rep(5, length(x)), col.mark='#2222FFFF', marker.line=TRUE) {
  
    require(plotly); 
    require(KernSmooth);
    require(gplots);
    
    npoints <- max(1, npoints);
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
    col.mark <- paste('rgb(', paste(col2rgb(col.mark, FALSE)[, 1], collapse=','), ')', sep='');
    if (marker.line) w <- 0.5 else w <- 0;
    mrk <- list(size = size[sel], symbol=symbol, color=col.mark, opacity=0.5,
                line=list(width=w, color='rgba(0, 0, 0, .5)'));
    
    # axes
    xa <- list(title=xlab, range=xlim, zeroline=FALSE, showgrid=FALSE, showline=TRUE, showticklabels=TRUE);
    ya <- list(title=ylab, range=ylim, zeroline=FALSE, showgrid=FALSE, showline=TRUE, showticklabels=TRUE);
    if (log.axis[1]) xa$type <- 'log';
    if (log.axis[2]) ya$type <- 'log';
    
    p <- plot_ly(x=x, y=y, type='histogram2dcontour', ncontours=ncontours, showscale=FALSE, hoverinfo='none',
                 colorscale = colorscale, reversescale=reversescale, contours=list(showlines=FALSE));
    p <- add_markers(p, x = x[sel], y = y[sel], type='scatter', text=txt[sel], hoverinfo="text", marker=mrk);
    
    # Add line
    if (length(lines)==2) {
      xs <- lines[[1]];
      ys <- lines[[2]];
      if (length(xs)==length(ys)) {
        if (!is.list(xs)) xs <- list(x=xs);
        if (!is.list(ys)) ys <- list(y=ys);
        for (i in 1:length(xs)) p <- add_lines(p, x=xs[[i]], y=ys[[i]], text='');
      }
    };
    
    p <- layout(p, xaxis=xa, yaxis=ya, showlegend=FALSE); 
    p; 
  }
  