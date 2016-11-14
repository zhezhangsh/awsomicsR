PlotlySmoothScatter <- function(x, y, xlab, ylab, xlim, ylim, size=rep(10, length(x)), symbol=0, txt=names(x), npoints=1000, line=list(), 
                                zero.line=c(TRUE, TRUE), log.axis=c(FALSE, FALSE), col.mark='#2222FFCC', col.shape='#2222FFFF', make.plot=TRUE) {
  require(plotly); 
  require(KernSmooth);
  require(gplots);
  
  rng.x <- range(x, na.rm = TRUE);
  rng.y <- range(y, na.rm = TRUE); 
  bandw <- c((rng.x[2]-rng.x[1])/100, (rng.y[2]-rng.y[1])/100);
  
  est <- bkde2D(cbind(x, y), bandwidth = bandw, gridsize=c(100, 100));
  
  x1 <- rep(est$x1, length(est$x2));
  y1 <- rep(est$x2, each=length(est$x1));
  z1 <- sqrt(as.vector(est$fhat)); 
  
  # Colors
  c0 <- colorpanel(256, '#FFFFFFFF', col.shape);
  mx <- rev(sort(z1))[ceiling(length(z1)/1000)]; 
  mn <- 0.01*mx;
  z1[z1<mn] <- 0; 
  z2 <- floor((z1-mn)/(mx-mn)*256)+2;
  z2 <- pmax(1, pmin(256, z2)); 
  cs <- c0[z2]; 

  # block width
  wx <- mean(est[[1]][-1]-est[[1]][-length(est[[1]])])/2;
  wy <- mean(est[[2]][-1]-est[[2]][-length(est[[2]])])/2;

  df <- data.frame(X=x1, Y=y1, Color=cs, stringsAsFactors = FALSE); 
  fl <- rev(sort(z2))[1600];
  df <- df[z2>fl & x1>=min(x) & x1<=max(x) & y1>=min(y) & y1<=max(y), , drop = FALSE]; 
  
  # density blocks
  sp <- lapply(1:nrow(df), function(i) {
    list(type='rect', fillcolor=df[i, 3], line=list(width=0), xref='x', yref='y', opacity = 0.5,
         x0=df[i, 1]-wx, x1=df[i, 1]+wx, y0=df[i, 2]-wy, y1=df[i, 2]+wy)
  }); 
  ###############################################
  # Plot points
  xm <- est$x1;
  ym <- est$x2;
  nx <- length(xm); 
  ixm <- 1 + as.integer((nx - 1) * (x - xm[1])/(xm[nx] - xm[1])); 
  iym <- 1 + as.integer((nx - 1) * (y - ym[1])/(ym[nx] - ym[1])); 
  
  dens <- est$fhat
  dens[] <- dens^0.25; 
  sel <- order(dens[cbind(ixm, iym)])[seq_len(npoints)]

  col.mark <- paste('rgba(', paste(col2rgb(col.mark, TRUE)[, 1], collapse=','), ')', sep='');
  mrk <- list(size = size[sel], symbol=symbol, color=col.mark, line=list(width=.8, color='rgba(0, 0, 0, .3)'));
  dd  <- data.frame(X=x, Y=y)[sel, ]; 
  
  # axes
  xa <- list(title=xlab, range=xlim, zeroline=zero.line[1], showgrid=TRUE, showline=TRUE, showticklabels=TRUE);
  ya <- list(title=ylab, range=ylim, zeroline=zero.line[2], showgrid=TRUE, showline=TRUE, showticklabels=TRUE);
  if (log.axis[1]) xa$type <- 'log';
  if (log.axis[2]) ya$type <- 'log';
  
  p <- plot_ly(data=dd, x = ~X, y = ~Y, type='scatter', mode='markers', text=txt[sel], hoverinfo="text", marker=mrk); 
  # Add line
  if (length(line)==2) {
    if (length(line[[1]]==line[[2]])) {
      lx <- line[[1]];
      ly <- line[[2]]; 
      ind <- seq(1, length(lx), length.out = max(length(x), 1000));
      p <- add_lines(p, x=lx[ind], y=ly[ind], text='')
    }
  }
  p <- layout(p, shapes=sp, showlegend=FALSE, xaxis=xa, yaxis=ya); 
  p; 
}