PlotMA <- function(x, y, xlab='', ylab='', title='', plotly=FALSE, npoints=1000) {
  require(awsomics);

  if (is.na(xlab) | xlab=='') xlab <- 'Log2(average expression)';
  if (is.na(ylab) | ylab=='') ylab <- 'Log2(fold change)';
  
  i <- which(!is.na(x) & !is.na(y) & x>-Inf & x<Inf & y>-Inf & y<Inf);
  x <- x[i];
  y <- y[i]; 
  
  z <- max(abs(y), na.rm=TRUE);
  lo  <- lowess(y~x);
  lox <- lo[[1]];
  loy <- lo[[2]]; 
  if (length(x) >= 1000) {
    i <- round(seq(1, length(y), length.out = 1000));
    lox <- lox[i];
    loy <- loy[i]; 
  }  
  
  if (plotly) {
    require(plotly); 
    
    xl <- range(x, na.rm=TRUE);
    yl <- c(-max(abs(y), na.rm=TRUE), max(abs(y), na.rm=TRUE)); 
    sz <- abs(y/z)*5*(5-max(1, min(4, round(log10(length(x))))));

    PlotlySmoothScatter(x, y, xlab, ylab, xl, yl, size=sz, symbol = 0, npoints = npoints, line = list(x=lox, y=loy),
                        zero.line = c(FALSE, TRUE), col.mark = '#0000BBBB', col.shape = '#0000BB'); 
  } else {
    if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
    plot(x, y, pch=18, col='#4444DD88', cex=.75, xlab=xlab, ylab=ylab, ylim=c(-z, z), main = title, cex.lab=2);
    abline(h=0, lwd=2, col='#FF8888');
    lines(lox, loy, lwd=2, col='#88FF88');
    box();
  }
}