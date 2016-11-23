PlotPA <- function(x, p, xlab='', ylab='', title='', plotly=FALSE, npoints=round(length(fc)/20)) {
  require(awsomics);
  
  if (is.na(xlab) | xlab=='') xlab <- 'Log2(average expression)';
  if (is.na(ylab) | ylab=='') ylab <- 'Log10(p value)';
  
  i <- which(!is.na(x) & !is.na(p) & p>=0 & p<=1 & x>-Inf & x<Inf);
  x <- x[i];
  p <- p[i]; 
  y <- -1*log10(p);
  z <- max(y, na.rm=TRUE);
  
  y[y==Inf] <- floor(max(y[y<Inf])) + 1.25; 
  
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
    yl <- c(0, 0.2+max(abs(y), na.rm=TRUE)); 
    xl[2] <- 0.2+xl[2];
    sz <- abs(y/z)*10*(5-max(1, min(4, round(log10(length(x))))));
    sz <- c(sz, 0,0,0,0);
    x <- c(x, xl, xl);
    y <- c(y, yl[1], yl[2], yl[1], yl[2]); 
    
    PlotlyContourScatter(x, y, xlab=xlab, ylab=ylab, xlim=xl, ylim=yl, reversescale = FALSE, line = list(x=lox, y=loy),
                         colorscale=list(list(0, '#FFFFFF'), list(.1, '#4444FF'), list(1, '#8888FF')),txt=names(x), 
                         npoints=npoints, col.mark = '#8888FF', size=sz, symbol=0, marker.line=FALSE);
  } else {
    if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
    plot(x, y, pch=18, col='#4444DD88', cex=.75, xlab=xlab, ylab=ylab, ylim=c(0, 1.05*z), main = title, cex.lab=2);
    abline(h=0, lwd=2, col='#FF8888');
    lines(lox, loy, lwd=2, col='#88FF88');
    box();
  }
}