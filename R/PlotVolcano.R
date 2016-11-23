PlotVolcano <- function(fc, p, title='', plotly=FALSE, npoints=round(length(fc)/20)) {
  require(awsomics);
  
  i  <- which(!is.na(fc) & !is.na(p) & fc<Inf & fc>-Inf);
  fc <- fc[i];
  p  <- p[i]; 
  
  y <- -1*log10(p);
  y[y==Inf] <- floor(max(y[y<Inf])) + 1.25; 
  z  <- sqrt(fc^2+y^2);
  cx <- z/max(z);
  mx <- max(abs(fc), na.rm=TRUE);
  xlim <- c(-mx, mx);
  ylim <- c(0, max(y)+0.1); 
  
  if (plotly) {
    require(plotly);
    sz <- 10*cx*max(1, 5-max(1, min(4, round(log10(length(fc))))));
    sz <- c(sz, 0,0,0,0);

    x <- c(fc, xlim, xlim);
    y <- c(y, ylim[1], ylim[2], ylim[1], ylim[2]); 
    
    PlotlyContourScatter(x, y, xlab='Log2(fold change)', ylab='-Log10(p value)', xlim=xlim, ylim=ylim,
                         reversescale = FALSE, lines=list(c(0, 0), c(0, ylim[2])),
                         colorscale=list(list(0, '#FFFFFF'), list(.1, '#FF4444'), list(1, '#FF8888')),
                         txt=names(fc), npoints=npoints, col.mark = '#FF8888', size=sz, symbol=2, marker.line=FALSE);
   } else {
    if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
    
    plot(fc, y, main=title, pch=18, col='#FF6666DD', cex=cx, xlab='Log2(fold change)', ylab='-Log10(p value)', 
         cex.lab=2, xlim=xlim, ylim=ylim, yaxs='i', axes=FALSE); 
    axis(1);
    if(min(p) > 0) axis(2) else {
      axis(2, at=0:(max(y)-1));
      axis(2, at=max(y), label=0);
    }
    abline(v=0, col=1, lty=3);
    box();
  }
}
