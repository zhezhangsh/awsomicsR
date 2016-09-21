PlotVolcano <- function(fc, p, title='') {
  
  i  <- which(!is.na(fc) & !is.na(p));
  fc <- fc[i];
  p  <- p[i]; 
  
  y <- -1*log10(p);
  y[y==Inf] <- floor(max(y[y<Inf])) + 1.25; 
  z  <- sqrt(abs(fc*y));
  cx <- 2 * z/max(z);
  mx <- max(abs(fc), na.rm=TRUE);
  xlim <- c(-mx, mx);
  ylim <- c(0, max(y)+0.5); 
  
  if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
  
  plot(fc, y, main=title, pch=18, col='#FF6666DD', cex=cx, xlab='Log2(fold change)', ylab='-Log10(p value)', 
       xlim=xlim, ylim=ylim, yaxs='i', axes=FALSE); 
  axis(1);
  if(min(p) > 0) axis(2) else {
    axis(2, at=0:(max(y)-1));
    axis(2, at=max(y), label=0);
  }
  abline(v=0, col=1, lty=3);
  box();
}
