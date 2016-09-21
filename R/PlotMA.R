PlotMA <- function(x, y, xlab='', ylab='', title='') {
  if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
  z <- max(y, na.rm=TRUE);
  if (is.na(xlab) | xlab=='') xlab <- 'Log2(average expression)';
  if (is.na(ylab) | ylab=='') ylab <- 'Log2(fold change)';
  plot(x, y, pch=18, col='#4444DD88', cex=.75, xlab=xlab, ylab=ylab, ylim=c(-z, z), main = title, cex.lab=2);
  abline(h=0, lwd=2, col='#FF8888');
  lines(lowess(y~x), lwd=2, col='#88FF88');
  box();
}