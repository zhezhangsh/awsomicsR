PlotMA <- function(x, y, nms, xlab='', title='') {
  if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
  z <- max(y, na.rm=TRUE);
  plot(x, y, pch=18, col='#66666688', cex=.5, ylab=paste(nms[2], nms[1], sep=' - '), xlab='Log2(average expression)', 
       ylim=c(-z, z), main = title, cex.lab=2);
  abline(h=0, lwd=2, col='#FF8888');
  lines(lowess(y~x), lwd=2, col='#8888FF');
  box();
}