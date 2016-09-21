PlotFDR <- function(q, col='#3333FF', xlab='', ylab='', title='') {
  
  q <- q[!is.na(q)];
  q <- round(q, 2); 
  n <- sapply(seq(min(q), 1, 0.01), function(x) length(q[q<=x]));
  
  title <- title[1];
  xlab  <- xlab[1];
  ylab  <- ylab[1];
  
  if (is.na(title) | title=='') par(mar=c(5,5,2,2)) else par(mar(c(5,5,3,2)));
  if (is.na(xlab) | xlab=='') xlab <- 'False discovery rate';
  if (is.na(ylab) | ylab=='') ylab <- 'Number of genes';
  
  plot(1, type='n', log='y', xlab=xlab, ylab=ylab, cex.lab=2, xaxs='i', xlim=c(0, 1), ylim=c(1, length(q)), main=title);
  abline(v=seq(0, 1, .05), lty=2, col=8);
  lines(seq(min(q), 1, 0.01), n, lwd=2, col=col);
  box();
}