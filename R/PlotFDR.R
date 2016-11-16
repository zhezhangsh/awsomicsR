PlotFDR <- function(q, col='#3333FF', xlab='', ylab='', title='', plotly=FALSE) {
  
  q <- q[!is.na(q)];
  q <- round(q, 2); 
  n <- sapply(seq(min(q), 1, 0.01), function(x) length(q[q<=x]));
  
  title <- title[1];
  xlab  <- xlab[1];
  ylab  <- ylab[1];
  
  if (is.na(title) | title=='') par(mar=c(5,5,2,2)) else par(mar(c(5,5,3,2)));
  if (is.na(xlab) | xlab=='') xlab <- 'False discovery rate';
  if (is.na(ylab) | ylab=='') ylab <- 'Number of genes';
  
  if (plotly) {
    require(plotly);
    v <- seq(min(q), 1, 0.01);
    plot_ly(x=v, y=n, type='scatter', mode='lines', line=list(color = "#885EFC", width=5)) %>%
      layout(
        showlegend=FALSE,
        xaxis = list(title=xlab, zeroline=FALSE, showgrid=FALSE, showline=TRUE, showticklabels=TRUE),
        yaxis = list(title=ylab, zeroline=FALSE, showgrid=TRUE, showline=TRUE, showticklabels=TRUE, type='log'));
  } else {
    plot(1, type='n', log='y', xlab=xlab, ylab=ylab, cex.lab=2, xaxs='i', xlim=c(0, 1), ylim=c(1, length(q)), main=title);
    abline(v=seq(0, 1, .05), lty=2, col=8);
    lines(seq(min(q), 1, 0.01), n, lwd=2, col=col);
    box();    
  }

}