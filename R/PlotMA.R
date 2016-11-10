PlotMA <- function(x, y, xlab='', ylab='', title='', plotly=FALSE) {
  if (is.na(xlab) | xlab=='') xlab <- 'Log2(average expression)';
  if (is.na(ylab) | ylab=='') ylab <- 'Log2(fold change)';
  z <- max(abs(y), na.rm=TRUE);
  lo  <- lowess(y~x);
  lox <- lo[[1]];
  loy <- lo[[2]]; 
  
  if (plotly) {
    require(plotly); 
    sz  <- 6*(5-max(1, min(4, round(log10(length(x))))));
    mrk <- list(size = sz, symbol=0, line=list(width=.8, color='rgba(0, 0, 0, .3)'));
    d   <- data.frame(x=x, y=y, lox=lox, loy=loy);
    
    plot_ly(data=d, x=~x, y=~y, type='scatter', mode='markers', text=names(x), hoverinfo="text", marker=mrk) %>%
      add_trace(x=lox, y=loy, mode='lines') %>%
      layout(
        showlegend=FALSE,
        xaxis = list(title=xlab, zeroline=FALSE, showgrid=TRUE, showline=TRUE, showticklabels=TRUE),
        yaxis = list(title=ylab, range=c(-z, z), zeroline=TRUE, showgrid=TRUE, showline=TRUE, showticklabels=TRUE));
  } else {
    if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
    plot(x, y, pch=18, col='#4444DD88', cex=.75, xlab=xlab, ylab=ylab, ylim=c(-z, z), main = title, cex.lab=2);
    abline(h=0, lwd=2, col='#FF8888');
    lines(lox, loy, lwd=2, col='#88FF88');
    box();
  }
}