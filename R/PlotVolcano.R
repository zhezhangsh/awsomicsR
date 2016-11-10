PlotVolcano <- function(fc, p, title='', plotly=FALSE) {
  
  i  <- which(!is.na(fc) & !is.na(p));
  fc <- fc[i];
  p  <- p[i]; 
  
  y <- -1*log10(p);
  y[y==Inf] <- floor(max(y[y<Inf])) + 1.25; 
  z  <- sqrt(abs(fc*y));
  cx <- z/max(z);
  mx <- max(abs(fc), na.rm=TRUE);
  xlim <- c(-mx, mx);
  ylim <- c(0, max(y)+0.5); 
  
  if (plotly) {
    require(plotly);
    sz  <- max(1, 5-max(1, min(4, round(log10(length(fc))))));
    mrk <- list(size = sz, symbol=2, line=list(width=.8, color='rgba(0, 0, 0, .3)'));
    d   <- data.frame(X=fc, Y=y);
    
    p <- ggplot(data=d, aes(x=X, y=Y)) + 
      theme(legend.position="none") + 
      labs(x="Log2(fold change)",y="-Log10(p value)") +
      geom_point(aes(text=names(fc)), size=0.8*sz, col='#DD0000DD');
    ggplotly(p); 
    
    plot_ly(data=d, x=~fc, y=~y, type='scatter', mode='markers', text=rownames(d), hoverinfo="text", marker=mrk) %>%
      layout(
        showlegend=FALSE,
        xaxis = list(title='Log2(fold change)', range=xlim, zeroline=TRUE, showgrid=TRUE, showline=TRUE, showticklabels=TRUE),
        yaxis = list(title='-Log10(p value)', range=ylim, zeroline=FALSE, showgrid=TRUE, showline=TRUE, showticklabels=TRUE));
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
