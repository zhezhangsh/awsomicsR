PlotMA <- function(x, y, xlab='', ylab='', title='', plotly=FALSE, max.p=5000) {
  if (is.na(xlab) | xlab=='') xlab <- 'Log2(average expression)';
  if (is.na(ylab) | ylab=='') ylab <- 'Log2(fold change)';
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
    cut <- rev(sort(abs(y)))[min(max(100, max.p), length(y))]; 
    sel <- abs(y) >= cut;
    sz  <- sqrt(abs(y/z))*10*(5-max(1, min(4, round(log10(length(x))))));
    mrk <- list(size = sz[sel], symbol=0, line=list(width=.8, color='rgba(0, 0, 0, .3)'));
    d   <- data.frame(x=x, y=y, txt=names(x))[sel, ];
    
    plot_ly(data=d, x=~x, y=~y, type='scatter', mode='markers', text=~txt, hoverinfo="text", marker=mrk) %>%
      add_lines(x=lox, y=loy, text='')  %>%
      layout(
        showlegend=FALSE,
        xaxis = list(title=xlab, range=range(x), zeroline=FALSE, showgrid=TRUE, showline=TRUE, showticklabels=TRUE),
        yaxis = list(title=ylab, range=c(-1.05*z, 1.05*z), zeroline=TRUE, showgrid=TRUE, showline=TRUE, showticklabels=TRUE),
        shapes = list(list(type = "rect", xref = 'x', x0 = min(x), x1 = max(x), yref = 'y', y0 = -cut, y1 = cut, 
                           fillcolor = 'rgb(38,78,152)', line = list(width=0), opacity = .5)));
  } else {
    if (title=='' | is.na(title)) par(mar=c(5,5,2,2)) else par(mar=c(5,5,3,2));
    plot(x, y, pch=18, col='#4444DD88', cex=.75, xlab=xlab, ylab=ylab, ylim=c(-z, z), main = title, cex.lab=2);
    abline(h=0, lwd=2, col='#FF8888');
    lines(lox, loy, lwd=2, col='#88FF88');
    box();
  }
}