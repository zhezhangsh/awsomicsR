PlotlyDensity <- function(d, title='', xlab='', ylab=c('Density', 'Percent', 'Count'), cutoff=NA, xzoom=!is.na(cutoff)) {d
  require(plotly); 
  
  d <- d[!is.na(d)]; 
  dens <- density(d, n=2048); 
  
  yl <- tolower(ylab[1]); 
  if (yl == 'count') dens$y <- length(d)*dens$y else
    if (yl == 'percent') dens$y <- 100*dens$y
  
  xaxis <- list(title = xlab, zeroline=FALSE, showgrid=FALSE, showline=TRUE);
  yaxis <- list(title = ylab, zeroline=FALSE, showgrid=FALSE, showline=TRUE, range=c(0, 1.1*max(dens$y)));
  
  if (is.na(cutoff)) {
    plot_ly(x=dens$x, y=dens$y, type='scatter', mode='line', fill="tozeroy") %>%
      layout(title = title, xaxis = xaxis, yaxis = yaxis);     
  } else { # Split plot by cutoff
    x1 <- dens$x[dens$x<=cutoff]; 
    y1 <- dens$y[dens$x<=cutoff];
    x2 <- dens$x[dens$x>=cutoff];
    y2 <- dens$y[dens$x>=cutoff]; 
    x1[length(x1)] <- x1[length(x1)]/2+x2[1]/2;
    x2[1] <- x1[length(x1)]/2+x2[1]/2;
    
    if (xzoom) {
      sd <- sd(d);
      rg <- range(d);
      if (cutoff>=rg[1] & cutoff<=rg[2]) xaxis$range <- c(max(rg[1], cutoff-1.5*sd), min(rg[2], cutoff+1.5*sd));
    };
    
    if (yl == 'count') tt <- paste('N =', c(length(d[d<cutoff]), length(d[d>cutoff]))) else
      tt <- paste(round(100*c(length(d[d<cutoff]), length(d[d>cutoff]))/length(d), 2), '%', sep='')
    
    tfont <- list(family = "sans serif", size = 16, color = '#666666');
    
    plot_ly(x=dens$x, y=dens$y, type='scatter', mode='line') %>%
      add_trace(x=x1, y=y1, mode='line', fill='tozeroy', fillcolor='#8899FF', line=list(color='#888888')) %>%
      add_trace(x=x2, y=y2, mode='line', fill='tozeroy', fillcolor='#FF9988', line=list(color='#888888'), width=0.5) %>%
      add_trace(x=cutoff, y=0, mode='text', text=paste(tt[1], ' '), textposition='top left', textfont=tfont) %>%
      add_trace(x=cutoff, y=0, mode='text', text=paste(' ', tt[2]), textposition='top right', textfont=tfont) %>%
      add_trace(x=c(cutoff, cutoff), y=c(0, 2*y2[1]), mode='line', line=list(width=3)) %>%
      layout(title = title, xaxis = xaxis, yaxis = yaxis, showlegend=FALSE);     
    
  }
}