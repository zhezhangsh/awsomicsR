PlotlyDensity <- function(d, cutoff=NA, title='', xlab='', ylab=c('Density', 'Percent', 'Count')) {
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
  } else {
    x1 <- dens$x[dens$x<=cutoff]; 
    y1 <- dens$y[dens$x<=cutoff];
    x2 <- dens$x[dens$x>=cutoff];
    y2 <- dens$y[dens$x>=cutoff]; 
    
    if (yl == 'count') tt <- paste('N =', c(length(d[d<cutoff]), length(d[d>cutoff]))) else
      tt <- paste(round(100*c(length(d[d<cutoff]), length(d[d>cutoff]))/length(d), 2), '%', sep='')
    
    tfont <- list(family = "sans serif", size = 24, color = '#B8B8B8');
    
    plot_ly(x=dens$x, y=dens$y, type='scatter', mode='line') %>%
      add_trace(x=x1, y=y1, mode='line', fill='tozeroy', fillcolor='#9966FF', line=list(color='#888888')) %>%
      add_trace(x=x2, y=y2, mode='line', fill='tozeroy', fillcolor='#FF6699', line=list(color='#888888'), width=0.5) %>%
      add_trace(x=cutoff, y=0, mode='text', text=paste(tt[1], '  '), textposition='top left', textfont=tfont) %>%
      add_trace(x=cutoff, y=0, mode='text', text=paste('  ', tt[2]), textposition='top right', textfont=tfont) %>%
      add_trace(x=c(cutoff, cutoff), y=yaxis$range, mode='line') %>%
      layout(title = title, xaxis = xaxis, yaxis = yaxis, showlegend=FALSE);     
    
  }
}