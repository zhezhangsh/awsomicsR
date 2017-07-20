PlotSynergy3 <- function(v, name=c('', '', ''), shape=c('circle', 'rect'), size=5, title='', legend='',
                         color.min=min(v), color.max=max(v), color.type=GetColorPanelTypes()[1]) {
  require(awsomics);
  
  if (name[1]=='') name[1] <- 'A';
  if (name[2]=='') name[2] <- 'B';
  if (name[3]=='') name[3] <- 'C';
  
  size <- max(1, min(6, size)); 
  
  if (color.type[1] %in% GetColorPanelTypes()) color.type <- color.type[1] else color.type <- 'blue-pink'; 
  color <- GetColors(128, color.type);
  if (color.type == 'heat') color <- rev(color);
  
  x <- c(0, -sqrt(0.75), sqrt(0.75), -sqrt(0.27), sqrt(0.27), 0, 0);
  y <- c(1, -0.5, -0.5, 0.3, 0.3, -0.6, 0) - 0.1; 
  w <- max(0.5, min(1.5, size));
  
  rng <- sort(c(color.min, color.max));
  rng <- FormatNumeric(matrix(rng, nc=1))[, 1];
  col <- pmin(length(color), round(length(color)*(v-rng[1])/(rng[2]-rng[1]))+1); 
  col <- color[col];
  
  plot(0, 0, type = "n", xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), axes = FALSE, bty = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = ""); 
  lines(x=x[c(1,4,2)], y=y[c(1,4,2)], col="#33333333", lwd=w);
  lines(x=x[c(2,6,3)], y=y[c(2,6,3)], col="#33333333", lwd=w);
  lines(x=x[c(3,5,1)], y=y[c(3,5,1)], col="#33333333", lwd=w);
  lines(x=x[c(1, 7)],  y=y[c(1, 7)],  col="#33333333", lwd=w);
  lines(x=x[c(2, 7)],  y=y[c(2, 7)],  col="#33333333", lwd=w);
  lines(x=x[c(3, 7)],  y=y[c(3, 7)],  col="#33333333", lwd=w);
  for (i in 1:length(x)) symbols(x=x[i], y=y[i], circles = size/33, inches = FALSE, add = TRUE, bg = col[i]);
  
  xt <- x[1:3];
  yt <- c(y[1]+size/33+0.06, y[2]-size/33-0.075, y[3]-size/33-0.075);
  text(xt, yt, labels = name, cex=1.5);
  
  xl <- seq(x[2]-size/33, x[3]+size/33, length.out = 1+length(color))
  lapply(1:(length(xl)-1), function(i) 
    rect(xleft = xl[i], xright = xl[i+1], ybottom = -1.1, ytop = -1, lwd=0, col = color[i])) -> z;
  text(x=min(xl), y=-0.95, labels=rng[1]);
  text(x=max(xl), y=-0.95, labels=rng[2]);
  text(x=0, y=-0.95, labels=legend);
    
  title(main = title, cex.main = 1.5);
};

PlotlySynergy3 <- function(v, name=c('', '', ''), shape=c('circle', 'rect'), size=5, title='', legend='', panel.size=600,
                           color.min=min(v), color.max=max(v), color.type=GetColorPanelTypes()[1]) {
  
  # v           numeric vector of 7 numbers: A, B, C, AB, AC, BC, ABC effect of 3 treatment
  # name        names of the 3 treatment
  # shape       plot shape of the blocks
  # size        plot size of the blocks
  # title       figure title
  # legend      name of legend
  # panel       figure size, in pixels
  # color.min   minimal value of the first color
  # color.max   maximal value of the last color
  # color.type  type of color panel; call awsomics::GetColorPanelTypes() to get options
  
  require(plotly);
  require(awsomics);
  
  v <- v[1:7]; 
  
  if (name[1]=='') name[1] <- 'A';
  if (name[2]=='') name[2] <- 'B';
  if (name[3]=='') name[3] <- 'C';
  
  size <- min(3, max(0.3, 0.3*size)); 
  
  if (color.type[1] %in% GetColorPanelTypes()) color.type <- color.type[1] else color.type <- 'blue-pink'; 
  color <- GetColors(128, color.type);
  if (color.type == 'heat') color <- rev(color);
  
  # Circle position
  rgb <- apply(col2rgb(color), 2, function(x) paste('rgb(', paste(x, collapse=', '), ')', sep=''));
  rng <- sort(c(color.min, color.max));
  rng <- FormatNumeric(matrix(rng, nc=1))[, 1];
  col <- round(length(rgb)*(v-rng[1])/(rng[2]-rng[1]))
  col <- rgb[pmin(pmax(col, 1), length(rgb))];
  x <- c(0, -sqrt(0.75), sqrt(0.75), -sqrt(0.27), sqrt(0.27), 0, 0);
  y <- c(1, -0.5, -0.5, 0.3, 0.3, -0.6, 0); 
  s <- lapply(1:length(x), function(i) 
    list(type=shape[1], xref='x', yref='y', 
         x0=x[i]-0.1*size, x1=x[i]+0.1*size, y0=y[i]-0.1*size, y1=y[i]+0.1*size, 
         fillcolor=col[i], opacity=1, line=list(color='black', width=size)))
  
  # Label position
  xt <- x[1:3];
  yt <- c(y[1]+0.1*size+0.05, y[2]-0.1*size-0.075, y[3]-0.1*size-0.075);
  st <- max(8, 18-round(sqrt(max(nchar(name[2]), nchar(name[3])))));
  
  # Axis 
  sz <- 0.1*size+0.1;
  xaxis <- list(title='', range=c(-1-sz, 1+sz), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
  yaxis <- list(title='', range=c(-1-sz, 1+sz), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
  
  # Legend
  xl <- seq(x[2]-0.1*size, x[3]+0.1*size, length.out = 1+length(color))
  s[8:(7+length(color))] <- lapply(1:(length(xl)-1), function(i) 
    list(type='rect', xref='x', yref='y', x0=xl[i], x1=xl[i+1], y0=-1.1, y1=-1, 
             fillcolor=rgb[i], opacity=1, line=list(color='#F9F9F911', width=0.01)));
 
  # Edge lines
  w <- max(0.5, min(1.5, size));
    
  plot_ly(x=0, y=0, type='scatter', mode='text', text='', hoverinfo='none') %>%
    add_lines(x=x[c(1,4,2)], y=y[c(1,4,2)], line = list(color = "#33333333", width=w)) %>%
    add_lines(x=x[c(2,6,3)], y=y[c(2,6,3)], line = list(color = "#33333333", width=w)) %>%
    add_lines(x=x[c(3,5,1)], y=y[c(3,5,1)], line = list(color = "#33333333", width=w)) %>%
    add_lines(x=x[c(1,7)], y=y[c(1,7)], line = list(color = "#33333333", width=2*w)) %>%
    add_lines(x=x[c(2,7)], y=y[c(2,7)], line = list(color = "#33333333", width=2*w)) %>%
    add_lines(x=x[c(3,7)], y=y[c(3,7)], line = list(color = "#33333333", width=2*w)) %>%
    add_lines(x=range(xl), y=c(-1.1, -1.1), line = list(color = "#000000", width=2)) %>%
    add_lines(x=range(xl), y=c(-1, -1), line = list(color = "#000000", width=2)) %>%
    add_lines(x=xl[1], y=c(-1.1, -1), line = list(color = "#000000", width=2)) %>%
    add_lines(x=xl[length(xl)], y=c(-1.1, -1), line = list(color = "#000000", width=2)) %>%
    add_text(x=xt, y=yt, type='scatter', mode='text', text=name, textfont = list(color = '#000000', size = st)) %>%
    add_text(x=0, y=-0.95, type='scatter', mode='text', text=legend, textfont = list(color='rgb(0,0,0)', size=12)) %>%
    add_text(x=min(xl),  y=-0.95, type='scatter', mode='text', text=rng[1], textposition='right', textfont = list(color='rgb(0,0,0)', size=12)) %>%
    add_text(x=max(xl),  y=-0.95, type='scatter', mode='text', text=rng[2], textposition='left',  textfont = list(color='rgb(0,0,0)', size=12)) %>%
    layout(shapes=s, title=title, showlegend=FALSE, xaxis=xaxis, yaxis=yaxis, 
           autosize=FALSE, width=panel.size, height=panel.size);

}