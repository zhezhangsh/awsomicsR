PlotlySynergy2 <- function(v, name=c('', ''), shape=c('circle', 'rect'), size=5, title='', legend='',  panel.size=600,
                           color.min=min(v), color.max=max(v), color.type=GetColorPanelTypes()[1]) {
  # v           numeric vector of 3 numbers: A, B, AB effect of 2 treatment
  # name        names of the 2 treatment
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
  
  v <- v[1:3]; 
  
  if (name[1]=='') name[1] <- 'A';
  if (name[2]=='') name[2] <- 'B';
  name[3] <- 'Both'; 
  
  size <- min(5, max(0.5, 0.5*size)); 
  
  if (color.type[1] %in% GetColorPanelTypes()) color.type <- color.type[1] else color.type <- 'blue-pink'; 
  color <- GetColors(128, color.type);
  if (color.type == 'heat') color <- rev(color);
  
  # Circle position
  rgb <- apply(col2rgb(color), 2, function(x) paste('rgb(', paste(x, collapse=', '), ')', sep=''));
  rng <- sort(c(color.min, color.max));
  rng <- FormatNumeric(matrix(rng, nc=1))[, 1];
  col <- round(length(rgb)*(v-rng[1])/(rng[2]-rng[1]))
  col <- rgb[pmin(pmax(col, 1), length(rgb))];
  x <- c(-sqrt(0.75), sqrt(0.75), 0);
  y <- c(-0.5, -0.5, 0.62); 
  s <- lapply(1:length(x), function(i) 
    list(type=shape[1], xref='x', yref='y', 
         x0=x[i]-0.1*size, x1=x[i]+0.1*size, y0=y[i]-0.1*size, y1=y[i]+0.1*size, 
         fillcolor=col[i], opacity=1, line=list(color='black', width=size)))
  
  # Label position
  xt <- x[1:3];
  yt <- c(y[1]-0.1*size-0.075, y[2]-0.1*size-0.075, y[3]+0.1*size+0.05);
  st <- max(8, 18-round(sqrt(max(nchar(name[2]), nchar(name[3])))));
  
  # Axis 
  sz <- 0.1*size+0.1;
  xaxis <- list(title='', range=c(-1-sz, 1+sz), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
  yaxis <- list(title='', range=c(-1-sz, 1+sz), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
  
  # Legend
  xl <- seq(x[1]-0.1*size, x[2]+0.1*size, length.out = 1+length(color))
  s[4:(3+length(color))] <- lapply(1:(length(xl)-1), function(i) 
    list(type='rect', xref='x', yref='y', x0=xl[i], x1=xl[i+1], y0=-1.1, y1=-1, 
         fillcolor=rgb[i], opacity=1, line=list(color='#F9F9F911', width=0.01)));
  
  # Edge lines
  w <- max(0.5, min(1.5, size));
  
  plot_ly(x=0, y=0, type='scatter', mode='text', text='', hoverinfo='none') %>%
    add_lines(x=x[c(1,3,2)], y=y[c(1,3,2)], line = list(color = "#33333333", width=w)) %>%
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