## Make plot using a data matrix, whose columns are split into sample groups to be compared.

PlotMatrixGroupTypes<-function() c('Heatmap', 'Bar Plot', 'Box Plot', 'Density Plot', 'Cumulative Plot');

PlotMatrixGroup<-function(d, grp, type, normalize=TRUE, color=GetColorTypes()[1], plotly=FALSE) {
  # d           A data matrix
  # grp         Named list corresponding to sample groups and column names in <d>
  # normalize   Whether to normalize data across samples
  # color       Color code
  
  type <- tolower(type)[1];
  tp <- tolower(PlotMatrixGroupTypes());
  
  grp <- lapply(grp, function(g) g[g %in% colnames(d)]);
  grp <- grp[sapply(grp, length)>0];
  d <- d[, unlist(grp, use.names=FALSE), drop=FALSE];
  d <- d[!is.na(rowMeans(d)), , drop=FALSE];
  if (normalize) d <- t(scale(t(d)));
  
  out<-list();
  
  if (ncol(d)==0) {
    msg <- 'Data requires at least 1 column.'; 
    out$message<-msg;
    if (!plotly) out$plotted <- plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg) else 
      plotted <- list(plotly_empty() %>% layout(title=msg, margin=list(t=100))); 
  } else if (nrow(d)==0) {
    msg <- 'Data requires at least 1 row.'; 
    out$message<-msg;
    if (!plotly) out$plotted <- plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg) else 
      plotted <- list(plotly_empty() %>% layout(title=msg, margin=list(t=100))); 
  } else {
    if (type == tp[1]) plotted <- PlotMatrixGroupHeatmap(d, grp, color, normalize, plotly) else 
      if (type == tp[2]) plotted <- PlotMatrixGroupBar(d, grp, color, normalize, plotly) else 
        if (type == tp[3]) plotted <- PlotMatrixGroupBox(d, grp, color, normalize, plotly) else 
          if (type == tp[4]) plotted <- PlotMatrixGroupDensity(d, grp, color, normalize, plotly) else 
            if (type == tp[5]) plotted <- PlotMatrixGroupCumulative(d, grp, color, normalize, plotly) else {
              msg <- paste('Unknown plot type: ', type, '.', sep=''); 
              out$message<-msg;
              if (!plotly) out$plotted <- plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg) else
                plotted <- list(plotly_empty() %>% layout(title=msg, margin=list(t=100))); 
            }
  };
  
  if (!plotly) out else plotted[[1]]; 
}

#############################################################################################
# Barplot
PlotMatrixGroupBar<-function(d, grp, color, normalize) {
  if (normalize) ylab<-'Normalized average expression level' else ylab<-'Average expression level';
  
  c<-GetColors(length(grp), color);
  
  m<-colMeans(d);
  s<-lapply(grp, function(g) m[g]);
  
  ########################################
  bar<-PlotBarFromList(s, ylab=ylab , c);#
  ########################################
  
  list(barplot=bar, color=c);  
}

#############################################################################################
# Heatmap plotly
PlotlyMatrixGroupHeatmap<-function(d, grp, color, key="key", clustering=TRUE) {
  if (nrow(d)<2 | ncol(d)<2) {
    plotly_empty() %>% layout(title="Not enough rows/columns to plot a heatmap.", margin=list(t=100)); 
  } else {
    grp <- lapply(grp, function(grp) grp[grp %in% colnames(d)]); 
    col <- GetColors(length(grp), color);
    col <- rep(col, sapply(grp, length));
    names(col) <- unlist(grp, use.names=FALSE); 
    d <- d[, names(col), drop=FALSE]; 

    if (clustering) {
      d <- d[, hclust(as.dist(1-cor(d)))$order, drop=FALSE];
      d <- d[hclust(as.dist(1-cor(t(d))))$order, , drop=FALSE];
      col <- col[colnames(d)]; 
    }
    
    xa <- list(title='', zeroline=FALSE, showgrid=FALSE, showline=FALSE, tickangle = -30, 
               tickmode='array', tickvals=0:(ncol(d)-1), ticktext=colnames(d));
    ya <- list(title='', zeroline=FALSE, showgrid=FALSE, showline=FALSE, tickangle = +30,
               tickmode='array', tickvals=0:(nrow(d)-1), ticktext=rownames(d));
    
    ml <- 7.5*max(nchar(rownames(d)));
    mb <- 7.5*max(nchar(colnames(d)));
    cl <- substr(col, 1, 7);  
    nr <- nrow(d); 
    cb <- list(title=key, titleside='right'); 
    sp <- lapply(1:length(col), function(i) 
      list(type='rect', fillcolor=cl[i], line=list(color='black', width=.4), opacity = 0.9,
           x0 = i-1.5, x1 = i-.5, xref = "x", y0 = -nr/25-.5, y1 = -nr/100-.5, yref = "y"));
    
    p <- plot_ly(z=d, type = "heatmap",  colors = colorRamp(c("yellow", "red")), colorbar=cb) %>%
      layout(xaxis=xa, yaxis=ya, margin=list(l=ml, b=mb, t=60), shapes=sp); 
    
    p;
}; 

#############################################################################################
# Heatmap
PlotMatrixGroupHeatmap<-function(d, grp, color, normalize, plotly=FALSE, clustering=TRUE) {
  if (nrow(d) < 2) {
    msg <- out <- "Not enough genes to plot heatmap.";
    if (!plotly) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg) else
      plotly_empty() %>% layout(title=msg, margin=list(t=100)); 
  } else {
    grp <- lapply(grp, function(grp) grp[grp %in% colnames(d)]); 
    col <- GetColors(length(grp), color);
    col <- rep(col, sapply(grp, length));
    names(col) <- unlist(grp, use.names=FALSE); 
    d <- d[, names(col), drop=FALSE]; 
    if (normalize) xlab<-'Normalized data' else xlab<-'Un-normalized data';
    
    if (!plotly) {
      # block color
      c <- gplots::colorpanel(129, 'yellow', 'red');
      
      # set label size
      w0 <- 1.2/max(strwidth(colnames(d), unit='in'));
      cexCol <- min(1.5, w0);
      w <- 1.2/max(strwidth(rownames(d), unit='in'));
      h <- (par()$fin[2]-2.5)/nrow(d)/max(strheight(rownames(d), unit='in'));
      cexRow <- min(1.5, min(w, h));
      
      mar.sz<-max(0, nrow(d)/-8+1.25);
      par(omi=c(mar.sz, 0, mar.sz, 0));
      out <- gplots::heatmap.2(
        d, scale='none', cexCol=cexCol, cexRow=cexRow, col=c, ColSideColors=col, trace='none', srtCol=30, srtRow=30, 
        key=TRUE, keysize=1, key.title='', key.xlab=xlab, key.ylab='', density.info='none', margins=c(6, 10));
      list(heatmap=out, col=col);
    } else {
      if (clustering) {
        d <- d[, hclust(as.dist(1-cor(d)))$order, drop=FALSE];
        d <- d[hclust(as.dist(1-cor(t(d))))$order, , drop=FALSE];
        col <- col[colnames(d)]; 
      }
      
      xa <- list(title='', zeroline=FALSE, showgrid=FALSE, showline=FALSE, tickangle = -30, 
                 tickmode='array', tickvals=0:(ncol(d)-1), ticktext=colnames(d));
      ya <- list(title='', zeroline=FALSE, showgrid=FALSE, showline=FALSE, tickangle = +30,
                 tickmode='array', tickvals=0:(nrow(d)-1), ticktext=rownames(d));
      
      ml <- 7.5*max(nchar(rownames(d)));
      mb <- 7.5*max(nchar(colnames(d)));
      cl <- substr(col, 1, 7);  
      nr <- nrow(d); 
      cb <- list(title=c(xlab), titleside=c('right')); 
      sp <- lapply(1:length(col), function(i) 
        list(type='rect', fillcolor=cl[i], line=list(color='black', width=.4), opacity = 0.9,
             x0 = i-1.5, x1 = i-.5, xref = "x", y0 = -nr/25-.5, y1 = -nr/100-.5, yref = "y"));
      
      out <- plot_ly(z=d, type = "heatmap",  colors = colorRamp(c("yellow", "red")), colorbar=cb) %>%
        layout(xaxis=xa, yaxis=ya, margin=list(l=ml, b=mb, t=60), shapes=sp); 
      
      list(heatmap=out, col=cl); 
    }
  }
}

#############################################################################################
# Boxplot
PlotMatrixGroupBox<-function(d, grp, color, normalize) {
  if (normalize) ylab<-'Normalized expression level' else ylab<-'Expression level';
  
  c<-GetColors(length(grp), color);
  
  m<-colMeans(d);
  s<-lapply(grp, function(g) m[g]);
  
  ########################################
  bar<-PlotBoxFromList(s, ylab=ylab , c);#
  ########################################
  
  list(boxplot=bar, color=c);  
}

#############################################################################################
# Density plot
PlotMatrixGroupDensity<-function(d, grp, color, normalize) {
  if (nrow(d) < 2) {
    dens<-"Not enough data points to make density plot";
    plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Not enough data to make density plot'));
  } else {
    s<-lapply(grp, function(g) rowMeans(d[, g, drop=FALSE]));
    if (normalize) xlab<-'Normalized expression level' else xlab<-'Expression level';
    c<-GetColors(length(grp), color);
    c0<-paste(substr(c, 1, 7), '88', sep='');
    ######################################################################
    dens<-PlotDensityFromList(s, xlab=xlab, c0, fill=TRUE, legend=TRUE); #
    ######################################################################
  }

  list(densityplot=dens, color=c);  
}


#############################################################################################
# Density plot
PlotMatrixGroupCumulative<-function(d, grp, color, normalize) {
  if (nrow(d) < 2) {
    dens<-"Not enough data points to make cumulative plot";
    plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Not enough data to make density plot'));
  } else {
    s<-lapply(grp, function(g) rowMeans(d[, g, drop=FALSE]));
    if (normalize) xlab<-'Normalized expression level' else xlab<-'Expression level';
    c<-GetColors(length(grp), color);
    #########################################################
    cm<-PlotCumulativeFromList(s, xlab=xlab, c, legend=TRUE); #
    #########################################################
  }
  
  list(cumulative=cm, color=c);  
}
