# Draw a 2-set Venn diagram
PlotlyVenn <- function(s1, s2, names=c('Set1', 'Set2'),  universe=c(), fisher=TRUE, title='') {
  require(plotly);

  if (length(universe)>=2) {
    s1<-intersect(s1, universe);
    s2<-intersect(s2, universe);
  };
  if (length(s1)==0&length(s2)==0) {
    plotly_empty()
  } else {
    s0 <- intersect(s1, s2);
    l0 <- length(s0);
    l1 <- length(s1)-l0;
    l2 <- length(s2)-l0;
    l  <- length(universe)-l0-l1-l2;

    x  <- c(1, 2, 3, 2, 1, 3);
    y  <- c(2, 2, 2, 0.75, 3.5, 3.5);
    tt <- c(l1, l0, l2, l, names);

    # Run fisher
    if (fisher) {
      f  <- fisher.test(matrix(c(l, l1, l2, l0), nr=2));
      p  <- f[[1]];
      ci <- f[[2]];
      or <- f[[3]];

      if (p<0.0001) p <- format(p, scientific=TRUE, digit=2) else p <- round(p, 4);
      p  <- gsub(' ', '', p);
      ln <- paste(
        paste('Odds ratio=', round(or, 2), sep=''), '; ',
        paste('95% C.I.=(', round(ci[1], 2), ', ', round(ci[2], 2), ')', sep=''), '; ',
        paste('p=', p), sep='');
      x  <- c(x, 2);
      y  <- c(y, 0.25);
      tt <- c(tt, ln);
    } # end of fisher test

    xaxis <- list(title='', range=c(0, 4), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
    yaxis <- list(title='', range=c(0, 4), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
    tfont <- list(family = "sans serif", size = 16, color = toRGB("grey50"));

    shap1 <- list(type='circle', xref='x', yref='y', x0=0.5, x1=2.5, y0=1, y1=3, fillcolor='blue', opacity=0.3, line=list(color='black', width=1));
    shap2 <- list(type='circle', xref='x', yref='y', x0=1.5, x1=3.5, y0=1, y1=3, fillcolor='red',  opacity=0.3, line=list(color='black', width=1));
    shap3 <- list(type='rect', xref='x', yref='y', x0=0, x1=4, y0=0, y1=4, fillcolor='none',  opacity=1);

    plot_ly(x=x, y=y, type='scatter', mode='text', text=tt, textfont = tfont) %>%
      layout(shapes=list(shap1, shap2, shap3), title=title, showlegend=FALSE, margin=list(b=1,l=1,r=1,t=1), xaxis=xaxis, yaxis=yaxis)
  }
};

##########################################################
PlotVenn<-function(s1, s2, names=c('Set1', 'Set2'),  universe=c(), fisher=TRUE, pdf=FALSE,
                   plot.new=FALSE, lwd=3, cex=1, title='', plotly=FALSE) {
  out<-list();

  if (length(universe)>=2) {
    s1<-intersect(s1, universe);
    s2<-intersect(s2, universe);
  }

  if (pdf) pdf(paste(names, collapse=' - '), w=8, h=6)
  else if (plot.new) quartz(w=8, h=6);

  if (length(s1)==0&length(s2)==0) {
    cat('Both sets are empty.\n');
    NA;
  } else {
    s0 <- intersect(s1, s2);
    l0 <- length(s0);
    l1 <- length(s1)-l0;
    l2 <- length(s2)-l0;
    l  <- length(universe)-l0-l1-l2;

    out$counts<-c(l, l1, l2, l0);

    if (plotly) {
      require(plotly);
      x  <- c(1, 2, 3, 2, 1, 3);
      y  <- c(2, 2, 2, 0.75, 3.5, 3.5);
      tt <- c(l1, l0, l2, l, names);

      # Run fisher
      if (fisher) {
        out$fisher <- fisher.test(matrix(out$counts, nr=2));
        f  <- fisher.test(matrix(c(l1, l, l0, l2), nr=2));
        p  <- out$fisher[[1]];
        ci <- out$fisher[[2]];
        or <- out$fisher[[3]];

        if (p<0.0001) p <- format(p, scientific=TRUE, digit=2) else p <- round(p, 4);
        p  <- gsub(' ', '', p);
        ln <- paste(
          paste('Odds ratio=', round(or, 2), sep=''), '; ',
          paste('95% C.I.=(', round(ci[1], 2), ', ', round(ci[2], 2), ')', sep=''), '; ',
          paste('p=', p), sep='');
        x  <- c(x, 2);
        y  <- c(y, 0.25);
        tt <- c(tt, ln);
      } # end of fisher test

      xaxis <- list(title='', range=c(0, 4), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
      yaxis <- list(title='', range=c(0, 4), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
      tfont <- list(family = "sans serif", size = 16, color = toRGB("grey50"));

      shap1 <- list(type='circle', xref='x', yref='y', x0=0.5, x1=2.5, y0=1, y1=3, fillcolor='blue', opacity=0.3, line=list(color='black', width=1));
      shap2 <- list(type='circle', xref='x', yref='y', x0=1.5, x1=3.5, y0=1, y1=3, fillcolor='red',  opacity=0.3, line=list(color='black', width=1));
      shap3 <- list(type='rect', xref='x', yref='y', x0=0, x1=4, y0=0, y1=4, fillcolor='none',  opacity=1);

      out$plot <- plot_ly(x=x, y=y, type='scatter', mode='text', text=tt, textfont = tfont) %>%
        layout(shapes=list(shap1, shap2, shap3), title=title, showlegend=FALSE, xaxis=xaxis, yaxis=yaxis)
    } else {
      if (title[1]=='') par(mar=rep(1, 4)) else par(mar=c(1, 1, 3, 1));
      plot(0, type='n', xlim=c(0, 8), ylim=c(0, 6), axes=F, bty='n', xaxs='i', yaxs='i', xlab='', ylab=''); # plot an empty space
      bg<-'#7FB3D5';
      fg<-'#F1C40F';
      symbols(3, 3, circles=1.75, bg='#E74C3CBB', fg=fg, lwd=lwd/2, add=TRUE, inches=FALSE);
      symbols(5, 3, circles=1.75, bg='#16A085BB', fg=fg, lwd=lwd/2, add=TRUE, inches=FALSE);
      symbols(3, 3, circles=1.75, fg=fg, lwd=lwd/2, add=TRUE, inches=FALSE);

      rect(0, 0, 8, 6, lwd=lwd, border='black');
      title(main=title, cex.main=2);

      col='#154360';
      text(2, 3, labels=l1, col=col, cex=cex, font=2);
      text(6, 3, labels=l2, col=col, cex=cex, font=2);
      text(4, 3, labels=l0, col=col, cex=cex, font=2);

      text(0.5, 3, labels=names[1], cex=cex, col='darkblue', srt=90);
      text(7.5, 3, labels=names[2], cex=cex, col='darkblue', srt=270);

      if (length(universe)>=2) {
        text(4, 0, labels=l, pos=3, cex=cex, col=col);
        if (fisher) {
          out$fisher<-fisher.test(matrix(out$counts, nr=2));
          p<-out$fisher[[1]];
          ci<-out$fisher[[2]];
          or<-out$fisher[[3]];

          if (p<0.0001) p<-format(p, scientific=TRUE, digit=2) else p<-round(p, 4);
          p<-gsub(' ', '', p);
          line<-paste(
            paste('Odds ratio=', round(or, 2), sep=''), '; ',
            paste('95% C.I.=(', round(ci[1], 2), ', ', round(ci[2], 2), ')', sep=''), '; ',
            paste('p=', p), sep='');
          text(4, 6, labels=line, cex=cex, pos=1);

        } # end of fisher test
      }
    }
  } # end of else

  if (pdf) dev.off();

  out;
}

PlotVenn3Way<-function(s1, s2=NA, s3=NA, names=rep('', 3), fill=c('#E74C3C', '#3498DB', '#2ECC71'), plot.new=TRUE, ...) {

  if(!identical(s2, NA) & !identical(s3, NA)) sets<-list(s1, s2, s3) else sets<-s1;

  n1<-sapply(sets, length);
  n2<-apply(cbind(c(1, 2, 1), c(2, 3, 3)), 1, function(ind) length(intersect(sets[[ind[1]]], sets[[ind[2]]])));
  n3<-length(intersect(sets[[1]], intersect(sets[[2]], sets[[3]])));

  if (plot.new) plot.new();
  VennDiagram::draw.triple.venn(n1[1], n1[2], n1[3], n2[1], n2[2], n2[3], n3, category=names, fill=fill,
                                alpha=0.85, margin=0.15, ...);
}

PlotlyVenn3Way<-function(s1, s2=NA, s3=NA, names=rep('', 3)) {
  require(plotly);

  if(!identical(s2, NA) & !identical(s3, NA)) sets<-list(s1, s2, s3) else sets<-s1;
  sets <- lapply(sets, unique);

  i  <- 1:3;
  n3 <- length(intersect(sets[[1]], intersect(sets[[2]], sets[[3]])));
  n2 <- sapply(rev(i), function(i) length(Reduce('intersect', sets[-i])))-n3;
  n1 <- sapply(i, function(i) length(setdiff(sets[[i]], Reduce('union', sets[-i]))));

  shap1 <- list(type='circle', xref='x', yref='y', x0=1, x1=3, y0=1.5, y1=3.5, fillcolor='red', opacity=0.3, line=list(color='black', width=1));
  shap2 <- list(type='circle', xref='x', yref='y', x0=0.5, x1=2.5, y0=0.5, y1=2.5, fillcolor='blue', opacity=0.3, line=list(color='black', width=1));
  shap3 <- list(type='circle', xref='x', yref='y', x0=1.5, x1=3.5, y0=0.5, y1=2.5, fillcolor='green', opacity=0.3, line=list(color='black', width=1));
  shap4 <- list(type='rect', xref='x', yref='y', x0=0, x1=4, y0=0, y1=4, fillcolor='#',  opacity=1);

  tt <- c(n1, n2, n3, names);
  xs <- c(2, 1, 3, 1.375, 2.625, 2, 2, 2, 1, 3);
  ys <- c(2.75, 1.25, 1.25, 2.125, 2.125, 1.125, 1.875, 3.75, 0.25, 0.25);

  xaxis <- list(title='', range=c(0, 4), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
  yaxis <- list(title='', range=c(0, 4), zeroline=FALSE, showgrid=FALSE, showline=FALSE, autotick=FALSE, showticklabels=FALSE);
  tfont <- list(family = "sans serif", size = 20, color = toRGB("grey50"));

  plot_ly(x=xs, y=ys, type='scatter', mode='text', text=tt, textfont = tfont) %>%
    layout(shapes=list(shap1, shap2, shap3, shap4), title=title, showlegend=FALSE, margin=list(b=1,l=1,r=1,t=1), xaxis=xaxis, yaxis=yaxis)
};

PlotVenn4Way <- function(sets, cols=c('#E74C3C', '#3498DB', '#2ECC71', '#F1C40F')) {
  require('VennDiagram');

  a <- sets[[1]];
  b <- sets[[2]];
  c <- sets[[3]];
  d <- sets[[4]];

  nm <- names(sets);
  if (is.null(nm)) nm <- paste('Set', 1:4, sep='_');

  draw.quad.venn(length(a), length(b), length(c), length(d),
                 length(intersect(a, b)), length(intersect(a, c)), length(intersect(a, d)),
                 length(intersect(b, c)), length(intersect(b, d)), length(intersect(c, d)),
                 length(Reduce('intersect', sets[-4])), length(Reduce('intersect', sets[-3])),
                 length(Reduce('intersect', sets[-2])), length(Reduce('intersect', sets[-1])),
                 length(Reduce('intersect', sets)),
                 fill = cols, cat.col = cols, category = names(sets)) -> venn;

  venn;
};
