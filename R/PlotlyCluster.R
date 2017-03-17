PlotlyCluster <- function(d, group=NA) {
  require(plotly); 
  
  if (is.na(group)) group <- colnames(d); 
  gp <- split(1:ncol(d), group)[unique(group)];
  d  <- d[, unlist(gp, use.names=FALSE), drop=FALSE]; 
  
  mn <- colMeans(d); 
  if (nrow(d)>1) se <- apply(d, 2, function(d) sd(d)/sqrt(length(d))) else se <- rep(0, ncol(d));
  ci <- qt(0.975, nrow(d)-1)*se;
  hi <- mn+ci;
  lo <- mn-ci;
  
  x0 <- lapply(gp, function(g) if (length(g)==1) 0 else if (length(g)==2) c(-0.1, 0.1) else
    seq(-0.15, 0.15, length.out = length(g)));
  xs <- rep(1:length(gp), sapply(gp, length)) + unlist(x0, use.names=FALSE);
  
  
}