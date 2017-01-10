# Find the best size of the epsilon neighborhood by maximizing silhouette scores
OptimizeDBSCAN <- function(d, depth=3, log10eps=c(-6, 6), minPts=5, search='kdtree', bucketSize=10, splitRule='suggest', approx=0) {
  require(dbscan); 
  require(cluster);
  
  depth <- max(1, round(depth)); 
  
  eps <- lapply(log10eps[1]:log10eps[2], function(i) seq(10^i, 10^(i+1), 10^i)); 
  eps <- unique(unlist(eps)); 
  
  minPts <- max(2, round(minPts)); 
  dst <- dist(d); 

  ############################################################################################################################
  getScore <- function(eps) {
    sapply(eps, function(eps) {
      db <- dbscan(d, eps = eps[1], minPts = minPts, search = search, bucketSize = bucketSize, splitRule = splitRule, approx = approx);
      cl <- db$cluster; 
      if (length(unique(cl))==1) 0 else mean(silhouette(cl, dst)[, 3])
    });
  }
  getRange <- function(eps) {
    sc <- getScore(eps); 
    
    i0 <- which(sc==max(sc))[1]; 
    if (i0==1) e <- seq(e[i0], e[i0+1], (e[i0+1]-e[i0])/10) else 
      if (i0==length(sc)) e <- seq(e[i0-1], e[i0], (e[i0]-e[i0-1])/10) else 
        e <- c(seq(e[i0-1], e[i0], (e[i0]-e[i0-1])/10), seq(e[i0], e[i0+1], (e[i0+1]-e[i0])/10));
    e; 
  }
  ############################################################################################################################
  
  e <- eps; 
  for (i in 1:depth) e <- getRange(e); 
  sc <- getScore(e); 
  i0 <- which(sc==max(sc))[1]; 

  dbscan(d, eps = e[i0], minPts = minPts, search = search, bucketSize = bucketSize, splitRule = splitRule, approx = approx);
}