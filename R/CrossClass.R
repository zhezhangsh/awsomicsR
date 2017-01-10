# Summarize agreement between 2 classifications
CrossClass <- function(cl0, cl1) {
  require(flexclust); 
    
  tab0 <- t(xtabs(~cl0+cl1)); 
  stat <- classAgreement(tab0); 
  chsq <- chisq.test(cl0, cl1, simulate.p.value=TRUE, B=10^4); 
  
  exp <- sapply(1:ncol(tab0), function(i) chsq$expected[i, ]);
  obs <- sapply(1:ncol(tab0), function(i) chsq$observed[i, ]);
  colnames(exp) <- colnames(obs) <- colnames(tab0); 
  
  list(stats=stat, chisq=chsq, expected=exp, observed=obs); 
}