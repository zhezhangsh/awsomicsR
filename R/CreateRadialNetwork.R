# Format a nested list for the radialNetwork {networkD3} function
CreateRadialNetwork <- function(lst, root='') {
  format.element <- function(l) {
    if (class(l) != 'list') { print(1);
      lapply(l, function(x) list(name=x));
    } else { print(3);
      lapply(names(l), function(nm) list(name=nm, children=format.element(l[[nm]])));
    }
  }
  # format.element <- function(l) {
  #   if (length(l)==1) {
  #     if (class(l[[1]]) != 'list') { print(1); 
  #       list(name=names(l), children=lapply(as.vector(l[[1]]), function(x) list(name=x)));
  #     } else { print(2); 
  #       list(name=names(l), children=format.element(l[[1]])); 
  #     } 
  #   } else { print(3);
  #     lapply(1:length(l), function(i) format.element(l[i]));
  #   }
  # }
  list(name=root, children=format.element(lst));
}