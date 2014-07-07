dgraph <- function(E, V, directed){
  g = list()
  
  g$E$V1 = factor(g$E$V1)
  g$E$V2 = factor(g$E$V2)
  
  g$E = if(directed) E else
    E %>% 
    select(V1=V2, V2=V1, W=W) %>%
    rbind(E) %>%
    filter(as.numeric(V1) <= as.numeric(V2))
  
  g$V = data.frame(V=factor(V))

  g$directed = directed
  
  if(!all(levels(V) %in% c(levels(E$V1), levels(E$V2)))) 
    warning("Extra vertices found in edge list")
  
  class(g) = "dgraph"
  g
}

dgraph.edgelist <- function(E, directed=T){
  require("dplyr")
  
  colnames(E) = c("V1", "V2", "W")
  
  V = E %>% 
    select(V1, V2) %>%
    unlist %>%
    unique
  
  dgraph(E, V, directed)
}

dgraph.adjacency <- function(adj, directed=T){
  require("dplyr")
  require("reshape2")
  
  if(!directed) if(isSymmetric(adj)) adj[lower.tri(adj)] = 0 else
    warning("Expected a symmetric matrix for undirected graph. Ignoring lower triangle of adjacency matrix")
  
  row.names(adj) = colnames(adj)
  
  E = adj %>% 
    as.matrix %>%
    melt %>%
    filter(value != 0) %>% 
    select(V1=Var1, V2=Var2, W=value)
  
  V = if(!is.null(colnames(adj))) colnames(adj) else
        1:ncol(a)
  
  dgraph(E, V, directed)
}

add.vertices <- function(g, V){
  dgraph(g$E, unique(c(g$V$V, V)), g$directed)
}

add.edges <- function(g, E){
  dgraph(rbind(g$E, E), unique(g$V$V, E[,1], E[,2]), g$directed)
}