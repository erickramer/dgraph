degree <- function(g, type="inbound"){
  s = function(type) switch(type,
                            inbound = g$E %>% select(V=V2, W=W),
                            outbound = g$E %>% select(V=V1, W=W),
                            both = rbind(s("inbound"), s("outbound")))
          
  type = if(g$directed) "inbound" else type
  s(type) %>% group_by(V) %>% summarize(sum(W))
}

page.rank <- function(g, threshold=1e-6){
  
  is.converged <- function(x){
    diff = x %>% 
      mutate(diff=abs(PR.old-PR)/PR.old) %>%
      group_by %>%
      summarize(max(diff))
    diff < threshold 
  }
  
  normalize <- function(z){
    n = (z %>% group_by %>% summarize(sum(PR)))[1,1]
    z %>% mutate(PR=PR/n)
  }
  
  calculate.pr <- function(x){
    edges = g$E %>% select(V1, V=V2, W)
    pr = x %>% select(V, PR)
    
    pr = left_join(pr, edges) %>%
      group_by(V, PR) %>%
      summarize(PR.new=sum(W*PR, na.rm=T)) %>%
      select(V=V, PR.old=PR, PR=PR.new) %>%
      group_by %>%
      normalize
    
    if(is.converged(pr)) pr else pr %>% calculate.pr
  }
  
  g$V %>% mutate(PR=1) %>% calculate.pr
}