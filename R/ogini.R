##原始基尼系数计算
##仅知道个体数据
ogini <- function(x){

  m <- length(x)
  w <- rep(1,m)
  w <- w/sum(w)
  x <- x[id <- order(x)]
  w <- w[id]
  f.hat <- w/2 + c(0, head(cumsum(w), -1)) #权重的累计密度函数
  wm <- WM(x, w)
  res <- 2/wm * sum(w * (x - wm) * (f.hat - WM(f.hat,w)))

  return(res)
}
