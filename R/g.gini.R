#x分组收入数据，每个元素表示对应组别的平均收入
#w每组的对应权重，为数值类型
g.gini <- function(x,w=NULL){

  #设定内部函数。计算加权平均
  WM <- function(x, w) {

    # 计算加权平均值
    weighted_sum <- sum(x * w)
    total_weight <- sum(w)
    mean_value <- weighted_sum / total_weight
    return(mean_value)

  }

  ##参数设置
  #权重w
  if(is.null(w)){
    w <- rep(1,length(x))}

  # 如果x中有缺失值或小于0的值，则返回NA
  if (any(is.na(x)) || any(x < 0))
    return(NA_real_)

  m <- length(x)#分组数
  w <- w/sum(w)
  x <- x[id <- order(x)]
  w <- w[id]
  f.hat <- w/2 + c(0, head(cumsum(w), -1)) #权重的累计密度函数
  wm <- WM(x, w)
  res <- 2/wm * sum(w * (x - wm) * (f.hat - WM(f.hat,w)))
  res_hat <- res+0.2/m

  return(res_hat)

}




