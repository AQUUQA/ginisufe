#x收入数据，默认为个体独立数据
#u优化方式，极高收入人群信息缺失u=1，极低收入人群信息缺失u=2，极低和极高收入人群信息缺失u=3
#ph缺失的极高收入人群占比
#pl缺失的极低收入人群占比



opt.gini <- function(x,u,ph=NULL,pl=NULL,sh=NULL,sl=NULL){
  ##设定内部函数，计算加权平均
  WM <- function(x, w) {
    # 计算加权平均值
    weighted_sum <- sum(x * w)
    total_weight <- sum(w)
    mean_value <- weighted_sum / total_weight
    return(mean_value)
  }

  sh_sim <- function(x,p=NULL){

    n <- length(x)
    n_0 <- n*p/(1-p)
    wx <- x/sum(x) #生成个体财富占比
    x <- x[id <- order(x)] #对x进行排序,升序
    wx <- wx[id] #对个体财富占比进行排序，其中的每个值即为斜率k
    if(is.null(p)){

      k_1 <- (wx[n]-wx[n-1])
      kk_1 <- (wx[n]-2*wx[n-1]+wx[n-3])
      kk_2 <- (wx[n-1]-2*wx[n-2]+wx[n-3])
      kk <- 0.5*(kk_1+kk_2)
      s <-  (wx[n]+(k_1+kk*0.5*p)*0.5*n_0)*n_0
      return(s)

    }
  }


  sl_sim <- function(x,p=NULL){

    n <- length(x)
    n_0 <- n*p/(1-p)
    wx <- x/sum(x) #生成个体财富占比
    x <- x[id <- order(x,decreasing = TRUE)] #对x进行排序,升序
    wx <- wx[id] #对个体财富占比进行排序，其中的每个值即为斜率k
    if(is.null(p)){

      k_1 <- (wx[n]-wx[n-1])
      kk_1 <- (wx[n]-2*wx[n-1]+wx[n-3])
      kk_2 <- (wx[n-1]-2*wx[n-2]+wx[n-3])
      kk <- 0.5*(kk_1+kk_2)
      s <-  (wx[n]+(k_1+kk*0.5*p)*0.5*n_0)*n_0
      return(s)

    }
  }


  ##参数设置
  #权重w
  w <- rep(1,length(x))

  # 如果x中有缺失值或小于0的值，则返回NA
  if (any(is.na(x)) || any(x < 0) || is.null(u))
    return(NA_real_)

  w <- w/sum(w)
  x <- x[id <- order(x)]
  w <- w[id]
  f.hat <- w/2 + c(0, head(cumsum(w), -1)) #权重的累计密度函数
  wm <- WM(x, w)
  res <- 2/wm * sum(w * (x - wm) * (f.hat - WM(f.hat,w)))


  ###默认该情况下所使用的数据为个体独立数据而非分组数据
  ##已知缺失人口占比p，求出对应的财富占比s
  ##生成洛伦茨曲线的横纵坐标：累计财富占比，累计人口占比
  ##计算将已知数据中最富裕1%等分成100份，并计算100份的斜率k，在计算斜率的斜率kk
  ##斜率k表示，每1%的人口占比所拥有的财富占比
  ##斜率kk表示，每1%的人口占比所拥有的财富占比的增长速率
  ##定义缺失人口所拥有的平均财富占比为km
  ##km可近似表示为，km=k_max+kk*0.5*p
  ##其中k_max为100份k中的最大值
  ##因此缺失人口的对应财富s= p*km

  ##进行优化
  if (u==1){
    #极高收入群体数据缺失
    if (!is.null(sh)){
      s_h <- sh
      res_hat <- res+(1-res)*s_h*0.5
      return(res_hat)
    }else if(!is.null(ph)){
      s_h <- s_sim(x,ph)
      res_hat <- res+(1-res)*s_h*0.5
      return(res_hat)
    }else {
      return (NA_real_)
    }

  }
  else if (u==2){
   #极低收入群体数据缺失
    if (!is.null(pl)){
      p_l <- pl
      res_hat <- res+(1-res)*p*0.5
      return(res_hat)
    }else{
      return(NA_real_)
    }
  }
  else{
    #极高收入群体与极低收入群体同时缺失
    #得已知pl，ph；
    #
    if(is.null(pl) | is.null(ph)){
      return (NA_real_)
    }else{

      p_h <- ph
      p_l <- pl

      if(sl==NUll){
        s_l <- sl_sim(x,pl) }
      else{s_l <- sl}

      if(sh==NUll){
        s_h <- sh_sim(x,ph) }
      else{s_h <- sh}

      p_m <- 1-p_l-p_h
      s_m <- 1-s_h-s_l
      res_hat <- p_m*s_m*res+s_m*p_l+s_h*p_l+s_h*p_m-0.5*(p_l+s_l)

      return(res_hat)

    }

   }

}


