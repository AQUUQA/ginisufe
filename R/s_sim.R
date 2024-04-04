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


#检测：用不同数据，

data <- read.csv("C:/Users/10120/Desktop/科研/基尼系数/代码实现/数据/fin/fin_12_sorted.csv")

s_sim <- function(x,p=NULL){

  n <- length(x)
  n_0 <- n*p/(1-p)
  w <- rep(1,length(x))
  w <- w/sum(w)
  wx <- x/sum(x) #生成个体财富占比
  x <- x[id <- order(x)] #对x进行排序,升序
  w <- w[id] #对w进行排序
  wx <- wx[id] #对个体财富占比进行排序，其中的每个值即为斜率k
  if(is.null(p)){
    k_1 <- (wx[n]-wx[n-1])
    kk_1 <- (wx[n]-2*wx[n-1]+wx[n-3])
    kk_2 <- (wx[n-1]-2*wx[n-2]+wx[n-3])
    kk <- 0.5*(kk_1+kk_2)
    s <-  (wx[n]+(k_1+kk*0.5*p)*0.5*n_0)*n_0
    print(paste("预测结果为", s))
    return(s)
  }
}


#输入缺失条数n或占比p
#输出预测占比与真是占比
Test <- function(x,n=NULL,p=NULL){
  #权重化
  N <- length(x)
  w <- rep(1,length(x))
  w <- w/sum(w)
  wx <- x/sum(x) #生成个体财富占比
  x <- x[id <- order(x)] #对x进行排序,升序
  w <- w[id] #对w进行排序
  wx <- wx[id] #对个体财富占比进行排序，其中的每个值即为斜率k
  #计算真实缺失财富占比,生成测试用的数据
  if(!is.null(n)){
    if(n==1){
      s_real <- wx[N]
      print(paste("真实结果为", s_real))
    }else{
      n_low <- N-n+1
      wxx <- wx[n_low:N]
      s_real <- sum(wxx)
      print(paste("真实结果为", s_real))
    }

    Tx <- head(x,-n)
    p <- n/N
    s_sim(Tx,p)

  }else if(!is.null(p)){

    if(round(N*(1-p))==N){
      s_real <- wx[N]
      print(paste("真实结果为", s_real))
    }else{
      s_real <- sum(wx[round(N*(1-p)):N])
      print(paste("真实结果为", s_real))
    }

    Tx <- head(x,-round(N*(1-p))+1)
    s_sim(Tx,p)

  }
}
