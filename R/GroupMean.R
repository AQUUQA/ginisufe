##将数据处理成分组数数据
##输入数据x为个体收入数据
##分组个数n
##输出为分组后，每组的均值
##总数除以分组个数，确定每组中的样本个数，并找到对应节点
GroupMean <- function(x,n){
  #样本总数
  N <- length(x)
  #计算每组样本个数（向下取整）
  m <- floor(N/n)
  #初始化每组样本个数向量
  num <- rep(m,n)
  #将多余样本进行分摊
  rest <- N-m*n
  for(i in 1:rest){
    num[i] <- num[i]+1
  }
  #print(N-sum(num))
  #print(head(cumsum(num)+1,-1))
  #构建各组起始点位置向量
  f_num <- c(1, cumsum(num)+1)
  #初始化均值向量
  avg <- rep(0,n)
  for(i in 1:n){
    avg[i] <- mean(x[f_num[i]:f_num[i+1]-1])
  }
  return(avg)
}


