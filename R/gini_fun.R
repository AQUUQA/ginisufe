library(magrittr)

# 该函数计算组间基尼系数，第二个参数留空可用以计算组内基尼系数
gini <- function(income_j, income_h = NULL) {
  if (is.null(income_h)){
    income_h <- income_j
  }
  C<-matrix(rep(income_j,length(income_h)),nrow = length(income_j))
  D<-matrix(rep(income_h,length(income_j)),nrow = length(income_h))
  mean(mean(abs(C-t(D))))/(mean(income_j)+mean(income_h))

}


