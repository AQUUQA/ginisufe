# 该函数计算组间基尼系数，第二个参数留空可用以计算组内基尼系数
gini <- function(income_j, income_h = NULL) {
  if (is.null(income_h)){
    income_h <- income_j
  }

  (rep(income_j, length(income_h)) - rep(income_h, each=length(income_j))) %>%
    abs %>%
    mean %>%
    ( function(x) { x / (mean(income_j) + mean(income_h)) } )
}
