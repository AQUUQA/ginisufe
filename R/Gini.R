#输入两列数据
#输出组内组间（一个矩阵）超变（矩阵）+差异净值（矩阵）
#gini(组内或者组间，两组之间比较)

# 该函数计算两组之间的总经济影响富裕d_jh
gross_afflue <- function(income_j, income_h){

  # 确保j组的平均收入不小于h组的平均收入
  if (mean(income_j) < mean(income_h)){
    M <- income_j
    income_j <- income_h
    income_h <- M
  }
  # 令j组元素与h组元素两两互减
  D<-matrix(rep(income_j,length(income_h)),nrow = length(income_j))-
    t(matrix(rep(income_h,length(income_j)),nrow = length(income_h)))
  D[D<0]<-0
  sum(D)/length(income_j) / length(income_h)

}

# 该函数计算两组之间的超变一阶矩pjh
transv <- function(income_j, income_h){
  # 确保j组的平均收入不小于h组的平均收入
  if (mean(income_j) < mean(income_h)){
    M <- income_j
    income_j <- income_h
    income_h <- M
  }

  # 令j组元素与h组元素两两互减
  D<-matrix(rep(income_j,length(income_h)),nrow = length(income_j))-
    t(matrix(rep(income_h,length(income_j)),nrow = length(income_h)))
  D[D>0]<-0
  sum(D)/length(income_j) / length(income_h)
}

# 该函数计算相对经济富裕D_jh
relative_economic_affluence <- function(income_j, income_h){
  d_jh <- gross_economic_affluence(income_j, income_h)
  p_jh <- transvariation(income_j, income_h)

  (d_jh - p_jh) / (d_jh + p_jh)
}



dagum_gini <- function(model, df){

  # 为计算基尼系数准备数据
  income <- model.response(model.frame(model, df))  # 抽取收入数据
  group <- model %>%
    format %>%
    strsplit('~') %>%
    ( function(x) x[[1]][2] ) %>%
    trimws %>%
    ( function(x) df[[x]] ) %>%
    unique

  # 计算总体基尼系数
  total_gini_index <- gini(income)

  # 分解总体基尼系数
  within <- 0
  net_between <- 0
  trans <- 0
  gini_matrix <- matrix(0, length(group), length(group),
                        dimnames = list(group, group))
  rea_matrix <- gini_matrix

  for (j in group) {
    for (h in group){
      income_j <- income[group == j]
      income_h <- income[group == h]
      G_jh <- gini(income_j, income_h)
      p_j <- length(income_j) / length(income)
      s_h <- sum(income_h) / sum(income)
      item <- G_jh * p_j * s_h

      gini_matrix[group == j, group == h] <- G_jh

      if (j == h){
        within <- within + item
      } else {
        d_jh = relative_economic_affluence(income_j, income_h)
        net_between <- net_between + item * d_jh
        trans <- trans + item * (1 - d_jh)
        rea_matrix[group == j, group == h] <- d_jh
      }
    }
  }

  list(
    index = data.frame(
      '指标' = c('绝对数值', '相对份额(%)'),
      '总体基尼系数' = c(total_gini_index, 100),
      '组内差异贡献' = c(within, 100 * within / total_gini_index),
      '组间差异净贡献' = c(net_between, 100 * net_between / total_gini_index),
      '组间超变密度贡献' = c(trans, 100 * trans / total_gini_index)),
    gini = data.frame(gini_matrix),
    rea = data.frame(rea_matrix)
  )
}
