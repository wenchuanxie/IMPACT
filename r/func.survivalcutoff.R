#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.survivalcutoff.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-19
# @IDE     : RStudio
# @Desc    : 生存分析中，使用surv_cutpoint计算不同cutoff值，分组计算HR
#===============================================================================

# 函数参数
# @ data      : 数据框，必须含有times、status两列，以及需要分析的对象名称列。
# @ col.name  : 待分析的对象的名称（列名）

# 函数返回
# @ reply  : 返回一个数据框: x row * 9 column

func.survivalcutoff <- function(data,col.name){

  cat("Current variant:",col.name, "\n")

  data <- data[complete.cases(data),] # 删除有NA的行
  data <- data %>%
    dplyr::rename(cut_variant = col.name)

  # 寻找最佳分割点，每组最低样本比例不低于10%
  res.cut <- surv_cutpoint(data, time="times", event="status", variables = 'cut_variant')

  # 取出所有分割点cutoff
  # 该结果中stats最大值对应的cuts即为最佳分割点，此处取出所有
  res.cut.df <- data.frame(cuts = res.cut[['cut_variant']]$cuts, stats = res.cut[['cut_variant']]$stats)

  for(ri in 1:nrow(res.cut.df)){
    # ri = 1
    res.cut$cutpoint$cutpoint <- res.cut.df$cuts[ri]
    res.cat <- surv_categorize(res.cut)
    res.cat$cut_variant <- factor(res.cat$cut_variant,levels = c('low','high'))
    tmp.cox <- coxph(Surv(times, status) ~ cut_variant, data = res.cat)
    tmp.coef <- summary(tmp.cox)$coefficients
    tmp.ci <- summary(tmp.cox)$conf.int
    n <- cbind(sum(data$cut_variant <= res.cut.df$cuts[ri])/nrow(data), # 小于指定cutoff的样本数的占比
               tmp.coef[2],
               tmp.ci[3],
               tmp.ci[4],
               tmp.coef[5])

    if(ri == 1){
      tmp.out <- n
    }else{
      tmp.out <- rbind(tmp.out,n)
    }
  }

  out.df <- as.data.frame(cbind(res.cut[['cut_variant']]$cuts,
                                res.cut[['cut_variant']]$stats,
                                tmp.out)) %>%
    dplyr::mutate(Symbol = col.name) %>%
    dplyr::mutate(best.cutoff = res.cut[['cut_variant']]$estimate)
  colnames(out.df)[1:7] <- c('cuts','stats',"Cumulative_probability","HR", "LCI", "HCI", "P")
  return(out.df)
}
