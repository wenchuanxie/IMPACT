#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.bacthchitest.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-19
# @IDE     : RStudio
# @Desc    : 自定义批量卡方检验
#===============================================================================

#函数说明
# @ obs   : Y轴属性，免疫原性名称
# @ data  : 比较分析数据，三列：smaple_id,group,[group的原始基因连续变量],[其他待分析的对象，基因或者cell singature]

# 计算指定对象在group两组间某指标的差异
func.bacthchitest <- function(property_name,data){
  t.df <- data %>%
    dplyr::select(Type = group,Group = property_name)
  
  # 卡方检验
  x.table <- xtabs(~ Type + Group, data = t.df) # 构建列联表，进行四格表检验
  chisq.res <- chisq.test(x.table,correct = T)  # correct = T 表示Yates校正
  chisq.res$statistic
  vcd::assocstats(x.table)
  chisq.res.exp <- as.data.frame(chisq.res$expected) %>%
    tibble::rownames_to_column(var = 'test')%>%
    tidyr::gather(type,value,-test)
  # 所有理论数T≥5
  T5 <- as.data.frame(apply(chisq.res$expected, 2, function(x) x>=5)) %>%
    tibble::rownames_to_column(var = 'test')%>%
    tidyr::gather(type,value,-test)
  # 所有理论数5>T≥1
  T1 <- as.data.frame(apply(chisq.res$expected, 2, function(x) x>=1 & x<5))%>%
    tibble::rownames_to_column(var = 'test')%>%
    tidyr::gather(type,value,-test)
  
  if(sum(as.numeric(chisq.res.exp$value)) >= 40 & !(FALSE %in% as.logical(T5$value))){
    p.res <- chisq.test(x.table)$p.value
    p.method <- c('Pearson Chisq')
    # cat("===> ",property_name,",所有理论数T≥5并且总样本量n≥40,用Pearson卡方进行检验:",p.res,"\n")
  }else if(sum(as.numeric(chisq.res.exp$value)) >= 40 & !(FALSE %in% as.logical(T1$value))){
    p.res <- chisq.test(x.table,correct = T)$p.value
    p.method <- c('Yates Chisq')
    # cat("===> ",property_name,",所有理论数5>T≥1,并且n≥40,用连续性校正的卡方进行检验:",p.res,"\n")
  }else{
    p.res <- fisher.test(x.table)$p.value
    p.method <- c('Fisher')
    # cat("===> ",property_name,",有理论数T＜1或n＜40,则用Fisher’s检验:",p.res,"\n")
  }
  p.df <- data.frame(characterestics = property_name,
                     p.method,
                     p.res.str = ifelse(p.res < 0.001, '<0.001',round(p.res,4)),
                     p.res = round(p.res,4))
  return(p.df)
}