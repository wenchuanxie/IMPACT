#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.bacthwilcoxtest.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : 自定义批量秩和检验
#===============================================================================

#函数说明
# @ obs   : Y轴属性，免疫原性名称
# @ data  : 比较分析数据，三列：smaple_id,group,[group的原始基因连续变量],[其他待分析的对象，基因或者cell singature]

# 计算指定对象在group两组（High and Low）间的差异
func.bacthwilcoxtest <- function(obs,data){
  # cat("=======> obs: ",obs,"\n")
  # browser()
  t.df <- data %>%
    dplyr::select(sample_id,group,calc_obs = obs) %>%
    tibble::as_tibble()
  g.mt = t.df[which(t.df$group == 'High'),'calc_obs']
  g.wt = t.df[which(t.df$group == 'Low'),'calc_obs']
  if(nrow(g.mt) >0 & nrow(g.wt) > 0 ){
    res <- wilcox.test(x = g.mt$calc_obs, y = g.wt$calc_obs)
    # res <- wilcox.test(x = g.mt, y = g.wt)
    res.df <- data.frame(calc_obs = obs,
                         num_high = nrow(g.mt),
                         num_low = nrow(g.wt),
                         # num_high = length(g.mt),
                         # num_low = length(g.wt),
                         p.value = round(res$p.value,4),
                         p.value.str = ifelse(res$p.value < 0.001, '<0.001',round(res$p.value,4)),
                         p.sig = ifelse(res$p.value < 0.001,'***',
                                      ifelse(res$p.value < 0.01,"**",
                                             ifelse(res$p.value < 0.05,'*',''))))
    return(res.df)
  }
  
}