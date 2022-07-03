#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.psychcorrwide2long.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-08
# @IDE     : RStudio
# @Desc    : 相关系数提取
#===============================================================================

#函数说明
# @ list.dat   : 由psych包的corr.test函数计算的系数结果（list格式）

# 计算指定对象在group两组（High and Low）间的差异
func.psychcorrwide2long <- function(list.dat){
  
  # 提取相关系数
  corr.df <- list.dat
  corr.r <- corr.df$r
  corr.p <- corr.df$p
  corr.padj <- corr.df$p.adj
  
  corr.r.long <- corr.r %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'xname') %>%
    tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'corr')
  corr.p.long <- corr.p %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'xname') %>%
    tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'p')
  corr.padj.long <- corr.padj %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'xname') %>%
    tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'padj')
  corr.long <- cbind(corr.r.long,corr.p.long$p,corr.padj.long$padj)
  colnames(corr.long) <- c('yname','xname','r','p','padj')
  
  corr.long <- corr.long %>%
    dplyr::mutate_at(3, ~ round(.,2)) %>%  # 将corr值（行列名）四舍五入
    dplyr::mutate_at(4:5, ~ round(.,6))  # 将corr值（行列名）四舍五入
  # dplyr::mutate_at(4:5, ~ format(.,digits = 3,scientific = TRUE)) # 科学计数法表示p值
  
  return(corr.long)
}