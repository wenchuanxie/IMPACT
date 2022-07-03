#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.bubbleplot.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-02
# @IDE     : RStudio
# @Desc    : 相关系数气泡图绘制
#===============================================================================

#函数说明
# @ data     : 相关系数矩阵，长数据格式。五列：xname,yname,r,p,padj;
# @ color1   : 高值的颜色
# @ color2   : 低值的颜色
# @ xlevel   : x轴指标的顺序, x轴为选定的免疫指标（基因或者cell或者signature的名称）
# @ ylevel   : y轴指标的顺序，y轴为选择的队列的基因

# 相关系数和p值进行绘图
func.bubbleplot <- function(data,
                            color1 = "#2165B0",
                            color2 = "#B11F2C",
                            xlevel,
                            ylevel){
  
  p.corr <- data %>%
    dplyr::mutate(lgQ = log2(p)) %>%
    dplyr::select(xname,yname,Rho = r,lgQ) %>%
    dplyr::mutate(xname = factor(xname,levels = xlevel)) %>%  # 约定顺序
    dplyr::mutate(yname = factor(yname,levels = ylevel)) %>%
    ggplot(aes(x = xname, y = yname, size = -lgQ, color=Rho)) + 
    geom_point() +
    scale_color_gradient2(low = color2, mid="#EEEEEE", high = color1, limits=c(-1,1),na.value = "#E6E6E6") +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.border = element_rect(colour="black",fill=NA),
          axis.text = element_text(face = 'bold'),legend.position = "bottom")
  return(p.corr)
}