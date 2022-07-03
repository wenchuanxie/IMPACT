#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.scatterbwplot.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : 自定义箱线图
#===============================================================================

#函数说明
# @ data     : 绘图数据，三列：smaple_id,group,value
# @ img_name : Y轴属性，免疫原性名称
# @ gene_name: X轴属性，基因名
# @ color1   : 第一组的颜色，默认 #FF9800
# @ color2   : 第二组的颜色，默认 #2196F3

func.scatterbwplot <- function(data,img_name,gene_name,color1 = "#FF9800", color2 = "#2196F3"){
  if("mut" %in% tolower(data$group) |
     "wt" %in% tolower(data$group)){
    # 突变只绘制蜂群图
    # p.bw <- func.beeswarmplot(data,c(color1,color2),ytitle = img_name)
    p.bw <- func.ggboxplot(data,unique(data$group)[1],unique(data$group)[2],color1,color2,ytitle = img_name)
    
    p.out <- p.bw
  }else{
    # print(data[1:5,])
    # 对表达值绘相关系数图和蜂群图
    p.scatter <- ggscatter(data, x = "sltgene", y = "img_name",
                           color = 'group',
                           add = "reg.line",                                 # Add regression line
                           conf.int = TRUE,                                  # Add confidence interval
                           add.params = list(color = "#F44336CC", #F44336CC
                                             fill = "lightgray"))+
      stat_cor(method = "spearman") +
      ylab(toupper(img_name)) + 
      xlab(gene_name) + 
      theme_cowplot() +
      scale_color_manual(values = c(color1,color2))
    # p.bw <- func.beeswarmplot(data,c(color1,color2),ytitle = img_name)
    p.bw <- func.ggboxplot(data,unique(data$group)[1],unique(data$group)[2],color1,color2,ytitle = img_name)
    # Arranging the plot using cowplot
    # Cleaning the plots
    p.scatter <- p.scatter + rremove("legend")
    p.bw <- p.bw  + rremove("legend")
    p.out <- plot_grid(p.scatter, p.bw, ncol = 2, align = "hv", rel_widths = c(2, 1))
  }

  return(p.out)
}