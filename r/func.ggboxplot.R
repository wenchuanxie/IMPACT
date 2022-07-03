#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.ggboxplot.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : 自定义箱线图
#===============================================================================

#函数说明
# @ data    : 绘图数据，三列：smaple_id,group,img_name(临床特征名，连续变量)
# @ group1  : 第一组的名称
# @ group2  : 第二组的名称
# @ color1  : 第一组的颜色，默认 #FF9800（黄色，默认为High和Mut的颜色）
# @ color2  : 第二组的颜色，默认 #2196F3（蓝色，默认为Low和Wt的颜色）
# @ ytitle  : Y轴标题，默认为空

func.ggboxplot <- function(data,
                           group1,
                           group2,
                           color1 = "#FF9800", 
                           color2 = '#2196F3',
                           ytitle = ""){
  
  # cat("---->12143234",tolower(unique(data$group)[1]) == 'wt',"\n")
  if(length(unique(data$group)) == 1){
    # 只有一组时
    p.out <- ggplot(data, aes(group,img_name)) +
      geom_boxplot(
        aes(fill = group), width = 0.8,
        position = position_dodge(0.9)
      ) + 
      # geom_beeswarm(aes(color = group),cex = 1.5, show.legend = FALSE) +         #cex 可以通过点的尺寸定义宽度
      theme_bw() + 
      labs(x = '', y = toupper(ytitle)) +
      scale_fill_manual(values = c(case_when(tolower(unique(data$group)[1]) == 'mut' ~ color1,
                                             TRUE ~ color2))) +
      scale_x_discrete(limits = c(unique(data$group)))+ # 将坐标轴显示的值更改
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 0))
  }else{
    p.out <- ggplot(data, aes(x = group, y = img_name))  +
      geom_boxplot(
        aes(fill = group), width = 0.8,
        position = position_dodge(0.9)
      ) +
      theme_bw() +
      # stat_summary(fun = mean, geom="point",colour="darkred", size=3) +
      # stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7,aes( label=round(..y.., digits=2)))+
      xlab("")+ylab(toupper(ytitle)) +
      scale_fill_manual(values = c(color2,color1)) +
      scale_x_discrete(limits = c(group2,group1))+ # 将坐标轴显示的值更改
      theme(legend.position = 'none',
            axis.ticks = element_line(size = 2),
            axis.text.x = element_text(angle = 0,size = 12)) +
      # facet_grid(.~category) +
      geom_signif(comparisons=list(c(group1,group2)),
                  map_signif_level=function(p){ifelse(p < 0.001,'Wilcoxon test, P<0.001',paste0("Wilcoxon test, P=",round(p,3)))} )
  }
  
  return(p.out)
}