#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.beeswarmplot
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : 自定义蜂群图
#===============================================================================

#函数说明
# @ data      : 绘图数据，三列：smaple_id,group,img_name
# @ bp_color  : 分组颜色名称
# @ ytitle    : Y轴标题，默认为空

# 分组绘制连续性变量的箱线/蜂群图绘制
func.beeswarmplot <- function(data,bp_color = c("#F44336CC","#4CAF50"),ytitle = ""){
  # data$group <- capitalize(tolower(as.character(data$group)))
  if(length(unique(data$group)) == 1){
    # 只有一组时
    p.bw <- ggplot(data, aes(group,img_name)) +
      geom_beeswarm(aes(color = group),cex = 1.5, show.legend = FALSE) +         #cex 可以通过点的尺寸定义宽度
      theme_cowplot() + 
      labs(x = '', y = ytitle) +
      scale_color_manual(values = bp_color[1]) +  
      stat_summary(fun = median, fun.min = median, fun.max = median, 
                   geom = 'crossbar', width = 0.3, size = 0.5, color = 'black') +
      stat_summary(fun.data = function(x) median_hilow(x, 0.5), 
                   geom = 'errorbar', width = 0.25, color = 'black') + 
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 0))
  }else{
    p.bw <- ggplot(data, aes(group,img_name)) +
      geom_beeswarm(aes(color = group),cex = 1.5, show.legend = FALSE) +         #cex 可以通过点的尺寸定义宽度
      theme_cowplot() + 
      labs(x = '', y = ytitle) +
      scale_color_manual(values = bp_color) +  
      stat_summary(fun = median, fun.min = median, fun.max = median, 
                   geom = 'crossbar', width = 0.3, size = 0.5, color = 'black') +
      stat_summary(fun.data = function(x) median_hilow(x, 0.5), 
                   geom = 'errorbar', width = 0.25, color = 'black') + 
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 0)) 
    # Add the statistical results of multi group comparison
    if("High" %in% data$group){
      p.bw <- p.bw + 
        ggsignif::geom_signif(comparisons=list(c('Low','High')),
                              map_signif_level=function(p){ifelse(p < 0.001,'<0.001',round(p,3))} )
    }else{
      p.bw <- p.bw + 
        ggsignif::geom_signif(comparisons=list(c('Mut','Wt')),
                              map_signif_level=function(p){ifelse(p < 0.001,'<0.001',round(p,3))} )
    }
    
  }
  
  return(p.bw)
}