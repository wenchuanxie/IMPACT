#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.ggsurvivalcutoff.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-19
# @IDE     : RStudio
# @Desc    : 对生存分析中使用surv_cutpoint计算不同cutoff值分组计算HR的结果绘图
#===============================================================================

# 函数参数
# @ data      : 数据框，func.survivalcutoff函数的结果。
# @ col.name  : 待分析的对象的名称（列名）
# @ outcomes  : 生存终点

# 函数返回
# @ reply  : ggplot2对象

func.ggsurvivalcutoff <- function(data,col.name,
                                  line.color = '#FF9800', # for best cutoff
                                  text.color = '#FF9800', # for best cutoff
                                  show.median.cuttof = 'FALSE'){

  data <- data %>%
    dplyr::filter(!is.infinite(HCI))

  best.cutoff.df <- data %>%
    dplyr::filter(cuts == data$best.cutoff[1])
  # 在is.infinite(HCI)进行过滤时，当原最大统计量的上限为inf时会被过滤，因此需重新选择最大统计量的cutoff值
  if(nrow(best.cutoff.df) == 0){
    best.cutoff.df <- data %>%
      dplyr::filter(stats == max(data$stats,na.rm = T))
  }

  p.lineplot <- data %>%
    ggplot(aes(x=Cumulative_probability, y=HR,colour =col.name )) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c(line.color)) + # "#e66f66", 07b2b7
    geom_hline(aes(yintercept = 1),linetype = 2,alpha=0.4) +
    # geom_vline(aes(xintercept = 0.5),linetype = 2,alpha=0.4) +
    # geom_text(aes(x=0.5, label="Median cutoff\n", y=1.5), colour="grey",
    #           angle=90, text=element_text(size=11)) +
    # OS
    geom_vline(aes(xintercept = best.cutoff.df$Cumulative_probability[1]),
               linetype = 2,alpha=0.4,colour=line.color) +
    geom_text(aes(x=best.cutoff.df$Cumulative_probability[1],
                  label=paste0("Maxstat cutoff\n",round(best.cutoff.df$HR,2),
                               "(",round(best.cutoff.df$LCI,2),"-",round(best.cutoff.df$HCI,2),")"),
                  y=median(HR,na.rm = T)),
              colour=text.color, angle=90, text=element_text(size=11)) +
    scale_x_continuous(limits = c(0,1),expand = c(0,0))+
    scale_y_continuous(limits = c(0,max(data$HCI,na.rm = T)),expand = c(0,0)) +
    geom_ribbon(aes(ymin=LCI, ymax=HCI), linetype = 3,alpha=0.05) +
    theme_classic() +
    # 本函数调用的数据是固定为H vs L
    ylab(paste0("High"," vs.","Low","(HR & 95%CI)")) +
    xlab("Proportion of samples below cutoff") +
    ggeasy::easy_add_legend_title('') +   # no legend name
    theme( #legend.position = 'none'
           legend.position = c(0.9,0.9)
    )
  # 展示中位分组
  if(show.median.cuttof){
    p.lineplot <- p.lineplot +
      geom_vline(aes(xintercept = 0.5),linetype = 2,alpha=0.4) +
      geom_text(aes(x=0.5, label="Median cutoff\n", y=median(HR,na.rm = T)), colour="grey",
                angle=90, text=element_text(size=11))
  }
  return(p.lineplot)
}
