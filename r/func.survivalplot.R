# -*- coding: utf-8 -*-
# @File    : func.survivalplot.R
# @License : Copyright(C), WEN
# @Author  : gzlyc1228@163.com
# @Time    : 2020/07/21 15:00 
# @IDE     : RStudio
# @Desc    : KM绘图自定义函数

func.survivalplot = function(plotdata,kw.title,time.break = 10,key.stye = 'OS',tbl.loc = "topright",color.dis = c("#F44336","#4CAF50"),y.title = 'Survival probability'){
  #' @plotdata：为数据框，必含几下几个字段：times（时间维度为Month）,status, risk(分类值，如0和1，High和low)
  #' @kw_title:关键字符串
  {
    # R coxph中，两组（risk：mut vs wt）比较，默认首字母在前的组是ref组（mut），另一组是对比组（wt），即后者相对于前者比较，是什么结果（HR的解释）
    # 若需要反过来比较，即mut比wt是什么结果，则执行以下语句（或者通过factor指定，本处在调用函数前设定好ref，因此不执行下述语句）：
    # plotdata$risk  <- plotdata$risk %>% 
    #   as.factor() %>% 
    #   forcats::fct_rev()
    # plotdata <- plot.df
    # key.stye <- 'PFS'
    data.survdiff <- survdiff(Surv(plotdata$times, plotdata$status, type = 'right') ~ plotdata$risk, data = plotdata)
    p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
    x = summary(coxph(Surv(times, status,type = 'right')~risk, data = plotdata,method = 'efron'))
    HR = signif(x$coef[2], digits=2)
    up95 = signif(x$conf.int[,"upper .95"],2)
    low95 = signif(x$conf.int[,"lower .95"], 2)
    HR.v <- round(HR,2)
    HR <- paste("HR:", HR.v, sep = "")
    CI.v = paste(round(low95,2), round(up95,2), sep = "-")
    CI <- paste("95%CI:", CI.v, sep = "")
    P.v = ifelse(p.val < 0.001, "P < 0.001", paste("P = ",round(p.val,3), sep = ""))
  }
  #Sur <- Surv(plotdata$times,plotdata$status)
  sfit <- survfit(Surv(times,status) ~ risk,data = plotdata)
  if(F){
    # 20220321修改
    # 绘制信息表格
    mtable <- surv_median(sfit) # 求中位生存时间
    mtable <- mtable %>%
      dplyr::select(strata,Mid = median) %>%
      dplyr::mutate(strata = substr(strata,6,nchar(strata)),
                    Mid = round(Mid,2),
                    `HR(95%CI)` = paste0(HR.v," (",CI.v,")"),
                    `Log-rank` = ifelse(p.val < 0.001, "P < 0.001", paste0("P = ",round(p.val,3))))
    colnames(mtable)[1] = ''
    colnames(mtable)[2] = paste0('m',key.stye,"(mo)")
    # mtable[1,1] = paste0(names(table(plotdata$risk)[1]),"(",table(plotdata$risk)[[1]],")")
    # mtable[2,1] = paste0(names(table(plotdata$risk)[2]),"(",table(plotdata$risk)[[2]],")")
    mtable[2,c(3,4)] = ''
    # t(mtable)
    stable.p <- ggtexttable(t(mtable),theme = ttheme("blank",base_size = 12))
    stable.p <- stable.p %>%
      tab_add_hline(at.row = c(1, 2), from.column = 2,row.side = "top", linewidth = 2, linetype = 1)
    stable.p <- table_cell_font(stable.p, row = 1, column = 2,color = color.dis[1])
    stable.p <- table_cell_font(stable.p, row = 1, column = 3,color = color.dis[2])
  }
  {
    # 20220623修改,不实用表格
    mtable <- surv_median(sfit) # 求中位生存时间
    mtable <- mtable %>%
      dplyr::select(strata) %>%
      dplyr::mutate(strata = substr(strata,6,nchar(strata)),
                    `HR(95%CI)` = paste0(HR.v," (",CI.v,")"),
                    `Log-rank` = ifelse(p.val < 0.001, "P < 0.001", paste0("P = ",round(p.val,3))))
    colnames(mtable)[1] = ''
    tmp.name <- mtable[1,1]
    mtable[1,1] = mtable[2,1]
    mtable[2,1] = tmp.name
    mtable[2,c(2,3)] = ''
    # t(mtable)
    stable.p <- ggtexttable(t(mtable),theme = ttheme("blank",base_size = 12))
    stable.p <- table_cell_font(stable.p, row = 1, column = 2,color = color.dis[2])
    stable.p <- table_cell_font(stable.p, row = 1, column = 3,color = color.dis[1])
    # stable.p <- paste0(levels(plotdata$risk)[2]," vs. ",levels(plotdata$risk)[1],
    #                    " \n HR(95%CI): ",paste0(HR.v," (",CI.v,")"),
    #                    " \n Log-rank P ",
    #                    ifelse(p.val < 0.001, "< 0.001", paste0("= ",round(p.val,3))))
    
  }
  p <- ggsurvplot(sfit, 
                  data = plotdata, 
                  conf.int = F, #置信区间
                  title = kw.title,
                  palette = color.dis,
                  risk.table = T,
                  risk.table.pos = "out",
                  risk.table.y.text.col = T,
                  risk.table.y.text = TRUE,
                  ncensor.plot = F,
                  xlim = c(0, max(plotdata$times)*1.1),
                  legend = 'none',  
                  legend.title = '',
                  break.time.by = time.break,
                  legend.labs = c(paste0(names(table(plotdata$risk)[1])),
                                  paste0(names(table(plotdata$risk)[2])))
                  ) +  # 根据factor指定的顺序给定改字符串顺序
    labs(x = paste0("Months"),y = y.title) 
  
  if(tbl.loc == "topright"){
    p$plot <- p$plot + annotation_custom(ggplotGrob(stable.p),
                                       xmin = max(plotdata$times) * 0.55,
                                       xmax = max(plotdata$times) * 0.85,
                                       ymin = 0.75,ymax = 0.95)
  }
  if(tbl.loc == "bottomright"){
    p$plot <- p$plot + annotation_custom(ggplotGrob(stable.p), 
                                       xmin = max(plotdata$times) * 0.65, 
                                       xmax = max(plotdata$times) * 0.95, 
                                       ymin = 0.15,ymax = 0.35) 
  }
  if(tbl.loc == "bottomleft"){
    p$plot <- p$plot + annotation_custom(ggplotGrob(stable.p), 
                                       xmin = max(plotdata$times) * 0.15,
                                       xmax = max(plotdata$times) * 0.45, 
                                       ymin = 0.15,ymax = 0.35)
  }
  
  p$table <- p$table + theme(axis.line = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.y = element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_blank(),
                             plot.title = element_text(size=12))
  return(p)
}
