#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : global.R
# @License : Copyright(C), Yucc
# @Author  : wenchuan.xie
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : 项目自定义函数集合
#===============================================================================

library(shiny)
library(dplyr)
library(stringr)
library(shinythemes)     # install.packages("shinythemes")， 105环境是apper用户下安装
library(shinydashboard)
library(shinyWidgets)
library(rintrojs)
library(colourpicker) # remotes::install_github("daattali/colourpicker")
library(shinysky)
library(DT)
library(prompter)
library(shinyjs)  # 导致报错： Error: colourInput() has been moved to the 'colourpicker' package.
library(shinybusy)
library(shinycssloaders)
library(shinydashboardPlus)
library(shinydisconnect)
library(shinyBS)
library(shinyvalidate)
# library(shinylogs)
library(rmarkdown)

library(RMySQL)
library(RMariaDB)
library(DBI)

library(magrittr)
library(survival)
library(survminer)
library(ggplot2)
library(plotly)
library(ezcox)    # 
library(forester) # devtools::install_github("rdboyes/forester")
library(broom)
library(Hmisc)
library(tibble)
library(ggbeeswarm)
library(cowplot)
library(ggpubr)
library(openxlsx)
library(gridExtra)

library(corrplot)
library(psych) # 
library(ggnewscale)
library(tidyr)
library(GSVA)  # BiocManager::install("GSVA")
library(circlize)
library(ComplexHeatmap) # BiocManager::install("ComplexHeatmap")
library(RColorBrewer)
library(Publish)
library(meta)
library(Cairo)
library(forestploter)
library(ggplotify)
library(grid)
library(gridGraphics)
library(ggstatsplot)
library(ggeasy)
library(summaryBox)
library(jstable)
library(treemap)
library(gridBase)
library(log4r)

# Increase band width for shiny to handle bigger file 
options(shiny.maxRequestSize=100*1024^2) 

log_file <- "./logs/impact_server_debug.log"
file_logger <- logger("INFO", appenders = file_appender(log_file))
info(file_logger, paste("Here's the original log debug message."))

# 临时图片路径，需对应服务器修改，绝对路径！
temp_file_path <- "/opt/shinyapp/IMPACT/temp"     # prod: XWC阿里云路径
# temp_file_path <- "/opt/shinyapp/temp/impact"   # prod: BR内网路径

# 输出文件前缀
prefix_output_file <- 'IMPACT'

source('./r/dataimport.R') 

# load ui/server from each tab
source('./tab_00_ici_explorer/tab.R')
source('./tab_00_prognosis/tab.R')
source('./tab_01_survival_km/tab.R') 
source('./tab_01_survival_cox/tab.R')
source('./tab_01_cutpoint_analyses/tab.R')
source('./tab_02_response/tab.R')
source('./tab_01_subgroup_analyses/tab.R')
source('./tab_01_mut_treatment/tab.R')
source('./tab_02_tumor_immunogenicity/tab.R')
source('./tab_03_tme_corr/tab.R')
source('./tab_03_tme_comp/tab.R')
source('./tab_04_tumor_mutation/tab.R')
source('./tab_05_data_resource/tab.R')
source('./tab_08_user_defined/tab.R')

# source("./r/chunkModule.R")

#Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button")
  )
}

#Creating small boxes for further information in the landing page (see ui for formatting css)
lp_about_box <- function(title_box, image_name, button_name, description) {
  
  div(class="landing-page-box-about",
      div(title_box, class = "landing-page-box-title"),
      div(class = "landing-page-about-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      (actionButton(button_name, NULL,
                    class="landing-page-button",
                    icon = icon("arrow-circle-right", "icon-lp"),title=description)))
}

# plotly survival plot
plotly_survival <- function(plotdata,
                            kw.title = '',
                            time.break = 10,
                            color.dis = c("#F44336","#4CAF50"),
                            y.title = 'Survival probability'){
  sfit <- survfit(Surv(times,status) ~ risk,data = plotdata)
  p <- ggsurvplot(sfit, 
                  data = plotdata, 
                  conf.int = F, #置信区间
                  title = kw.title,
                  pval = T,pval.method = T,surv.median.line = "hv",
                  palette = color.dis,
                  risk.table = T,
                  risk.table.pos = "out",
                  risk.table.y.text.col = T,
                  risk.table.y.text = TRUE,
                  ncensor.plot = F,
                  xlim = c(0, max(plotdata$times)*1.1),
                  legend = 'right',
                  legend.title = '',
                  break.time.by = time.break,
                  legend.labs = c(paste0(names(table(plotdata$risk)[1])),
                                  paste0(names(table(plotdata$risk)[2])))
  ) +  # 根据factor指定的顺序给定改字符串顺序
    labs(x = paste0("Months"),y = y.title) 
  return(p)
}
# 获取x轴时间分组数目
time_break_survival <- function(stime){
  
  tbreak <- as.numeric(stime) / 5
  if(tbreak <= 15) return(10)
  if(tbreak <= 25 & tbreak > 15) return(20)
  if(tbreak <= 35 & tbreak > 25) return(30)
  if(tbreak <= 45 & tbreak > 35) return(40)
  if(tbreak > 45) return(50)
}

# 表格编辑
dt_output = function(title, id) {
  fluidRow(column(
    12, 
    h1(title),
    # hr(), 
    DTOutput(id)
  ))
}
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}



# ref: https://github.com/rstudio/shinydashboard/issues/119
prgoressBar <- function(value = 0, label = FALSE, color = "aqua", size = NULL,
                        striped = FALSE, active = FALSE, vertical = FALSE) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
    stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "2em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}

progressGroup <- function(text, value, min = 0, max = value, color = "aqua") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    prgoressBar(round(value / max * 100), color = color, size = "sm")
  )
}


# 删除全为NA 的列
func.removeColsAllNa  <- function(x){
  delNa.df <- x[, apply(x, 2, function(y) any(!is.na(y)))]
  return(delNa.df)
}
# 删除全为NA的行
func.removeRowsAllNa  <- function(x){
  delNa.df <- x[apply(x, 1, function(y) any(!is.na(y))),]
  return(delNa.df)
}

# 删除全为空的列
func.removeColsAllEmpty  <- function(x){
  delEpy.df <- x[, apply(x, 2, function(y) any(y != "" ))]
  return(delEpy.df)
}

# 空白图
p.noinfo <- ggplot2::ggplot() +
  theme_void() +
  geom_text(aes(0, 0, label = "NOT APPLICABLE!"),
            colour = "grey", fontface = "bold", size = 8
  ) +
  xlab(NULL)


# function for subgroup analyses
# 数据整理
func.table4subplot2 <- function(cli.names,data.df){
  temp.df <- data.df %>%
    dplyr::filter(Group == cli.names) %>%
    dplyr::distinct(Group,.keep_all = T) %>%
    dplyr::select(Characteristic = Group,Size,HR,L95,U95,HR95CI,P.value,P.interaction) %>%
    dplyr::mutate(Size = NA,HR = NA,L95=NA,U95=NA,P.value=NA,HR95CI = NA)
  out.df <- data.df %>%
    dplyr::filter(Group == cli.names) %>%
    dplyr::select(-Group,-P.interaction) %>%
    dplyr::mutate(Characteristic = paste0("  ",Characteristic)) %>%
    dplyr::bind_rows(temp.df,.)
  return(out.df)
}

# cutoff分析中临床因素的选择：连续性变量
# 在此处列全，每个队列都与其取交集，则为该队列的连续性变量
vars.continues <- c("tmb","pdl1_exp","neoantigen","tumor_purity","ploidy","cyt","scna",
                   "aneuploidy","methylation",
                   "wgii","ith",
                   "hrd","ntai","lst","loh","tls_score",
                   "stemness_score","chr_instability_idx",
                   "fraction_genome_gltered",
                   "stromal_score","immune_score","estimate_score")

valueBox_diy <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

valueBox_diy2 <- function(value, subtitle, icon, color) {
  div(class = "col-lg-4 col-md-4",style = "border-color: white;",
      div(class = "panel panel-primary",style = "border-color: white;",
          div(class = "panel-heading", style = paste0("background-color:", color,";border-color: white;"),
              div(class = "row",
                  div(class = "col-xs-4",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-8 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          )
          # div(class = "panel-footer",
          #     div(class = "clearfix")
          # )
      )
  )
}

# 预定义基因集加载
{
  
  pre_genesets_list <- list()
  pre_genesets_list[["AR Signaling"]] <- c("SOX9","RAN","TNK2","EP300","PXN","NCOA2","AR","NRIP1","NCOR1","NCOR2")
  pre_genesets_list[["AR and steroid synthesis enzymes"]] <- c("AKR1C3","AR","CYB5A","CYP11A1","CYP11B1","CYP11B2","CYP17A1","CYP19A1","CYP21A2","HSD17B1","HSD17B10","HSD17B11","HSD17B12","HSD17B13","HSD17B14","HSD17B2","HSD17B3","HSD17B4","HSD17B6","HSD17B7","HSD17B8","HSD3B1","HSD3B2","HSD3B7","RDH5","SHBG","SRD5A1","SRD5A2","SRD5A3","STAR")
  pre_genesets_list[["Steroid inactivating genes"]] <- c("AKR1C1","AKR1C2","AKR1C4","CYP3A4","CYP3A43","CYP3A5","CYP3A7","UGT2B15","UGT2B17","UGT2B7")
  pre_genesets_list[["TP53 Pathways"]] <- c("CDKN2A","MDM2","MDM4","TP53")
  pre_genesets_list[["RTK/Ras/PI3K/AKT Signaling"]] <- c("EGFR","ERBB2","PDGFRA","MET","KRAS","NRAS","HRAS","NF1","SPRY2","FOXO1","FOXO3","AKT1","AKT2","AKT3","PIK3R1","PIK3CA","PTEN")
  pre_genesets_list[["RB Pathways"]] <- c("CDKN2A","CDKN2B","CDKN2C","CDK4","CDK6","CCND2","RB1")
  pre_genesets_list[["Notch Signaling"]] <- c("ADAM10","ADAM17","APH1A","APH1B","ARRDC1","CIR1","CTBP1","CTBP2","CUL1","DLL1","DLL3","DLL4","DTX1","DTX2","DTX3","DTX3L","DTX4","EP300","FBXW7","HDAC1","HDAC2","HES1","HES5","HEYL","ITCH","JAG1","JAG2","KDM5A","LFNG","MAML1","MAML2","MAML3","MFNG","NCOR2","NCSTN","NOTCH1","NOTCH2","NOTCH3","NOTCH4","NRARP","NUMB","NUMBL","PSEN1","PSEN2","PSENEN","RBPJ","RBPJL","RFNG","SNW1","SPEN","HES2","HES4","HES7","HEY1","HEY2")
  pre_genesets_list[["DNA Damage Response"]] <- c("CHEK1","CHEK2","RAD51","BRCA1","BRCA2","MLH1","MSH2","ATM","ATR","MDC1","PARP1","FANCF")
  pre_genesets_list[["TGF-beta Pathways"]] <- c("TGFB1","TGFB2","TGFB3","TGFBR1","TGFBR2","TGFBR3","BMP2","BMP3","BMP4","BMP5","BMP6","BMP7","GDF2","BMP10","BMP15","BMPR1A","BMPR1B","BMPR2","ACVR1","ACVR1B","ACVR1C","ACVR2A","ACVR2B","ACVRL1","Nodal","GDF1","GDF11","INHA","INHBA","INHBB","INHBC","INHBE","SMAD2","SMAD3","SMAD1","SMAD5","SMAD4","SMAD9","SMAD6","SMAD7","SPTBN1","TGFBRAP1","ZFYVE9")
  pre_genesets_list[["Survival/cell death regulation signaling"]] <- c("NFKB1","NFKB2","CHUK","DIRAS3","FAS","HLA-G","BAD","BCL2","BCL2L1","APAF1","CASP9","CASP8","CASP10","CASP3","CASP6","CASP7","GSK3B","ARL11","WWOX","PEG3","TGFB1","TGFBR1","TGFBR2")
  
  
}

### 首页treemap元素点击
tm_locate <- function(coor, tmSave) {
  tm <- tmSave$tm
  
  # retrieve selected rectangle
  rectInd <- which(tm$x0 < coor[1] &
                     (tm$x0 + tm$w) > coor[1] &
                     tm$y0 < coor[2] &
                     (tm$y0 + tm$h) > coor[2])
  
  return(tm[rectInd[1], ])
  
}
