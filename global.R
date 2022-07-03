#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : global.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : 项目自定义函数集合
#===============================================================================

# Sys.setenv(http_proxy="http://10.12.3.1:3128")
# Sys.setenv(https_proxy="https://10.12.3.1:3128")

# options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
# options(BioC_mirror="https://mirrors.tuna.tsinghua.edu.cn/bioconductor")

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

library(corrplot)
library(psych) # 安装问题参考：https://stackoverflow.com/questions/16710047/usr-bin-ld-cannot-find-lnameofthelibrary
library(ggnewscale)
library(tidyr)
library(GSVA)  # BiocManager::install("GSVA")
library(circlize)
library(ComplexHeatmap) # BiocManager::install("ComplexHeatmap")
library(RColorBrewer)
library(Publish)
library(meta)
library(Cairo)

# Increase band width for shiny to handle bigger file 
# options(shiny.maxRequestSize=30*1024^2) 
options(shiny.maxRequestSize=100*1024^2) 

# 临时图片路径，需对应服务器修改，绝对路径！
temp_file_path <- "/Users/xiewenchuan/Documents/Rprojects/Pro3BNR/A02.proj/P20.OmicsMiner/IMPACT/temp"
# temp_file_path <- "/opt/shinyapp/IMPACT/temp"   # prod

# 输出文件前缀
prefix_output_file <- 'IMPACT'

source('./r/dataimport.R') 

# load ui/server from each tab
source('./tab_00_ici_explorer/tab.R')
source('./tab_00_prognosis/tab.R')
source('./tab_01_survival_km/tab.R') 
source('./tab_01_survival_cox/tab.R')
source('./tab_01_subgroup_analyses/tab.R')
source('./tab_01_mut_treatment/tab.R')
source('./tab_02_tumor_immunogenicity/tab.R')
source('./tab_03_tme_corr/tab.R')
source('./tab_03_tme_comp/tab.R')
source('./tab_04_tumor_mutation/tab.R')
source('./tab_05_data_resource/tab.R')
# source('./tab_06_about/tab.R')      # Rmd文件代替
# source('./tab_07_tutorial/tab.R') # Rmd文件代替

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
# ggsave(filename = "./www/demo.not.applicable.png",p.noinfo,width = 6,height = 4)

# 选择输入框，两层菜单显示
# js <- HTML("
# $(function() {
#   let observer = new MutationObserver(callback);
# 
#   function clickHandler(evt) {
#     Shiny.setInputValue('group_select', $(this).children('span').text());
#   }
# 
#   function callback(mutations) {
#     for (let mutation of mutations) {
#       if (mutation.type === 'childList') {
#         $('.dropdown-header').on('click', clickHandler).css('cursor', 'pointer');
#         
#       }
#     }
#   }
# 
#   let options = {
#     childList: true,
#   };
# 
#   observer.observe($('.inner')[0], options);
# })
# ")

