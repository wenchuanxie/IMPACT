#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-06-23
# @IDE     : RStudio
# @Desc    : 第一个Tab功能的第4个子功能： 基因突变与治疗的交互影响，只在肾癌的Chechmate025队列分析（免疫治疗和化疗）
#===============================================================================

# source("./r/func.factorization.R")

tab_01_mut_treatment <- list()
# UI
sidebar <- sidebarPanel(
  id = 'mutntreat_sidebar',
  width = 3,
  h3("Interaction Analyses"),
  
  selectizeInput(inputId = "mutntreat_symbol_id", 
                 label = "Select/Input gene symbol, E.g. 'EGFR STK11'", 
                 choices = NULL, 
                 width = '100%',
                 multiple = T, 
                 options = list(delimiter = " ", create = T,
                                maxItems = 10,
                                placeholder = "No more than 10 genes.",
                                plugins = list('remove_button', 'drag_drop'))
                 ),
  pickerInput(inputId = "mutntreat_vartype_id",
              label = tags$span(
                add_prompt(tags$span(icon(name = "question-circle")),
                           message = "Variant Classification for Mutation data", 
                           position = "right"),
                "Select gene mutation types"),
              choices = NULL,
              multiple = TRUE,
              selected = NULL),
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "mutntreat_colorg1_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "question-circle")),
                                                    message = "Color for Mut group", 
                                                    position = "right"),'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(5,colourpicker::colourInput(inputId = "mutntreat_colorg2_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "question-circle")),
                                                    message = "Color for Wt group", 
                                                    position = "right"),'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))
  ),
  # 提交按钮
  actionButton(inputId = "mutntreat_goButton",
               label = "Submit",
               class ="btn-primary"
  )
)

mainpage <- mainPanel(
  id = 'mutntreat_mainpage',
  width = 9,
  uiOutput(outputId='mutntreat_maintabs')
)

tab_01_mut_treatment$ui <- sidebarLayout(sidebar, mainpage)

# Server
tab_01_mut_treatment$server <- function(input, output,session){
  
  cat("=========================== Start Interaction =======================\n")
  
  # 定义变量存储输出对象
  output.graphic.interaction <- reactiveValues()
  
  # 根据队列，根据symbol和vartype
  # 本处只选择KIRC的Checkmate025队列
  observe({
    
    updateSelectizeInput(session = session,
                         inputId = "mutntreat_symbol_id",
                         choices = unique(cm025.mut$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, 
                      inputId = "mutntreat_vartype_id",
                      choices = unique(cm025.mut$variant_classification), 
                      selected = unique(cm025.mut$variant_classification),
                      options = list(`actions-box` = TRUE)) 
  })
  
  # interaction分析
  observeEvent(input$mutntreat_goButton,{
    
    # 进度条
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#e95420",
      duration = 90,
      easing = "easeOut",
      text = "Starting Interaction Analyses..."
    )
    
    # 参数值
    input.symbols <- input$mutntreat_symbol_id
    cat("Interaction - 选择的基因名称: ", input.symbols, "\n")
    input.vartype <- input$mutntreat_vartype_id
    cat("Interaction - 选择的基因突变类型: ", input.vartype, "\n")
    input.colorg1 <- input$mutntreat_colorg1_id
    input.colorg2 <- input$mutntreat_colorg2_id
    cat("Interaction - 选择的样本分组1颜色: ", input.colorg1, "\n")
    cat("Interaction - 选择的样本分组2颜色: ", input.colorg2, "\n")
    input.color <- c(input.colorg2,input.colorg1)
    
    # 多个基因时，对组合进行分析
    if (length(input.symbols) > 1) {
      input.symbols <- c(input.symbols,"All Queries Genes")
    }
    
    # 输出页面布局
    output$mutntreat_maintabs <- renderUI({
      tabs <- lapply(input.symbols, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,"_mutntreat"))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    
    # 遍历输入的每个基因，分析突变与野生型人群在不同治疗方式下的p interaction
    input.symbols.num <- length(input.symbols)
    lapply(seq(1,input.symbols.num), function(i){
      
      gname <- input.symbols[i]
      current.symbol <- gname
      if(gname == 'All Queries Genes') {current.symbol <- input.symbols}
      
      # 进度条过程
      update_modal_progress(
        value = i / input.symbols.num,
        text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/input.symbols.num*100)) 
      )
      
      # 取出时间字段,只分析有终点的
      survival.colnames <- colnames(cm025.cli)[grepl("months|status",colnames(cm025.cli))]
      # outcome.type <- study.df[which(study.df$study_id == 'Checkmate025'),]$outcome
      # outcome.type <- unlist(str_split(outcome.type,"#"))
      # survival.colnames <- c(paste0(outcome.type,"_months"),paste0(outcome.type,"_status"))
      
      # 临床样本与突变样本取交集
      tumor.samples <- intersect(cm025.cli$sample_id,cm025.mut$sample_id)
      mut.df <- cm025.mut %>%
        dplyr::filter(hugo_symbol %in% current.symbol,
                      variant_classification %in% input.vartype) %>%
        dplyr::select(sample_id,hugo_symbol)
      clc.cli <- cm025.cli %>%
        dplyr::filter(sample_id %in% tumor.samples) %>%
        dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(mut.df$sample_id),'Mut','Wt'),
                                     levels = c('Wt','Mut'))) %>%
        dplyr::select(sample_id,treatment,ici_treatment,risk = Group,all_of(survival.colnames))
      
      # 获取输出的临床终点
      outcome.df <- study.df %>%
        # dplyr::filter(study_type %in% c("ICIs")) %>%
        # dplyr::filter(tumor_detail %in% c("KIRC")) %>%
        dplyr::filter(study_id %in% c("Checkmate025"))
      cli.tab.names <- toupper(unlist(strsplit(outcome.df$outcome[1],split = "#",fixed = F)))
      cli.tab.names <- cli.tab.names[which(cli.tab.names != '')]
      
      # 根据终点存储结果
      output[[paste0(gname,"_mutntreat")]] <- renderUI({
        tabss <- lapply(cli.tab.names, function(cli.name) {
          tabPanel(
            style = "padding-top:15px",
            title = paste0("",toupper(cli.name)), status = "primary", solidHeader = TRUE, collapsible = TRUE,
            shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_mutntreat_",tolower(cli.name)),
                                                    height = "350px", width = "100%")),
            h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queries Genes: mutation of at least one of query genes.", "</span>"))),
            
            downloadButton(paste0(gname, "_dl_plot_mutntreat_",tolower(cli.name)), 
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "question-circle")),
                                        message = "Save plot in a PDF file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            downloadButton(paste0(gname, "_dl_tbl_mutntreat_",tolower(cli.name)), 
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "question-circle")),
                                        message = "Save data of survival plot in a txt file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60;border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            br(),
            DT::dataTableOutput(paste0(gname, "_tbl_mutntreat_",tolower(cli.name)))
          )
        })
        do.call(tabsetPanel, tabss)
      })
      
      # 根据终点计算结果
      lapply(cli.tab.names, function(cli.name){
        
        # 选取当前临床终点的信息
        km.df <- clc.cli %>%
          dplyr::mutate(row_id = seq(1,nrow(clc.cli))) %>%
          dplyr::select(row_id,sample_id,
                        treatment,
                        ici_treatment,
                        times = paste0(tolower(cli.name),"_months"),
                        status = paste0(tolower(cli.name),"_status"),
                        risk)%>%
          dplyr::mutate(times = round(as.numeric(times),2)) %>%
          dplyr::filter(!is.na(times),status != '',!is.na(status))
        
        # 存储表格
        output.graphic.interaction[[paste0(gname,"_",tolower(cli.name))]]$tbl <- km.df
        
        km.graphic.list <- list()
        length(km.graphic.list) <- 2
        names(km.graphic.list) <- c('icis', 'chem')
        if(length(unique(km.df$risk)) == 1){
          cat("基因 ",gname," 分组只有一组\n")
          p.inte <- NA
          for(i in c(1,2)){
            if(names(km.graphic.list[i]) == 'icis') ici_treatment_value <- 'Yes'
            if(names(km.graphic.list[i]) == 'chem') ici_treatment_value <- 'No'
            km.graphic.list[i] <- km.df %>%
              dplyr::filter(ici_treatment == ici_treatment_value) %>%
              ggsurvplot(survfit(Surv(times, status) ~ risk, data = .), 
                         data = .,
                         title = paste0("Patients with ",unique(.$treatment)[1]),
                         risk.table = T,
                         risk.table.pos = "out",
                         risk.table.y.text.col = T,
                         risk.table.y.text = TRUE,
                         ncensor.plot = F,
                         xlim = c(0, max(.$times)*1.1),
                         legend = "none",  # 取消图例，下面legend参数无效
                         legend.title = '',
                         color.dis = input.color[i],
                         break.time.by = time_break_survival(max(.$times,na.rm = T)))
          }
          
        }else{
          # interaction analyses
          km.cox <- coxph(Surv(times, status)~risk*ici_treatment, data = km.df)
          km.sum <- summary(km.cox)
          km.coef <- as.data.frame(km.sum$coefficients)
          p.inte <- signif(km.coef$`Pr(>|z|)`[3], digits=4)
          
          for(i in c(1,2)){
            # cat("current i:",i,"\n")
            if(names(km.graphic.list[i]) == 'icis') ici_treatment_value <- 'Yes'
            if(names(km.graphic.list[i]) == 'chem') ici_treatment_value <- 'No'
            temp.df <- km.df %>%
              dplyr::filter(ici_treatment == ici_treatment_value)
            if(length(unique(temp.df$risk)) == 1){
              km.graphic.list[i] <- temp.df %>%
                ggsurvplot(survfit(Surv(times, status) ~ risk, data = .), 
                           data = .,
                           title = paste0("Patients with ",unique(.$treatment)[1]),
                           risk.table = T,
                           risk.table.pos = "out",
                           risk.table.y.text.col = T,
                           risk.table.y.text = TRUE,
                           ncensor.plot = F,
                           xlim = c(0, max(.$times)*1.1),
                           legend = "none",  # 取消图例，下面legend参数无效
                           legend.title = '',
                           color.dis = input.color[i],
                           break.time.by = time_break_survival(max(.$times,na.rm = T)))
            }else{
              km.graphic.list[i] <- temp.df %>%
                func.survivalplot(paste0("Patients with ",unique(.$treatment)[1]),
                                  cli.name,
                                  time.break = time_break_survival(max(.$times,na.rm = T)),
                                  tbl.loc = 'topright',
                                  color.dis = input.color,
                                  y.title = case_when(cli.name == 'PFS' ~ 'Progression-free Survival',
                                                      cli.name == 'DFS' ~ 'Disease-free Survival',
                                                      cli.name == 'PFI' ~ 'Progression-free Interval',
                                                      cli.name == 'DFI' ~ 'Disease-free Interal',
                                                      cli.name == 'DSS' ~ 'Disease-specific Survival',
                                                      cli.name == 'OS' ~ 'Overall Survival',
                                                      TRUE ~ 'Survival'))
            }
          }
        }
        
        # 图左右排布输出
        km.graphic <- ggpubr::ggarrange(km.graphic.list[[1]],
                                        km.graphic.list[[2]],
                                        ncol = 2,
                                        widths = c(1,1))
        km.graphic <- annotate_figure(km.graphic, 
                                      bottom = text_grob("Mut = mutation,Wt = wild type", 
                                                      color = "black",  size = 12),
                                      fig.lab.pos = "bottom.left")
        km.graphic <- annotate_figure(km.graphic, 
                                      top = text_grob(paste0("P interaction: ",p.inte), 
                                                      color = "red", face = "bold", size = 14),
                                      fig.lab.pos = "top")
        
        # 存储图像
        output.graphic.interaction[[paste0(gname,"_",tolower(cli.name))]]$km <- km.graphic
        
        # 输出对象
        output[[paste0(gname, "_plot_mutntreat_",tolower(cli.name))]] <- renderPlot({
          km.graphic
        })
        output[[paste0(gname, "_tbl_mutntreat_",tolower(cli.name))]] <- renderDataTable({
          datatable(km.df %>%
                      dplyr::select(-row_id,-ici_treatment), 
                    selection = list(mode = "single", target = "row"),
                    style = 'bootstrap', 
                    rownames = FALSE, 
                    options = list(orderClasses = TRUE,dom = 'tp', language = list(
                      zeroRecords = "No records found matching your selection"),
                      columnDefs = list(list(className = 'dt-center', targets = '_all'))) ,
                    colnames = c("Sample ID","Treatment", cli.name, paste0(cli.name,".event"), gname)
                    )
        })
        output[[paste0(gname, "_dl_plot_mutntreat_",tolower(cli.name))]] <- downloadHandler(
          filename = function() {
            if(grepl("#",gname)) gname <- "Comb"
            paste0(prefix_output_file,"_Interaction_Plot_",toupper(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
          },
          content = function(file) {
            pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
            print(output.graphic[[paste0(gname,"_",tolower(cli.name))]]$km)
            dev.off()
          }
        )
        output[[paste0(gname, "_dl_tbl_mutntreat_",tolower(cli.name))]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_Interaction_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
          },
          content = function(file) {
            readr::write_delim(x = output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl, path = file, delim = "\t")
          })
        
      })
    })
    
    # 遍历结束，删除进度条
    remove_modal_progress()
    
    
    
  })
}

