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

# Interaction分析使用的数据，预先加载
{
  
  # tbl.name <- 'Checkmate025'
  # cm025.cli <- queryDataFromMySQL(paste0(tbl.name,"_clinical"))
  # cm025.mut <- queryDataFromMySQL(paste0(tbl.name,"_mutation"))
  
  cohort.names <- c('Checkmate025','Gandara_2018')
  tumors.df <- study.df %>%
    dplyr::filter(study_id %in% cohort.names) %>%
    dplyr::distinct(study_id,tumor_detail) 
  
  inact.cohort.list <- list()
  length(inact.cohort.list) <- length(unique(tumors.df$study_id))
  names(inact.cohort.list) <- unique(tumors.df$study_id)
  
  for(tbl.name in names(inact.cohort.list)){
    
    tbl.cli <- queryDataFromMySQL(paste0(tbl.name,"_clinical"))
    tbl.mut <- queryDataFromMySQL(paste0(tbl.name,"_mutation"))
    
    tbl.tumors <- tumors.df %>%
      dplyr::filter(study_id == tbl.name) %>%
      dplyr::distinct(tumor_detail,.keep_all = T) %>%
      .$tumor_detail
    
    inner.out.list <- list()
    length(inner.out.list) <- length(tbl.tumors)
    names(inner.out.list) <- tbl.tumors
    
    for(var.tumor in tbl.tumors){
      tumor.cli <- tbl.cli %>%
        dplyr::filter(cancer_detail == var.tumor) %>%
        dplyr::filter(sample_type == "T")
      tumor.mut <- tbl.mut %>%
        dplyr::filter(sample_id %in% tumor.cli$sample_id)
      tumor.sym <- unique(tumor.mut$hugo_symbol)
      tumor.var_type <- unique(tumor.mut$variant_classification)
      inner.out.list[[var.tumor]] <- list("clinical" = tumor.cli,
                                          "mutation" = tumor.mut,
                                          "symbols" = tumor.sym,
                                          "var_type" = tumor.var_type)
      
    }
    
    inact.cohort.list[[tbl.name]] <- inner.out.list
  }
  
}

tab_01_mut_treatment <- list()
cat_prefix_mutntreat <- 'Interaction'
# UI
sidebar <- sidebarPanel(
  id = 'mutntreat_sidebar',
  width = 3,
  h3("Interaction Analysis"),
  
  pickerInput(inputId = "mutntreat_study_id",
              label = "Select one cohort, E.g. 'Gandara_2018'",
              choices = NULL,
              multiple = FALSE,
              selected = NULL),
  pickerInput(inputId = "mutntreat_cancer_id",
              label = "Select one cancer type, E.g. 'LUAD'",
              choices = NULL,
              multiple = FALSE,
              selected = NULL),
  
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
                add_prompt(tags$span(icon(name = "circle-question")),
                           message = "Variant Classification for Mutation data", 
                           position = "right"),
                "Select gene mutation types"),
              choices = NULL,
              multiple = TRUE,
              selected = NULL),
  awesomeRadio(
    inputId = "mutntreat_logical",
    label = "Mutations in all(AND)/any(OR) quried genes",
    inline = T,
    choices = c("AND" ,"OR"),
    selected = "AND",
    status = "success",
    checkbox = TRUE
  ),
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "mutntreat_colorg1_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for Mut group", 
                                                    position = "right"),'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(6,colourpicker::colourInput(inputId = "mutntreat_colorg2_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for WT group", 
                                                    position = "right"),'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))
  ),
  # 提交按钮
  useShinyjs(),
  # actionButton(inputId = "mutntreat_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "mutntreat_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_mutntreat", "Clear",class="btn-warning"), style="float:left"),
           
    )
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
  
  cat("========================= Start Interaction =========================\n")
  
  # 定义变量存储输出对象
  output.graphic.interaction <- reactiveValues()
  
  observeEvent(input[['reset_input_mutntreat']], {
    shinyjs::reset("mutntreat_sidebar")
  })
  observeEvent(input$reset_input_mutntreat, {
    output[['mutntreat_maintabs']] <- NULL
  })
  
  
  # 将获取到的队列名称更新到页面
  observe({
    updatePickerInput(session = session, 
                      inputId = "mutntreat_study_id",
                      choices = c(names(inact.cohort.list)), 
                      selected = names(inact.cohort.list)[1])
  })
  # 将获取到的肿瘤名称更新到页面
  # 根据选择的队列，确定肿瘤名称
  mutntreat.study.list <- eventReactive(input$mutntreat_study_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    select_study_id <- input$mutntreat_study_id
    cat(cat_prefix_mutntreat,"-选择的队列是:",select_study_id,"\n")
    output.list <- inact.cohort.list[select_study_id][[1]]
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  observe({
    updatePickerInput(session = session, 
                      inputId = "mutntreat_cancer_id",
                      choices = c(names(mutntreat.study.list())), 
                      selected = names(mutntreat.study.list())[1])
  })
  # 根据队列，根据symbol和vartype
  mutntreat.data.list <- eventReactive(input$mutntreat_cancer_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    # browser()
    select_cancer_id <- input$mutntreat_cancer_id
    cat(cat_prefix_mutntreat,"-选择的瘤种是:",select_cancer_id,"\n")
    output.list <- mutntreat.study.list()[select_cancer_id][[1]]
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  observe({
    # browser()
    updateSelectizeInput(session = session,
                         inputId = "mutntreat_symbol_id",
                         choices = unique(mutntreat.data.list()$symbols),
                         selected = NULL,
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, 
                      inputId = "mutntreat_vartype_id",
                      choices = unique(mutntreat.data.list()$var_type), 
                      selected = unique(mutntreat.data.list()$var_type),
                      options = list(`actions-box` = TRUE)) 
  })
  
  observe({
    shinyjs::toggleState("mutntreat_goButton", 
                         !is.null(input$mutntreat_symbol_id) && input$mutntreat_symbol_id != "")
  })
  
  # 输入为空验证
  iv_mutntreat <- InputValidator$new()
  iv_mutntreat$add_rule("mutntreat_symbol_id", sv_required())
  iv_mutntreat$add_rule("mutntreat_vartype_id", sv_required())
  iv_mutntreat$enable()
  
  # interaction分析
  observeEvent(input$mutntreat_goButton,{
    
    # 进度条
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting interaction analysis ..."
    )
    
    # 参数值
    input.cohort <- input$mutntreat_study_id
    cat(paste0(cat_prefix_mutntreat," - 选择的队列名称: ", input.cohort, "\n"))
    input.cancer <- input$mutntreat_cancer_id
    cat(paste0(cat_prefix_mutntreat,"- 选择的肿瘤名称: ", input.cancer, "\n"))
    input.symbols <- input$mutntreat_symbol_id
    cat(paste0(cat_prefix_mutntreat,"- 选择的基因名称: ", input.symbols, "\n"))
    input.vartype <- input$mutntreat_vartype_id
    cat(paste0(cat_prefix_mutntreat,"- 选择的基因突变类型: ", input.vartype, "\n"))
    input.colorg1 <- input$mutntreat_colorg1_id
    input.colorg2 <- input$mutntreat_colorg2_id
    cat(paste0(cat_prefix_mutntreat,"- 选择的样本分组1颜色: ", input.colorg1, "\n"))
    cat(paste0(cat_prefix_mutntreat,"- 选择的样本分组2颜色: ", input.colorg2, "\n"))
    input.color <- c(input.colorg2,input.colorg1)
    
    mutntreat_logical_type <- input$mutntreat_logical
    cat(cat_prefix_mutntreat,"-选择的多基因突变逻辑关系: ", mutntreat_logical_type, "\n")
    
    mutntreat.cli <- mutntreat.data.list()$clinical
    mutntreat.mut <- mutntreat.data.list()$mutation
    
    # 多个基因时，对组合进行分析
    if (length(input.symbols) > 1) {
      input.symbols <- c(input.symbols,"All Queried Genes")
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
      if(gname == 'All Queried Genes') {current.symbol <- setdiff(input.symbols,gname)}
      
      # 进度条过程
      update_modal_progress(
        value = i / input.symbols.num,
        text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/input.symbols.num*100)) 
      )
      
      # 取出时间字段,只分析有终点的
      survival.colnames <- colnames(mutntreat.cli)[grepl("months|status",colnames(mutntreat.cli))]
      
      # 临床样本与突变样本取交集
      tumor.samples <- intersect(mutntreat.cli$sample_id,mutntreat.mut$sample_id)
      
      if(gname == 'All Queried Genes'){
        # 多个基因时
        if(mutntreat_logical_type == 'AND'){
          # logical 为 AND 时
          cat(cat_prefix_mutntreat,"- 逻辑与选择多基因突变模式！\n")
          tmp.mut.samples <- mutntreat.mut %>%
            dplyr::filter(hugo_symbol %in% current.symbol,
                          variant_classification %in% input.vartype)  %>%
            dplyr::select(sample_id,hugo_symbol) %>%
            dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
            dplyr::group_by(sample_id) %>%
            dplyr::summarise(sample_count = n()) %>%
            dplyr::ungroup() %>%
            dplyr::filter(sample_count == length(current.symbol)) # 取
        }
        if(mutntreat_logical_type == 'OR'){
          # logical 为 OR 时
          cat(cat_prefix_mutntreat,"- 逻辑或选择多基因突变模式！\n")
          tmp.mut.samples <- mutntreat.mut %>%
            dplyr::filter(hugo_symbol %in% current.symbol,
                          variant_classification %in% input.vartype) %>%
            dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
            dplyr::group_by(sample_id) %>%
            dplyr::summarise(sample_count = n()) %>%
            dplyr::ungroup() %>%
            dplyr::filter(sample_count >= 1) # 取
        }
        mut.df <- mutntreat.mut %>%
          dplyr::filter(hugo_symbol %in% current.symbol,
                        variant_classification %in% input.vartype) %>%
          dplyr::filter(sample_id %in% tmp.mut.samples$sample_id) %>%
          dplyr::select(sample_id,hugo_symbol)
      }else{
        # 单个基因时
        mut.df <- mutntreat.mut %>%
          dplyr::filter(hugo_symbol %in% current.symbol,
                        variant_classification %in% input.vartype) %>%
          dplyr::select(sample_id,hugo_symbol)
      }
      
      clc.cli <- mutntreat.cli %>%
        dplyr::filter(sample_id %in% tumor.samples) %>%
        dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(mut.df$sample_id),'Mut','WT'),
                                     levels = c('WT','Mut'))) %>%
        dplyr::select(sample_id,ici_treatment,ici_treatment_drug,risk = Group,all_of(survival.colnames))
      
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
            h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queried Genes: mutation of at least one of query genes.", "</span>"))),
            
            downloadButton(paste0(gname, "_dl_plot_mutntreat_",tolower(cli.name)), 
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save plot in a PDF file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            downloadButton(paste0(gname, "_dl_tbl_mutntreat_",tolower(cli.name)), 
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "circle-question")),
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
                        ici_treatment,
                        ici_treatment_drug,
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
                         title = paste0("Patients with ",unique(.$ici_treatment_drug)[1]),
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
          # p.inte <- signif(km.coef$`Pr(>|z|)`[3], digits=4)
          p.inte <- round(km.coef$`Pr(>|z|)`[3], 3)
          
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
                           title = paste0("Patients with ",unique(.$ici_treatment_drug)[1]),
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
                func.survivalplot(paste0("Patients with ",unique(.$ici_treatment_drug)[1]),
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
                                      bottom = text_grob("Mut = mutation,WT = wild type", 
                                                      color = "black",  size = 12),
                                      fig.lab.pos = "bottom.left")
        km.graphic <- annotate_figure(km.graphic, 
                                      top = text_grob(paste0("P for interaction ", ifelse(p.inte  < 0.001,
                                                                                          "< 0.001",
                                                                                          paste0("= ",p.inte))), 
                                                      color = "#00A651", face = "bold", size = 14),
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
            pdf(file,onefile = F, width = 10, height = 4, pointsize = 10)
            print(output.graphic.interaction[[paste0(gname,"_",tolower(cli.name))]]$km)
            dev.off()
          }
        )
        output[[paste0(gname, "_dl_tbl_mutntreat_",tolower(cli.name))]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_Interaction_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
          },
          content = function(file) {
            readr::write_delim(x = output.graphic.interaction[[paste0(gname,"_",tolower(cli.name))]]$tbl, path = file, delim = "\t")
          })
        
      })
    })
    
    # 遍历结束，删除进度条
    remove_modal_progress()
    
    
    
  })
}

