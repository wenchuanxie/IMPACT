#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-09-05
# @IDE     : RStudio
# @Desc    : 对于免疫治疗队列，分析治疗相应与临床、基因突变、转录表达的相关性
#===============================================================================

# Response 数据预载入
{
  
  response.data.temp <- study.df %>%
    dplyr::filter(study_type == 'ICIs') %>%
    dplyr::filter(grepl("orr|clinical_benefit",clinical_property))
  
  # 查询出队列
  if(F){
    study.names <- unique(response.data.temp$study_id)
    response.ici.list <- list()
    length(response.ici.list) <- length(study.names)
    names(response.ici.list) <- unique(study.names)
    for(i in c(1:length(study.names))){
      tbl.name <- study.names[i]
      cat("current study: ",tbl.name,"\n")
      # tbl.name <- 'Allen_2015'
      tbl.temp <- response.data.temp %>%
        dplyr::filter(study_id == tbl.name)
      # tbl.temp可能有多条记录（一个队列，多个瘤种）
      data.types <- unique(tbl.temp$data_type[1])
      cancer.names <- unique(tbl.temp$tumor_detail)
      # 队列具有的治疗响应变量
      tumor.resp <- intersect(c('orr','clinical_benefit'),unlist(str_split(tbl.temp$clinical_property[1],"#")))
      # 队列的数据类型
      cohort.data_type <- 'Clinical characteristics'
      
      tbl.cli <- queryDataFromMySQL(paste0(tbl.name,"_clinical"))
      tbl.mut <- data.frame()
      if(grepl("Mutation",data.types)){
        tbl.mut <- queryDataFromMySQL(paste0(tbl.name,"_mutation"))
        cohort.data_type <- c(cohort.data_type,'Mutation')
      }
      tbl.exp <- data.frame()
      if(grepl("Expr",data.types)){
        tbl.exp <- queryDataFromMySQL(paste0(tbl.name,"_expr"))
        cohort.data_type <- c(cohort.data_type,'Expr')
      }
      
      response.ici.list[[tbl.name]] <- list("clinical" = tbl.cli,
                                            "mutation" = tbl.mut,
                                            "expr" = tbl.exp)
    }
  }
  
  # response.data.list
  {
    study.names <- unique(response.data.temp$study_id)
    # 遍历队列
    response.data.list <- list()
    length(response.data.list) <- length(study.names)
    names(response.data.list) <- unique(study.names)
    
    for(i in c(1:length(study.names))){
      # i = 1
      # tbl.name <- 'LUAD'
      tbl.name <- study.names[i]
      cat("current study: ",tbl.name,"\n")
      
      tbl.temp <- response.data.temp %>%
        dplyr::filter(study_id == tbl.name)
      # data.types <- unlist(str_split(unique(tbl.temp$data_type[1]),"/"))
      data.types <- unique(tbl.temp$data_type[1])
      # 队列的数据类型
      cohort.data_type <- 'Clinicopathologicals'
      # 查询DB
      tbl.cli <- queryDataFromMySQL(paste0(tolower(tbl.name),"_clinical"))
      tbl.mut <- data.frame()
      if(grepl("Mutation",data.types)){
        tbl.mut <- queryDataFromMySQL(paste0(tolower(tbl.name),"_mutation"))
        cohort.data_type <- c(cohort.data_type,'Mutation')
      }
      tbl.exp <- data.frame()
      if(grepl("Expr",data.types)){
        tbl.exp <- queryDataFromMySQL(paste0(tolower(tbl.name),"_expr"))
        cohort.data_type <- c(cohort.data_type,'Expr')
      }
      tbl.pro <- data.frame()
      if(grepl("Proteome",data.types)){
        tbl.pro <- queryDataFromMySQL(paste0(tolower(tbl.name),"_proteome"))
        cohort.data_type <- c(cohort.data_type,'Proteome')
      }
      
      cancer.names <- unique(tbl.temp$tumor_detail)
      # 遍历肿瘤
      response.cancer.list <- list()
      length(response.cancer.list) <- length(cancer.names)
      names(response.cancer.list)  <- paste0(cancer.names)
      
      for(var.tumor in cancer.names){
        
        # var.tumor <- 'Melanoma'
        cat("current tumor: ",var.tumor,"\n")
        
        tumor.temp <- tbl.temp %>%
          dplyr::filter(tumor_detail == var.tumor)
        # 队列具有的治疗响应变量
        resp.types <- intersect(c('orr','clinical_benefit'),unlist(str_split(tumor.temp$clinical_property[1],"#")))
        response_biopsys <- unlist(str_split(tumor.temp$ici_study_biopsy[1],"#"))
        response_ici_types <- unlist(str_split(tumor.temp$ici_type[1],","))
        
        tumor.cli <- tbl.cli %>%
          dplyr::filter(cancer_detail == var.tumor) %>%
          dplyr::filter(sample_type == "T")%>%
          dplyr::filter(ici_treatment == "Yes")
        
        tumor.mut <- data.frame()
        if(nrow(tbl.mut) > 0){
          tumor.mut <- tbl.mut %>%
            dplyr::filter(sample_id %in% tumor.cli$sample_id)
          # tumor.var_type <- unique(tumor.mut$variant_classification)
        }
        
        tumor.exp <- data.frame()
        if(nrow(tbl.exp) > 0){
          tumor.exp <- tbl.exp %>%
            dplyr::select(hugo_symbol,intersect(tumor.cli$sample_id,colnames(tbl.exp)))
        }
        
        response.cancer.list[[var.tumor]] <- list("clinical" = tumor.cli,
                                                  "mutation" = tumor.mut,
                                                  "expr" = tumor.exp,
                                                  "response_vars" = resp.types,
                                                  "cohort_data_type" = cohort.data_type,
                                                  "biopsys" = response_biopsys,
                                                  "ici_types" = response_ici_types)
        
      }
      
      response.data.list[[tbl.name]] <- response.cancer.list
    }
  }
  
  # 遍历数据，将response.data.list的cohort - data 改为tumor - cohort - data 结构
  cancer.names <- unique(response.data.temp$tumor_detail)
  cancer.response.list <- list()
  for(i in c(1:length(cancer.names))){
    # i = 1
    cancer.var <- cancer.names[i]
    # cancer.var <- 'LUAD'
    study.vars.df <- response.data.temp %>%
      dplyr::filter(tumor_detail == cancer.var) %>%
      dplyr::distinct(study_id,.keep_all = T)
    study.vars <- study.vars.df$study_id
    
    cancer.study.list <- list()
    for(j in c(1:length(study.vars))){
      # j = 1
      study.var <- study.vars[j]
      # study.var <- 'Hellmann_2018'
      cancer.data.list <- response.data.list[[study.var]][[cancer.var]]
      
      cancer.study.list[[study.var]] <- cancer.data.list
    }
    cancer.response.list[[cancer.var]] <- cancer.study.list
  }
  
  cat("Response 数据加载完成!\n")
}

source("./r/func.stackplot.R")
tab_02_response <- list()
cat_prefix_reponse <- "Response"
sidebar <- sidebarPanel(
  id = 'response_sidebar',
  width = 3,
  h3("Immunotherapy response"),
  useShinyjs(),
  div(id = "response_form",
      
  #  队列
  pickerInput(inputId = "response_cancer_id",
              label = "Select one cancer type, E.g. 'LUAD'",
              choices = NULL,
              multiple = FALSE,
              selected = NULL),
  # 肿瘤
  conditionalPanel('input.response_cancer_id != ""',
                   pickerInput(inputId = "response_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  # tags$hr(style="border-color: purple;"),
  # 分组条件：ORR or Clinical benefit
  # pickerInput(inputId = "response_group_id",
  #             label = "Select response variable, E.g. 'ORR'",
  #             choices = NULL,
  #             multiple = FALSE,
  #             selected = NULL),
  tags$hr(style="border-color: purple;"),
  # h3("Gene parameters", style = "margin-bottom: 25px;color:#00a064"),
  awesomeRadio(inputId = "response_data_type",
               label = "Select data type", 
               choices = '',
               selected = NULL,
               status = "success"),
  conditionalPanel('input.response_data_type == "Clinicopathologicals"',
                   # 用于亚组分析的临床属性
                   pickerInput(inputId = "response_clinical_id",
                               label = "Select one or multiple clinical characteristics",
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)),
  conditionalPanel('input.response_data_type == "Expr"| input.response_data_type == "Mutation"',
                   # 用于分析的基因
                   selectizeInput(inputId = "response_symbol_id", 
                                  label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,placeholder = "No more then 10 genes!",
                                                 plugins = list('remove_button', 'drag_drop')))),
  
  conditionalPanel('input.response_data_type == "Mutation"',
                   # 队列数据的基因突变类型
                   pickerInput(inputId = "response_vartype_id",
                               label = tags$span(
                                  add_prompt(tags$span(icon(name = "circle-question")),
                                             message = "Variant Classification for Mutation data", 
                                             position = "right"),
                                 "Select gene mutation types"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)),
  
  tags$hr(style="border-color: purple;"),
  # h3("Sample parameters", style = "margin-bottom: 25px;color:#00a064"),
  # ici_treatment_status 免疫治疗取样时间点参数控制
  pickerInput(inputId = "response_biopsy_status",
              label = tags$span(
                add_prompt(tags$span(icon(name = "circle-question")),
                           message = "Biopsy time of tumor sample", 
                           position = "right"),
                "Biopsy time (Pre-,On- or Post-treatment):"),
              choices = NULL,
              multiple = TRUE,
              selected = NULL),
  # 免疫治疗药物类型选择
  # tags$head(tags$script(HTML(js_icitype))),
  pickerInput(inputId = "response_ici_type",
              label = tags$span(
                add_prompt(tags$span(icon(name = "circle-question")),
                           message = "Immune checkpoint inhibitors types", 
                           position = "right"),
                "ICIs type:"),
              choices = NULL,
              multiple = TRUE,
              selected = NULL),
  # tags$hr(style="border-color: purple;"),
  # h3("Graphic parameters", style = "margin-bottom: 25px;color:#00a064"),
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "response_colorg1_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Boxplot color for responder (PR/CR, DCB) group", 
                                                     position = "right"),
                                         'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(1),
    column(5,colourpicker::colourInput(inputId = "response_colorg2_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Boxplot color for non responder (PD/SD, NDB) group", 
                                                     position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))
    )),
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "response_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_response", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
)

mainpage <- mainPanel(
  useShinyjs(),
  id = 'response_mainpage',
  width = 9,
  uiOutput(outputId='response_maintabs')
)

tab_02_response$ui <- sidebarLayout(sidebar, mainpage)

tab_02_response$server <- function(input, output,session) {
  cat("========================= Start Response ============================\n")
  
  # track_usage(storage_mode = store_json(path = "logs/"),what = 'error')
  
  # 定义变量存储输出对象
  output.graphic.response <- reactiveValues()
  
  # 一键清空
  observeEvent(input[['reset_input_response']], {
    shinyjs::reset("response_sidebar")
  })
  observeEvent(input$reset_input_response, {
    output[['response_maintabs']] <- NULL
  })
  
  # 将获取到的肿瘤名称更新到页面
  observe({
    choices.vars <- sort(names(cancer.response.list))
    updatePickerInput(session = session, 
                      inputId = "response_cancer_id",
                      choices = choices.vars, 
                      selected = choices.vars[1])
  })
  # 根据选择的肿瘤，确定队列名称
  reponse.cancer.list <- eventReactive(input$response_cancer_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    select_cancer_id <- input$response_cancer_id
    cat(cat_prefix_reponse,"-选择的肿瘤是:",select_cancer_id,"\n")
    # output.dat <- response.data.temp %>%
    #   dplyr::filter(tumor_detail == select_cancer_id)
    output.dat <- cancer.response.list[[select_cancer_id]]
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.dat)
  })
  observe({
    choices.vars <- sort(names(reponse.cancer.list()))
    updatePickerInput(session = session, 
                      inputId = "response_study_id",
                      choices = c(choices.vars), 
                      selected = choices.vars[1])
  })
  
  # 根据选择的队列，确定队列的数据类型
  reponse.study.list <- eventReactive(input$response_study_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    select_study_id <- input$response_study_id
    cat(cat_prefix_reponse,"-选择的队列是:",select_study_id,"\n")
    # output.dat <- reponse.cancer.dat() %>%
    #   dplyr::filter(study_id == select_study_id)
    output.dat <- reponse.cancer.list()[[select_study_id]]
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.dat)
  })
  
  # observe({
  #   response.clidx <- reponse.study.list()[['response_vars']]
  #   response.clidx <- toupper(response.clidx)
  #   if("CLINICAL_BENEFIT" %in% response.clidx) response.clidx <- gsub("CLINICAL_BENEFIT","Clinical_Benefit",response.clidx)
  #   updatePickerInput(session = session, 
  #                     inputId = "response_group_id",
  #                     choices = c(response.clidx), 
  #                     selected = response.clidx[1])
  # })
  
  observe({
    response.datatype <- reponse.study.list()[['cohort_data_type']]
    updateAwesomeRadio(
      session = session, 
      inputId = "response_data_type",
      choices = c(response.datatype),
      selected = response.datatype[1],
    )
  })
  
  response.clinicalfactors.list <- eventReactive(input$response_study_id,{
    response_clinicalfactors_study_id <- input$response_study_id
    temp.df <- response.data.temp %>%
      dplyr::filter(study_id == response_clinicalfactors_study_id) %>%
      dplyr::distinct(study_id,.keep_all = T)
    
    # 根据队列名称确定待分析临床指标（事先存储于总表）
    temp.imgs <- unlist(strsplit(temp.df$clinical_property,split = "#",fixed = F))
    # 在study_info中，PD-L1标识为pdl1，在每个队列中，pdl1_exp是连续性变量，pdl1是分组变量
    # if('pdl1' %in% temp.imgs){ 
    #   temp.imgs <- gsub("pdl1",'pdl1_exp',temp.imgs)
    # }
    temp.imgs <- setdiff(temp.imgs,c('orr','clinical_benefit'))
    temp.imgs <- toupper(temp.imgs)
    return(temp.imgs)
  })
  observe({
    # 更新clincal属性
    updatePickerInput(session = session, inputId = "response_clinical_id",
                      choices = response.clinicalfactors.list(), 
                      selected = response.clinicalfactors.list(),
                      options = list(
                        `actions-box` = TRUE))
  })
  
  # 根据队列，根据symbol和vartype
  reponse.data.dat <- eventReactive(c(input$response_data_type),{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    # browser()
    # response_select_study_id <- input$response_study_id
    response_select_data_type <- input$response_data_type
    response_select_data_type <- tolower(response_select_data_type)
    # response_select_data_type <- setdiff(response_select_data_type,"clinicopathologicals")
    cat(cat_prefix_reponse,"-前端数据类型参数:",response_select_data_type,"\n")
    output.df <- reponse.study.list()[[response_select_data_type]]
    if(response_select_data_type == 'expr'|
       response_select_data_type == 'proteome'){
      output.df <- reponse.study.list()[[response_select_data_type]]
      output.df$variant_classification <- ''
    }
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.df)
  })
  observe({
    # browser()
    updateSelectizeInput(session = session,
                         inputId = "response_symbol_id",
                         choices = unique(reponse.data.dat()$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    # 
    # # 更新突变类型
    updatePickerInput(session = session,
                      inputId = "response_vartype_id",
                      choices = unique(reponse.data.dat()$variant_classification),
                      selected = unique(reponse.data.dat()$variant_classification),
                      options = list(`actions-box` = TRUE))
  })
  
  # 根据队列，根据symbol和vartype
  reponse.biy.list2 <- eventReactive(c(input$response_study_id),{
    select_study_id <- input$response_study_id
    output.dat <- reponse.cancer.list()[[select_study_id]][['clinical']]
    
    return(output.dat)
  })
  
  observe({
    response.biopsys <- reponse.study.list()[['biopsys']]
    
    
    updatePickerInput(session = session,
                      inputId = "response_biopsy_status",
                      choices = response.biopsys,
                      selected = response.biopsys,
                      options = list(`actions-box` = TRUE))
  })
  observe({
    response.ici_types <- reponse.study.list()[['ici_types']]
    
    
    updatePickerInput(session = session,
                      inputId = "response_ici_type",
                      choices = response.ici_types,
                      selected = response.ici_types,
                      options = list(`actions-box` = TRUE))
  })
  # 输入为空验证
  iv_resp <- InputValidator$new()
  iv_resp$add_rule("response_cancer_id", sv_required())
  iv_resp$add_rule("response_study_id", sv_required())
  iv_resp$add_rule("response_data_type", sv_required())
  iv_resp$add_rule("response_data_type", sv_required())
  iv_resp$add_rule("response_clinical_id", sv_required())
  iv_resp$add_rule("response_symbol_id", sv_required())
  iv_resp$add_rule("response_vartype_id", sv_required())
  iv_resp$add_rule("response_biopsy_status", sv_required())
  iv_resp$add_rule("response_ici_type", sv_required())
  iv_resp$enable()
  
  # Submit后业务流
  observeEvent(input$response_goButton,{
    cat("===================== Server Response =============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting response analysis ..."
    )
    
    resp_selected_cancer_id = input$response_cancer_id
    cat(cat_prefix_reponse,"-选择的研究瘤肿: ", resp_selected_cancer_id, "\n")
    
    resp_selected_study_id <- input$response_study_id
    cat(cat_prefix_reponse,"-选择的研究队列: ", resp_selected_study_id, "\n")
    
    # resp_selected_groupid <- input$response_group_id
    reponse.cancer.list()[[resp_selected_study_id]]
    resp_groupid <- reponse.study.list()[['response_vars']]
    resp_groupid <- toupper(resp_groupid)
    if("CLINICAL_BENEFIT" %in% resp_groupid) resp_groupid <- gsub("CLINICAL_BENEFIT","Clinical_Benefit",resp_groupid)
    cat(cat_prefix_reponse,"-队列的治疗相应指标(X): ", resp_groupid, "\n")
    
    resp_selected_datatype <- input$response_data_type 
    cat(cat_prefix_reponse,"-选择的指标类型(Y): ", resp_selected_datatype, "\n")
    
    resp_selected_clinicals <- input$response_clinical_id
    cat(cat_prefix_reponse,"-选择的临床属性名称: ", resp_selected_clinicals, "\n")
    
    resp_selected_symbol <- input$response_symbol_id
    cat(cat_prefix_reponse,"-选择的基因名称: ", resp_selected_symbol, "\n")
    
    resp_selected_vartype <- input$response_vartype_id
    cat(cat_prefix_reponse,"-选择的突变类型: ", resp_selected_vartype, "\n")
    
    resp_selected_biopsys <- input$response_biopsy_status
    cat(cat_prefix_reponse,"-选择的采样节点: ", resp_selected_biopsys, "\n")
    
    resp_selected_icitype <- input$response_ici_type
    cat(cat_prefix_reponse,"-选择的ICI治疗药物类型: ", resp_selected_icitype, "\n")
    
    resp_selected_color1 <- input$response_colorg1_id
    resp_selected_color2 <- input$response_colorg2_id
    
    # 写出到页面
    output$response_maintabs <- renderUI({
      
      tabs <- lapply(resp_groupid, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,"_resp"))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    
    # 根据参数提取数据
    # reponse.study.list() 是一个涵盖多个数据类型的list
    raw_clinical_df.resp <- reponse.study.list()[['clinical']]
    raw_mutation_df.resp <- reponse.study.list()[['mutation']]
    raw_expr_df.resp <- reponse.study.list()[['expr']]
    # browser()
    #  对年龄分组
    if("age" %in% colnames(raw_clinical_df.resp)){
      
      
      if(max(as.numeric(raw_clinical_df.resp$age),na.rm = T) > 61){
        raw_clinical_df.resp$age <- ifelse(as.numeric(raw_clinical_df.resp$age) >= 60,">=60",
                                           ifelse(as.numeric(raw_clinical_df.resp$age) <60 ,"<60",
                                                  ''))
      }else{
        resp.age.cutpoint <- median(as.numeric(raw_clinical_df.resp$age),na.rm = T)
        raw_clinical_df.resp$age <- ifelse(as.numeric(raw_clinical_df.resp$age) >= resp.age.cutpoint,">=median",
                                           ifelse(as.numeric(raw_clinical_df.resp$age) < resp.age.cutpoint ,"<median",
                                                  ''))
      } 
    }
    
    # 根据免疫治疗取样时间和药物类型过滤
    clc_clinical_df.resp <- raw_clinical_df.resp %>%
      dplyr::filter(ici_treatment_status %in% resp_selected_biopsys,
                    str_detect(ici_treatment_type, paste0(resp_selected_icitype,collapse = "|")))
    
    # 提取Y变量数据
    if(resp_selected_datatype == 'Clinicopathologicals'){
      
      clc_y.resp <- raw_clinical_df.resp %>%
        dplyr::select(sample_id,
                      tolower(resp_selected_clinicals))
      
      resp.var.categorial <- setdiff(tolower(resp_selected_clinicals),vars.continues)
      resp.var.continues <- setdiff(tolower(resp_selected_clinicals),resp.var.categorial)
    }
    if(resp_selected_datatype == 'Mutation'){
      # browser()
      clc_df.resp <- raw_mutation_df.resp %>%
        dplyr::filter(hugo_symbol %in% resp_selected_symbol) %>%
        dplyr::filter(variant_classification %in% resp_selected_vartype) %>%
        dplyr::select(sample_id,hugo_symbol,variant_classification) %>%
        dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) 
      # long to width data
      # 聚合同sample_id下同基因的突变类型
      clc_mat.resp <- aggregate(clc_df.resp[,3],
                                by=list(gene = clc_df.resp$hugo_symbol,sample=clc_df.resp$sample_id),
                                FUN=paste0)  
      colnames(clc_mat.resp)[3] <- 'Variant_Classification'
      clc_y.resp <- clc_mat.resp %>%
        # dplyr::mutate(Variant_Classification = as.character(Variant_Classification)) %>%          # list 转 character
        # dplyr::mutate(Variant_Classification = gsub('^[c]',"",Variant_Classification)) %>%        # 消除c
        # dplyr::mutate(Variant_Classification = gsub('[(\\"\\")]',"",Variant_Classification)) %>%  # 消除括号、引号
        # dplyr::mutate(Variant_Classification = gsub(',',": ",Variant_Classification))  %>%         # 替换 ,
        dplyr::mutate(Variant_Classification = 'Mut') %>%
        tidyr::spread(key=sample,value=Variant_Classification) %>%
        tibble::column_to_rownames(var = "gene") %>%
        t() %>% as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") 
      clc_y.resp[is.na(clc_y.resp)] <- 'WT'
      
      resp.var.categorial <- colnames(clc_y.resp)[-1]
      resp.var.continues <- c()
    }
    if(resp_selected_datatype == 'Expr'){
      
      clc_y.resp <- raw_expr_df.resp %>%
        dplyr::filter(hugo_symbol %in% resp_selected_symbol) %>%
        tibble::column_to_rownames(var = 'hugo_symbol') %>%
        t() %>% as.data.frame() %>%
        tibble::rownames_to_column(var = 'sample_id')
      
      resp.var.continues <- colnames(clc_y.resp)[-1]
      resp.var.categorial <-  c()
    }
    
    cat(cat_prefix_reponse,"-分类变量: ",resp.var.categorial,"\n")
    cat(cat_prefix_reponse,"-连续变量: ",resp.var.continues,"\n")
    
    n <- length(resp_groupid)
    lapply(seq(1,n), function(i) {
      
      resp_selected_groupid <- resp_groupid[i]
      
      update_modal_progress(
        value = i / n,
        text =  paste("Analyzing response varaible:", resp_selected_groupid,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
      )
      
      # 提取X变量数据
      if(tolower(resp_selected_groupid) == 'orr'){
        # cat(cat_prefix_reponse,"-current responder variable:",tolower(resp_selected_groupid),"\n")
        # print(tolower(resp_selected_groupid))
        clc_x.resp <- clc_clinical_df.resp %>%
          dplyr::select(sample_id,
                        group = tolower(resp_selected_groupid)) %>%
          dplyr::filter(group %in% c('PD','SD','PR','CR','CRPR')) %>%
          dplyr::mutate(group_new = ifelse(group %in% c('PD','SD'),"PD/SD","PR/CR")) %>%
          dplyr::select(sample_id,group = group_new)
      }
      if(tolower(resp_selected_groupid) == 'clinical_benefit'){
        # cat(cat_prefix_reponse,"-current responder variable:",tolower(resp_selected_groupid),"\n")
        # print(tolower(resp_selected_groupid))
        clc_x.resp <- clc_clinical_df.resp %>%
          dplyr::select(sample_id,
                        group = tolower(resp_selected_groupid)) %>%
          dplyr::filter(group %in% c('DCB','NDB')) %>%
          dplyr::mutate(group  = factor(group,levels = c('NDB','DCB'))) %>%
          # dplyr::mutate(group_new = ifelse(group %in% c('DCB'),"Responder","non Responder")) %>%
          dplyr::select(sample_id,group )
      }
      
      # 合并x和y数据:sample_id,group,.....
      clc_dat.resp <- clc_x.resp %>%
        # 必须左连接，因为mutation数据为只有突变的样本
        dplyr::left_join(clc_y.resp)
      # 突变类型指标时，需在合并后新增
      if(resp_selected_datatype == 'Mutation'){
        # browser()
        clc_dat.resp[is.na(clc_dat.resp)] <- 'WT'
      }
      # 分类变量绘图
      resp.plot.list <- list()
      resp.tabl.list <- list()
      if(length(resp.var.categorial) > 0){
        
        reps.plot.df <- clc_dat.resp %>%
          dplyr::select(sample_id,group,resp.var.categorial) #%>%
        
        
        for(cli.name in resp.var.categorial){
          # browser()
          resp.df <- reps.plot.df %>%
            dplyr::select(sample_id,group,var.name = cli.name) %>%
            dplyr::filter(!is.na(var.name))
          
          # if(length(unique(resp.df$group)) == 1){
          #   cat("基因 ",group," 分组只有一组\n")
          #   resp.graphic <- ggbarstats(
          #     data = resp.df,
          #     x = var.name,
          #     y = group,
          #     type = 'nonparametric',
          #     label = 'counts',
          #     proportion.test = NULL,
          #     title = toupper(cli.name),
          #     legend.title = toupper(cli.name),
          #     xlab = '') +
          #     scale_fill_manual(values = c(sample(brewer.pal(n = 12, name = "Set3"),
                                                  # length(unique(resp.df$var.name)))))
            
          # }else{
            # browser()
            # 关于结果值的解释
            # https://yuzar-blog.netlify.app/posts/2021-12-14-how-to-conduct-chi-square-test-in-r/
            # resp.graphic <- ggbarstats(
            #   data = resp.df,
            #   x = var.name,
            #   y = group,
            #   type = 'nonparametric',
            #   label = 'counts',
            #   proportion.test = NULL,
            #   title = toupper(cli.name),
            #   legend.title = toupper(cli.name),
            #   xlab = '') +
            #   scale_fill_manual(values = c(sample(brewer.pal(n = 12, name = "Set3"),
            #                                       length(unique(resp.df$var.name)))))
            
          # }
          # browser()
          plot.xtbl <- table(resp.df$group,resp.df$var.name)
          plot.pvalue <- chisq.test(plot.xtbl)$p.value
          
          plot.df <- resp.df %>%
            dplyr::group_by(xname = group,yname = var.name) %>%
            dplyr::summarise(countvalue = n())
          
          resp.graphic <- func.stackplot(plot.df,
                                         title = toupper(cli.name),
                                         legendTitle = toupper(cli.name),
                                         p.value = plot.pvalue,
                                         pvalue.xloc = 1.3,
                                         color.sets = sample(brewer.pal(n = 12, name = "Set3"),
                                                             length(unique(resp.df$var.name))))
          
          # 调整legend的titile名称
          # resp.graphic <- resp.graphic +
          #   theme(axis.text = element_text(face = 'bold',size = 12))
          # resp.graphic <- as.ggplot(resp.graphic)
          
          resp.plot.list[[cli.name]] <- resp.graphic
          resp.tabl.list[[cli.name]] <- resp.df
        }
        
        # cat.resp.plot <- ggarrange(plotlist = resp.plot.list,
        #                            ncol = length(resp.plot.list),
        #                            nrow = 1,
        #                            # labels = names(resp.plot.list),
        #                            widths = 500*length(resp.plot.list),
        #                            heights = 400 )
        
      }
      # 连续变量绘图
      if(length(resp.var.continues) > 0){
        # print("===========> bugtraced: \n")
        # print(resp.var.continues)
        # browser()
        reps.plot.df <- clc_dat.resp %>%
          dplyr::select(sample_id,group,resp.var.continues)  
        # 绘Boxplot图
        # con.resp.plot.list <- list()
        # con.resp.tabl.list <- list()
        for(cli.name in resp.var.continues){
          resp.df <- reps.plot.df %>%
            dplyr::select(sample_id,group,img_name = cli.name)%>%
            dplyr::mutate(img_name = as.numeric(img_name))
          resp.graphic <- func.ggboxplot(resp.df,
                                         unique(as.character(resp.df$group))[1],
                                         unique(as.character(resp.df$group))[2],
                                         resp_selected_color1,
                                         resp_selected_color2,
                                         ytitle = cli.name) +
            ggtitle(label = toupper(cli.name)) +
            theme(plot.title = element_text(color="black", size=12, face="bold",hjust = 0),
                  axis.ticks = element_line(size = 2))
          # output.graphic.resp[[paste0(tolower(cli.name))]]$tbl <- resp.df
          # output.graphic.resp[[paste0(tolower(cli.name))]]$plot <- resp.graphic
          resp.plot.list[[cli.name]] <- resp.graphic
          resp.tabl.list[[cli.name]] <- resp.df
        }
        
        # con.resp.plot <- ggarrange(plotlist = con.resp.plot.list,
        #                            ncol = length(con.resp.plot.list),
        #                            nrow = 1,
        #                            # labels = names(con.resp.plot.list),
        #                            widths = 500*length(con.resp.plot.list),
        #                            heights = 400 )
        
      }
      # 整合当前图表
      {
        # browser()
        # 图
        # if(length(resp.var.categorial) == 0){
        #   out.plot <- con.resp.plot
        #   widths = 500*length(con.resp.plot.list)
        #   heights = 400
        # }
        # if(length(resp.var.continues) == 0){
        #   out.plot <- cat.resp.plot
        #   widths = 500*length(cat.resp.plot.list)
        #   heights = 400
        # }
        # if(length(resp.var.categorial) > 0 & 
        #    length(resp.var.continues) > 0){
        #   out.plot <- ggarrange(cat.resp.plot,con.resp.plot,
        #                         ncol = 2,
        #                         nrow = 1)
        #   widths = 500*(length(cat.resp.plot.list)+length(con.resp.plot.list))
        #   heights = 400
        # }
        n.col <- min(5,length(resp.plot.list))
        n.row <- max(1,ceiling(length(resp.plot.list)/n.col))
        resp.out.plot <- ggarrange(plotlist = resp.plot.list,
                                   ncol = n.col,
                                   nrow = n.row,
                                   # labels = names(resp.plot.list),
                                   widths = 500*n.col,
                                   heights = 400 * n.row)
        output.width <- 500*n.col
        output.heights <- 400 * n.row
        
        
        # 表
        tabl.names <- names(resp.tabl.list)
        resp.out.tabl <- resp.tabl.list[[tabl.names[1]]]
        if(length(tabl.names)>1){
          for (i in c(2:length(resp.tabl.list))) {
            tmp.tabl<- resp.tabl.list[[tabl.names[i]]]
            resp.out.tabl <- resp.out.tabl  %>%
              dplyr::full_join(tmp.tabl %>%
                                 dplyr::select(-group),by='sample_id') %>%
              dplyr::select(sample_id,group,everything())
          }
        }
        colnames(resp.out.tabl) <- c('Sample ID',resp_selected_groupid,toupper(tabl.names))
        output.graphic.response[[paste0(resp_selected_groupid,"_resp_tabl")]]$tbl <- resp.out.tabl
        output.graphic.response[[paste0(resp_selected_groupid,"_resp_plot")]]$respplot <- resp.out.plot
        
      }
      
      {
        
        # 图输出到页面
        output[[paste0(resp_selected_groupid,"_resp")]] <- renderUI({
          tagList(
            shinycssloaders::withSpinner(plotOutput(paste0(resp_selected_groupid, "_resp_plot"),height = 400 * n.row,width = 'auto')),
            h5(HTML(paste("<span style=color:black;font-size:12px;>", "Categorical variables were analyzed by the Pearson's chi-squared test. 
                          Continuous variables were analyzed using non-parametric Wilcoxon rank-sum test. 
                          Mut: mutation, Wt: wild type.", "</span>"))),
            downloadButton(paste0(resp_selected_groupid, "_resp_plot_dl"),
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save plot in a pdf file.",
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            downloadButton(paste0(resp_selected_groupid, "_resp_tabl_dl"),
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save data of plot in a txt file.",
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),  
            br(),
            DT::dataTableOutput(paste0(resp_selected_groupid, "_resp_tabl"))
          )
        })
        output[[paste0(resp_selected_groupid,"_resp_plot")]] <- renderPlot({
          print(resp.out.plot)
        })
        
        # 表输出到页面
        output[[paste0(resp_selected_groupid,"_resp_tabl")]] <- renderDataTable({
          datatable(resp.out.tabl,
                    style = 'bootstrap',
                    rownames = FALSE,
                    selection = list(mode = "single", target = "row"),
                    options = list(orderClasses = TRUE,keys = TRUE,dom = 'Bfrtip', searching = F,
                                   language = list(zeroRecords = "No records found matching your selection"),
                                   columnDefs = list(list(className = 'dt-center', targets = '_all'))) #,
                    # colnames = c("Sample ID",resp_selected_groupid, tabl.names)
          )
        })
        
        # 下载图
        output[[paste0(resp_selected_groupid, "_resp_plot_dl")]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_Response_Plot_",resp_selected_groupid,"_",Sys.Date(),".pdf", sep = "")
          },
          content = function(file) {
            pdf(file,onefile = F, width = (output.width/100) * 0.8, height = output.heights/100, pointsize = 10)
            print(output.graphic.response[[paste0(resp_selected_groupid,"_resp_plot")]]$respplot)
            dev.off()
          })
        # 下载表
        output[[paste0(resp_selected_groupid, "_resp_tabl_dl")]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_Response_Data_",resp_selected_groupid,"_",Sys.Date(), '.txt', sep='')
          },
          content = function(file) {
            readr::write_delim(x = output.graphic.response[[paste0(resp_selected_groupid,"_resp_tabl")]]$tbl, path = file, delim = "\t")
          })
      }
      
    })
    remove_modal_progress()
  })
}
