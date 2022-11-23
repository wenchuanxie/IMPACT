#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-19
# @IDE     : RStudio
# @Desc    : 第二个Tab功能：tumor immunogenicity: tmb,neo,genomic scar (hrd),scna et al
#===============================================================================

source("./r/func.ggboxplot.R")
source("./r/func.beeswarmplot.R")
source("./r/func.scatterbwplot.R")


tab_02_tumor_immunogenicity <- list()
cat_prefix_img <- 'Immunogenicity'
sidebar <- sidebarPanel(
  id = 'immunogenicity_sidebar',
  width = 3,
  h3("Immunogenicity Analysis"),
  # 研究类型
  awesomeRadio(inputId = "immunogenicity_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               status = "success"),
  # 肿瘤
  conditionalPanel('input.immunogenicity_study_type != ""',
                   pickerInput(inputId = "immunogenicity_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  # 肿瘤队列
  conditionalPanel('input.immunogenicity_cancer_detail != ""',
                   pickerInput(inputId = "immunogenicity_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  tags$hr(style="border-color: purple;"),
  conditionalPanel('input.immunogenicity_study_id != ""',
                   # 队列数据类型：Mutation,RNA,Protein,Methyl
                   pickerInput("immunogenicity_data_type",
                               label = "Select data type, E.g. 'Mutation'", 
                               choices = NULL,
                               selected = NULL, 
                               multiple = FALSE),
                   
                   # h3("Horizontal Axis", style = "margin-bottom: 25px;color:#00a064"),
                   # 队列数据的基因list
                   selectizeInput(inputId = "immunogenicity_symbol_id", 
                                  label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop')))),
  conditionalPanel("input.immunogenicity_data_type == 'Mutation'",
                   # 队列数据的基因突变类型
                   pickerInput(inputId = "immunogenicity_vartype_id",
                               label = tags$span(
                                  add_prompt(tags$span(icon(name = "circle-question")),
                                             message = "Variant Classification for Mutation data", 
                                             position = "right"),
                                 "Select gene mutation types"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL),
                   awesomeRadio(
                     inputId = "immunogenicity_logical",
                     label = "Mutations in all(AND)/any(OR) quried genes",
                     inline = T,
                     choices = c("AND" ,"OR"),
                     selected = "AND",
                     status = "success",
                     checkbox = TRUE
                   )),
  
  conditionalPanel(condition = "input.immunogenicity_data_type == 'Expr' || input.immunogenicity_data_type == 'Proteome'",
                   # 队列数据的基因表达阈值
                   fluidRow(
                     column(6,numericInput(inputId = "immunogenicity_exprcut1_id",
                                           label = tags$span(
                                             add_prompt(tags$span(icon(name = "circle-question")),
                                                        message = "Percentile threshold for RNA data", 
                                                        position = "right"),
                                             "Percentile cutoff (%,High)"),
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%')),
                     column(5,numericInput(inputId = "immunogenicity_exprcut2_id",
                                           label = "",
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%'))
                   )),
  tags$hr(style="border-color: purple;"),
  # h3("Vertical Axis", style = "margin-bottom: 25px;color:#00a064"),
  # 选择基因还是临床信息
  awesomeRadio(inputId = "immunogenicity_yaxis_factor",
               label = "Select molecular type:", 
               choices = '',
               selected = NULL,
               status = "success"),
  conditionalPanel('input.immunogenicity_yaxis_factor == "Clinicopathologicals"',
                   # 用于亚组分析的临床属性
                   pickerInput(inputId = "immunogenicity_clinical_id",
                               label = tags$span(
                                 add_prompt(tags$span(icon(name = "circle-question")),
                                            message = "Select clinicopathologic characteristics used to subgroup analysis!", 
                                            position = "right"),
                                 "Select one or multiple clinical characteristics"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)),
  conditionalPanel('input.immunogenicity_yaxis_factor == "mRNA"| input.immunogenicity_yaxis_factor == "Protein"',
                   # 用与亚组分析的基因
                   selectizeInput(inputId = "immunogenicity_genes_id", 
                                  label = tags$span(
                                    add_prompt(tags$span(icon(name = "circle-question")),
                                               message = "Select genes used to immunogenicity analysis!", 
                                               position = "right"),
                                    "Select/Input gene symbols"), 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,placeholder = "No more then 10 genes!",
                                                 plugins = list('remove_button', 'drag_drop')))),
  
  # signature 或 cell sets
  conditionalPanel('input.immunogenicity_yaxis_factor == "Pathways or Signatures"',
                   pickerInput("immunogenicity_signature_id",
                               label = 'Pathways or Signatures', 
                               choices = NULL,
                               selected = NULL, 
                               multiple = TRUE,
                               options = pickerOptions(
                                 `live-search` = TRUE, title = "Pick no more than 10 choices",
                                 dropdownAlignRight = T,
                                 "max-options" = 10,
                                 "max-options-text" = "No more!"
                               ))),
  tags$hr(style="border-color: purple;"),
  # h3("Graphic parameters", style = "margin-bottom: 25px;color:#00a064"),
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "immunogenicity_colorg1_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for Mut/High group", 
                                                    position = "right"),
                                         'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(6,colourpicker::colourInput(inputId = "immunogenicity_colorg2_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for WT/Low group", 
                                                    position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))
  ),
  
  # 提交按钮
  # actionButton(inputId = "immunogenicity_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "immunogenicity_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_immunogenicity", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
  
)

mainpage <- mainPanel(
  id = 'immunogenicity_mainpage',
  width = 9,
  uiOutput(outputId='immunogenicity_maintabs')
)

tab_02_tumor_immunogenicity$ui <- sidebarLayout(sidebar, mainpage)

tab_02_tumor_immunogenicity$server <- function(input, output,session) {
  cat("========================= Start Immunogenicity ======================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  
  observeEvent(input[['reset_input_immunogenicity']], {
    shinyjs::reset("immunogenicity_sidebar")
  })
  observeEvent(input[['reset_input_immunogenicity']], {
    output[['immunogenicity_maintabs']] <- NULL
  })
  
  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(input$immunogenicity_study_type,{
    study_type <- input$immunogenicity_study_type
    # 要求genicity_property有值
    temp.df <- study.df %>%
      dplyr::filter(genicity_property != '',
                    !is.na(genicity_property))
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{ #  if(study_type == 'GEO/Paper Studies')
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') #%>%
        # dplyr::filter(study_id != 'TCGA') %>%
        # dplyr::mutate(study_id = ifelse(study_id == 'TCGA',paste0(study_id,"_",tumor_detail),study_id))
    }
    temp.df
  })
  # 将获取到的内更新到页面
  observe({
    img_tumor_detail <- sort(unique(tumor.names.df()$tumor_detail))
    updatePickerInput(session = session, inputId = "immunogenicity_cancer_detail",
                      choices = img_tumor_detail, 
                      selected = img_tumor_detail[1])
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$immunogenicity_cancer_detail),{
    study_type <- input$immunogenicity_study_type
    cancer <- input$immunogenicity_cancer_detail
    # 要求genicity_property有值
    temp.df <- study.df %>%
      dplyr::filter(tumor_detail == cancer) %>%
      dplyr::filter(genicity_property != '',
                    !is.na(genicity_property))
    
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') #%>%
        # dplyr::filter(study_id != 'TCGA') %>%
        # dplyr::mutate(study_id = ifelse(study_id == 'TCGA',paste0(study_id,"_",tumor_detail),study_id))
      
    }
    temp.df
  })
  # 将获取到的肿瘤队列list更新到页面
  observe({
    img_study_id <- sort(unique(study.names.df()$study_id))
    updatePickerInput(session = session, inputId = "immunogenicity_study_id",
                      choices = img_study_id, 
                      selected = img_study_id[1])
  })
  # 2. 获取数据类型
  observeEvent(c(input$immunogenicity_study_id),{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    imm_in_study_type_id <- case_when(input$immunogenicity_study_type  == 'Immunogenomics Studies' ~ 'ICIs',
                                      TRUE  ~ 'nonICIs')
    
    imm_in_cancer_id <- input$immunogenicity_cancer_detail 
    imm_in_study_id <- input$immunogenicity_study_id
    cat(cat_prefix_img,"-前端返回的队列名称: ",imm_in_study_id,"\n")
    
    # 本次过滤后应只有一条记录
    temp.df <- study.df %>%
      dplyr::filter(study_type %in% imm_in_study_type_id) %>%
      dplyr::filter(tumor_detail %in% imm_in_cancer_id) %>%
      dplyr::filter(study_id %in% imm_in_study_id) 
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    # return(data.types)
    # 更新数据类型
    updatePickerInput(session = session, inputId = "immunogenicity_data_type",
                      choices = data.types, 
                      selected = data.types[1])
    
    db.tmp.dat <- data.frame()
    if(data.types[1] == 'Mutation'){
      tbl.name <- paste0(tolower(imm_in_study_id),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data.types[1] == 'Expr'){
      tbl.name <- paste0(tolower(imm_in_study_id),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data.types[1] == 'Proteome'){
      tbl.name <- paste0(tolower(imm_in_study_id),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # 更新基因名
    updateSelectizeInput(session = session,
                         inputId = "immunogenicity_symbol_id",
                         choices = unique(db.tmp.dat$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    # 更新突变类型
    updatePickerInput(session = session, inputId = "immunogenicity_vartype_id",
                      # label = "Select/deselect all options",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(`actions-box` = TRUE))
    
    # 更新yaisx的基因列表
    # updateSelectizeInput(session = session, 
    #                      inputId = "subgroup_factor_gene_id",
    #                      choices = unique(db.tmp.dat$hugo_symbol), 
    #                      selected = NULL,
    #                      server = TRUE)
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
  })
  
  # 3. 根据队列名称从数据获取队列内的基因、表达、突变、临床信息
  observeEvent(c(input$immunogenicity_data_type),{
    
    study <- input$immunogenicity_study_id
    data_type = input$immunogenicity_data_type
    cat(cat_prefix_img,"-前端返回的数据类型: ",data_type,"\n")
    
    db.tmp.dat <- data.frame()
    if(data_type == 'Mutation'){
      tbl.name <- paste0(tolower(study),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data_type == 'Expr'){
      tbl.name <- paste0(tolower(study),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data_type == 'Proteome'){
      tbl.name <- paste0(tolower(study),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # return(db.tmp.dat)
    # 更新基因名
    updateSelectizeInput(session = session,
                         inputId = "immunogenicity_symbol_id",
                         choices = unique(db.tmp.dat$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    # 更新突变类型
    updatePickerInput(session = session, inputId = "immunogenicity_vartype_id",
                      # label = "Select/deselect all options",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(`actions-box` = TRUE))
  })
  
  # 4. 更新单选框值
  observeEvent(input$immunogenicity_study_id,{
    img_yaxis_study_type <- case_when(input$immunogenicity_study_type  == 'Immunogenomics Studies' ~ 'ICIs',
                                      TRUE  ~ 'nonICIs')
    img_yaxis_study_id <- input$immunogenicity_study_id
    img_yaxis_cancer_detail <- input$immunogenicity_cancer_detail 
    # browser()
    # 本次过滤后应只有一条记录
    img_yaxis.df <- study.df %>%
      dplyr::filter(study_type %in% img_yaxis_study_type) %>%
      dplyr::filter(tumor_detail %in% img_yaxis_cancer_detail) %>%
      dplyr::filter(study_id %in% img_yaxis_study_id) %>%
      dplyr::distinct(study_id,.keep_all = T)
    img.yaxis.list <- unique(unlist(str_split(img_yaxis.df$data_type[1],"/")))
    if(grepl("Expr",img_yaxis.df$data_type[1]) &
       grepl("Proteome",img_yaxis.df$data_type[1])){ 
      img_yaxis_choices <- c("Clinicopathologicals", "mRNA","Protein","Pathways or Signatures")
    }else if(grepl("Expr",img_yaxis.df$data_type[1])){
      img_yaxis_choices <- c("Clinicopathologicals", "mRNA","Pathways or Signatures")
    }else if(grepl("Proteome",img_yaxis.df$data_type[1])){
      img_yaxis_choices <- c("Clinicopathologicals","Protein","Pathways or Signatures")
    }else{
      img_yaxis_choices <- c("Clinicopathologicals")
    }
    # update radio option
    updateAwesomeRadio(
      session = session, 
      inputId = "immunogenicity_yaxis_factor",
      choices = img_yaxis_choices,
      selected = img_yaxis_choices[1],
    )
    
  })
  
  # 4.1 根据队列名称从数据概括表获取临床属性,并更新到页面
  img.clinicalfactors.list <- eventReactive(input$immunogenicity_study_id,{
    img_clinicalfactors_study_id <- input$immunogenicity_study_id
    temp.df <- study.df %>%
      dplyr::filter(study_id == img_clinicalfactors_study_id) %>%
      dplyr::distinct(study_id,.keep_all = T)
    
    # 根据队列名称确定待分析免疫原性指标（事先存储于总表）
    temp.imgs <- unlist(strsplit(temp.df$genicity_property,split = "#",fixed = F))
    # 在study_info中，PD-L1标识为pdl1，在每个队列中，pdl1_exp是连续性变量，pdl1是分组变量
    if('pdl1' %in% temp.imgs){ 
      temp.imgs <- gsub("pdl1",'pdl1_exp',temp.imgs)
    }
    temp.imgs <- toupper(temp.imgs)
    return(temp.imgs)
  })
  observe({
    # 更新clincal属性
    updatePickerInput(session = session, inputId = "immunogenicity_clinical_id",
                      choices = img.clinicalfactors.list(), 
                      selected = img.clinicalfactors.list(),
                      options = list(
                        `actions-box` = TRUE))
  })
  # 4.2 根据队列名称获取基因symbol,并更新到页面
  img.genes.list <- eventReactive(c(input$immunogenicity_study_id,
                                    input$immunogenicity_yaxis_factor),{
    img_genes_symbol_id <- input$immunogenicity_symbol_id
    if(is.null(img_genes_symbol_id)) img_genes_symbol_id <- ''
    img_genes_study_id <- input$immunogenicity_study_id
    img_genes_yaxis_factor <- input$immunogenicity_yaxis_factor
    cat(cat_prefix_img,"-前端返回的单选框数据属性: ",img_genes_yaxis_factor,"\n")
    # browser()
    img_gene_names <- ''
    if(img_genes_yaxis_factor == 'mRNA'){
      tbl.name <- paste0(tolower(img_genes_study_id),'_expr')
      img_genes_dat <- queryDataFromMySQL(tbl.name)
      img_gene_names <- unique(img_genes_dat$hugo_symbol)
    }
    if(img_genes_yaxis_factor == 'Protein'){
      tbl.name <- paste0(tolower(img_genes_study_id),'_proteome')
      img_genes_dat <- queryDataFromMySQL(tbl.name)
      img_gene_names <- unique(img_genes_dat$hugo_symbol)
    }
    img_gene_names <- setdiff(img_gene_names,img_genes_symbol_id)
    return(img_gene_names)
  })
  observe({
    # 更新基因名称
    updateSelectizeInput(session = session,
                         inputId = "immunogenicity_genes_id",
                         choices = img.genes.list(),
                         selected = NULL,
                         server = TRUE)
  })
  # 4.3 基因Signature 或 cell sets 更新
  observeEvent(input$immunogenicity_yaxis_factor,{
    # img_choices_df <- data.frame('sigtype' = c(rep('Gene',length(tme.gene.corr)),rep('Pathway',length(tme.cell.corr))),
    #                             'sigvalue' = c(tme.gene.corr,tme.cell.corr)) %>%
    #   dplyr::filter(sigtype == "Pathway")
    img_choices_df <- tme.cell.df %>%
      dplyr::select(short_name,main_category) %>%
      dplyr::distinct()
    # browser()
    img_choices <- lapply(split(img_choices_df$short_name, img_choices_df$main_category), as.list)
    updatePickerInput(session = session, inputId = "immunogenicity_signature_id",
                      choices = img_choices, 
                      selected = img_choices[[1]][[1]])
  })
  
  # 5. 基因表达分组cutoff更新联动
  observe({
    number1 <- input$immunogenicity_exprcut1_id  # value 1
    updateNumericInput(session, inputId = "immunogenicity_exprcut2_id",
                       label = "Cutoff (%,Low):",
                       value = 100-number1,
                       min = 0, max = 100, step = NA)
  })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$immunogenicity_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "immunogenicity_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  
  observe({
    shinyjs::toggleState("immunogenicity_goButton", 
                         !is.null(input$immunogenicity_symbol_id) && input$immunogenicity_symbol_id != "" )
  })
  
  # 重置输入
  # observeEvent(input$immunogenicity_reset, {
  #   shinyjs::reset('immunogenicity_sidebar')
  #   shinyjs::reset('immunogenicity_mainpage')
  # })
  
  
  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$immunogenicity_study_id,{
    cat(cat_prefix_img,"-查询Clinical，待分析的研究队列: ",input$immunogenicity_study_id,"\n")
    study <- input$immunogenicity_study_id
    cancer_id <- input$immunogenicity_cancer_detail # cancer_detail
    temp.df <- queryDataFromMySQL(paste0(tolower(study),'_clinical'))
    temp.df <- temp.df %>%
      dplyr::filter(cancer_detail %in% cancer_id)
    
    if("ici_treatment" %in% colnames(temp.df)){
      temp.df <- temp.df %>%
        # 免疫治疗队列有ici_treatment字段，需过滤出免疫治疗人群
        dplyr::filter(ici_treatment == 'Yes') 
    }
    if("sample_type" %in% colnames(temp.df)){
      temp.df <- temp.df %>%
        # GEO队列存在非肿瘤样本，需选择出肿瘤样本
        dplyr::filter(sample_type == 'T') 
    }
    return(temp.df)
  })
  # 突变或者表达谱
  # cat("查询Mutation/Expr，待分析的研究队列: ",study(),"\n")
  # tbl_name <- case_when(data_type() == 'DNA' ~ paste0(tolower(study()),'_mutation'),
  #                       data_type() == 'RNA' ~ paste0(tolower(study()),'_expr'),
  #                       TRUE ~ '')
  # db.dat <- queryDataFromMySQL(tbl_name)
  
  db.dat <- eventReactive(c(input$immunogenicity_study_id,
                            input$immunogenicity_data_type),{
                              cat(cat_prefix_img,"-查询Mutation/Expr，待分析的研究队列: ",input$immunogenicity_study_id,"\n")
                              data_type <- input$immunogenicity_data_type
                              study <- input$immunogenicity_study_id
                              tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                                                    data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                                                    data_type == 'Proteome' ~ paste0(tolower(study),'_proteome'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })
  
  # 输入为空验证
  iv_immunogenicity <- InputValidator$new()
  iv_immunogenicity$add_rule("immunogenicity_symbol_id", sv_required())
  iv_immunogenicity$add_rule("immunogenicity_vartype_id", sv_required())
  iv_immunogenicity$add_rule("immunogenicity_clinical_id", sv_required())
  iv_immunogenicity$add_rule("immunogenicity_genes_id", sv_required())
  iv_immunogenicity$add_rule("immunogenicity_signature_id", sv_required())
  iv_immunogenicity$enable()
  
  # 业务层：KM分析
  observeEvent(input$immunogenicity_goButton,{
    cat("===================== Server Immunogenicity =======================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting immunogenicity analysis ..."
    )
    selected_study_id = input$immunogenicity_study_id
    selected_tumor_detail_id = input$immunogenicity_cancer_detail
    cat(cat_prefix_img,"-选择的研究队列: ", selected_study_id, "\n")
    
    data_type <- input$immunogenicity_data_type
    cat(cat_prefix_img,"-选择的队列数据类型: ", data_type, "\n")
    
    # validate(need(try(input$immunogenicity_symbol_id),"Please select at least one gene!"))
    symbol <- input$immunogenicity_symbol_id
    cat(cat_prefix_img,"-选择的基因名称: ", symbol, "\n")
    
    vartype <- input$immunogenicity_vartype_id
    cat(cat_prefix_img,"-选择的基因突变类型: ", vartype, "\n")
    
    exprcut1 <- input$immunogenicity_exprcut1_id
    exprcut2 <- input$immunogenicity_exprcut2_id
    cat(cat_prefix_img,"-选择的基因高表达分位数: ", exprcut1, "\n")
    cat(cat_prefix_img,"-选择的基因低表达分位数: ", exprcut2, "\n")
    
    colorg1 <- input$immunogenicity_colorg1_id
    colorg2 <- input$immunogenicity_colorg2_id
    cat(cat_prefix_img,"-选择的样本分组1颜色: ", colorg1, "\n")
    cat(cat_prefix_img,"-选择的样本分组2颜色: ", colorg2, "\n")
    
    immunogenicity_logical_type <- input$immunogenicity_logical
    cat(cat_prefix_img,"-选择的多基因突变逻辑关系: ", immunogenicity_logical_type, "\n")
    
    immunogenicity_yaxis_selected <- input$immunogenicity_yaxis_factor
    cat(cat_prefix_img,"-选择的指标类型: ", immunogenicity_yaxis_selected, "\n")
    if(immunogenicity_yaxis_selected == 'Clinicopathologicals'){
      immunogenicity_clinicals <- input$immunogenicity_clinical_id
      immunogenicity_clinicals <- tolower(immunogenicity_clinicals)
      cat(cat_prefix_img,"-选择的临床指标: ", immunogenicity_clinicals, "\n")
    }
    if(immunogenicity_yaxis_selected == 'mRNA'){
      immunogenicity_genes <- input$immunogenicity_genes_id
      cat(cat_prefix_img,"-选择的基因指标: ", immunogenicity_genes, "\n")
      
      img.expr.dat <- queryDataFromMySQL(paste0(tolower(selected_study_id),"_expr"))
    }
    if(immunogenicity_yaxis_selected == 'Protein'){
      immunogenicity_genes <- input$immunogenicity_genes_id
      cat(cat_prefix_img,"-选择的蛋白指标: ", immunogenicity_genes, "\n")
      
      img.expr.dat <- queryDataFromMySQL(paste0(tolower(selected_study_id),"_proteome"))
    }
    
    if(immunogenicity_yaxis_selected == 'Pathways or Signatures'){
      immunogenicity_signatures <- input$immunogenicity_signature_id
      cat(cat_prefix_img,"-选择的Signature指标: ", immunogenicity_signatures, "\n")
      
      img.expr.dat <- queryDataFromMySQL(paste0(tolower(selected_study_id),"_expr"))
      
      # 如果选择pathways，先计算ssGSEA score
      # 细胞集名称时，需转换为基因名称
      cell.df <- tme.cell.df %>%
        dplyr::filter(short_name %in% immunogenicity_signatures) %>%
        dplyr::select(short_name,gene)
      
      img.cell.sets <- split(cell.df$gene,       # gene
                             cell.df$short_name) # pathway name
      # img.expr.dat行为基因列为样本，为转录组数据计算ssGSEA score
      img.gene.expr <- img.expr.dat %>%             
        tibble::column_to_rownames(var = 'hugo_symbol') %>%
        t() %>% 
        scale() %>%  # 转置后将表达谱进行Z-score处理
        t()    
      
      img.ssgsea.df <- gsva(expr = as.matrix(img.gene.expr),
                            gset.idx.list = img.cell.sets,
                            kcdf = "Gaussian",
                            parallel.sz = 1,
                            method = "ssgsea",
                            mx.diff = TRUE)
      
      img.ssgsea.df <- round(img.ssgsea.df,4)
      img.ssgsea.out <- img.ssgsea.df %>% # img.ssgsea.df：行为细胞，列为样本
        t() %>% as.data.frame()  %>%
        tibble::rownames_to_column(var = 'sample_id')
      
    }
    
    # 数据清理、绘图、输出
    input.genes <- symbol 
    if(data_type == 'Mutation'){
      # input.genes <- c(input.genes,"All Queried Genes")
      if(length(input.genes) >1){
        input.genes <- c(input.genes,"All Queried Genes")
      }
    }
    input.vartype <- vartype
    input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    input.exprcut2 <- exprcut2/100
    # input.color <- c(colorg1,colorg2)
    
    # 根据队列名称确定待分析免疫原性指标（事先存储于总表）
    # temp.df <- study.df %>%
    #   dplyr::filter(study_id == selected_study_id,
    #                 tumor_detail == selected_tumor_detail_id) 
    # temp.immunogenicities <- unlist(strsplit(temp.df$genicity_property,split = "#",fixed = F))
    # 在study_info中，PD-L1标识为pdl1，在每个队列中，pdl1_exp是连续性变量，pdl1是分组变量
    # if('pdl1' %in% temp.immunogenicities){ 
    #   temp.immunogenicities <- gsub("pdl1",'pdl1_exp',temp.immunogenicities)
    # }
    if(immunogenicity_yaxis_selected == 'Clinicopathologicals'){
      temp.immunogenicities <- immunogenicity_clinicals
    }
    if(immunogenicity_yaxis_selected == 'mRNA'|
       immunogenicity_yaxis_selected == 'Protein'){
      temp.immunogenicities <- immunogenicity_genes
    }
    if(immunogenicity_yaxis_selected == 'Pathways or Signatures'){
      temp.immunogenicities <- immunogenicity_signatures
    }
    
    # 写出到页面
    output$immunogenicity_maintabs <- renderUI({
      tabs <- lapply(input.genes, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,"_imgen"))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    # 执行数据分析
    n <- length(input.genes)
    lapply(seq(1,length(input.genes)), function(i) {
      
      gname <- input.genes[i]
      symbol.name <- gname
      if(gname == 'All Queried Genes') {symbol.name <- setdiff(input.genes,gname)} # EGFR#TP53#ALK
      
      update_modal_progress(
        value = i / n,
        text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
      )
      # browser()
      if(data_type == 'Mutation'){
        # 临床样本与突变样本取交集
        tumor.samples <- intersect(unique(db.dat()$sample_id),db.dat.cli()$sample_id)
        
        if(gname == 'All Queried Genes'){
          # 多个基因时
          if(immunogenicity_logical_type == 'AND'){
            # logical 为 AND 时
            cat(cat_prefix_img,"- 逻辑与选择多基因突变模式！\n")
            tmp.mut.samples <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% symbol.name,
                            variant_classification %in% input.vartype) %>%
              dplyr::select(sample_id,hugo_symbol) %>%
              dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
              dplyr::group_by(sample_id) %>%
              dplyr::summarise(sample_count = n()) %>%
              dplyr::ungroup() %>%
              dplyr::filter(sample_count == length(symbol.name)) # 取
          }
          if(immunogenicity_logical_type == 'OR'){
            # logical 为 OR 时
            cat(cat_prefix_img,"- 逻辑或选择多基因突变模式！\n")
            tmp.mut.samples <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% symbol.name,
                            variant_classification %in% input.vartype) %>%
              dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
              dplyr::group_by(sample_id) %>%
              dplyr::summarise(sample_count = n()) %>%
              dplyr::ungroup() %>%
              dplyr::filter(sample_count >= 1) # 取
          }
          grp1.df <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% symbol.name,
                          variant_classification %in% input.vartype) %>%
            dplyr::filter(sample_id %in% tmp.mut.samples$sample_id) %>%
            dplyr::select(sample_id,hugo_symbol)
        }else{
          # 单个基因时
          grp1.df <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% symbol.name,
                          variant_classification %in% input.vartype) %>%
            dplyr::select(sample_id,hugo_symbol)
        }
        if(immunogenicity_yaxis_selected == 'Clinicopathologicals'){
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::mutate(group = ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','WT')) %>%
            # 本处给突变类型时，初始化为一个很大的值，但后续绘图时，突变数据只绘制蜂群图，不绘制相关系数图
            dplyr::mutate(sltgene = 100000) %>%
            dplyr::select(sample_id,group,sltgene,all_of(temp.immunogenicities))
        }else{
          # 基因表达、蛋白丰度、免疫细胞集时，只需要分组信息
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::mutate(group = ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','WT')) %>%
            # 本处给突变类型时，初始化为一个很大的值，但后续绘图时，突变数据只绘制蜂群图，不绘制相关系数图
            dplyr::mutate(sltgene = 100000) %>%
            dplyr::select(sample_id,group,sltgene)
          
          # 再整合基因表达、蛋白丰度、免疫细胞集信息
          if(immunogenicity_yaxis_selected == 'Pathways or Signatures'){
            # 细胞集名称时，需转换为基因名称
            # cell.df <- tme.cell.df %>%
            #   dplyr::filter(short_name %in% temp.immunogenicities) %>%
            #   dplyr::select(short_name,gene)
            # 
            # img.cell.sets <- split(cell.df$gene,       # gene
            #                    cell.df$short_name) # pathway name
            # img.expr.dat行为基因列为样本，为转录组数据计算ssGSEA score
            # img.gene.expr <- img.expr.dat %>%             
            #   tibble::column_to_rownames(var = 'hugo_symbol') %>%
            #   t() %>% 
            #   scale() %>%  # 转置后将表达谱进行Z-score处理
            #   t()    
            # 
            # img.ssgsea.df <- gsva(expr = as.matrix(img.gene.expr),
            #                   gset.idx.list = img.cell.sets,
            #                   kcdf = "Gaussian",
            #                   parallel.sz = 4,
            #                   method = "ssgsea",
            #                   mx.diff = TRUE)
            # 
            # img.ssgsea.out <- img.ssgsea.df %>% # img.ssgsea.df：行为细胞，列为样本
            #   t() %>% as.data.frame()  %>%
            #   tibble::rownames_to_column(var = 'sample_id')
            
            clc.cli <- clc.cli %>%
              dplyr::inner_join(img.ssgsea.out ,by='sample_id')
            
            
          }else{
            img.temp.df <- img.expr.dat %>%
              dplyr::filter(hugo_symbol %in% temp.immunogenicities) %>%
              tibble::column_to_rownames(var = 'hugo_symbol') %>%
              t() %>% as.data.frame() %>%
              tibble::rownames_to_column(var = 'sample_id')
            
            clc.cli <- clc.cli %>%
              dplyr::inner_join(img.temp.df,by='sample_id')
          }
          
        }
        
        
      }
      # for expr
      if(data_type %in% c('Expr','Proteome')){
        # 临床样本与表达谱样本取交集
        tumor.samples <- intersect(colnames(db.dat()),db.dat.cli()$sample_id)
        # symbol.name必须为单个基因的名称
        grp1.df <- db.dat() %>%
          dplyr::select(hugo_symbol,tumor.samples) %>%
          dplyr::filter(hugo_symbol %in% symbol.name) %>%
          tibble::column_to_rownames(var = 'hugo_symbol') %>%
          t() %>% as.data.frame() %>%
          tibble::rownames_to_column(var = 'sample_id')
        colnames(grp1.df)[2] <- "sltgene"
        
        cutoff_h <- quantile(as.numeric(grp1.df$sltgene),input.exprcut1,na.rm = T)
        cutoff_l <- quantile(as.numeric(grp1.df$sltgene),input.exprcut2,na.rm = T)
        
        quantile.25 <- quantile(as.numeric(grp1.df$sltgene),0.25,na.rm = T)
        quantile.75 <- quantile(as.numeric(grp1.df$sltgene),0.75,na.rm = T)
        if(quantile.25 == quantile.75){
          cat("基因：",symbol.name,"的第一分位数和第三分为数相等,无法以75%以内分位值为阈值。系统将以第一个不同的表达值为阈值分组\n")
          cutoff_h <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
          cutoff_l <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
        }
        cat("Cutoff of Expression: ",cutoff_h," (High); ",cutoff_l," (Low)\n")
        
        grp1.df <- grp1.df %>%
          dplyr::mutate(sltgene = as.numeric(sltgene)) %>%
          dplyr::mutate(group = ifelse(sltgene >= cutoff_h,"High",
                                       ifelse(sltgene <= cutoff_l,'Low','Middle'))) %>%
          dplyr::filter(group %in% c('Low','High')) %>%
          dplyr::select(sample_id,group,sltgene)
        
        
        if(immunogenicity_yaxis_selected == 'Clinicopathologicals'){
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::inner_join(grp1.df,by='sample_id')  %>%
            dplyr::select(sample_id,group,sltgene,all_of(temp.immunogenicities))
        }else{
          # 基因表达、蛋白丰度、免疫细胞集时，只需要分组信息
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::inner_join(grp1.df,by='sample_id')  %>%
            dplyr::select(sample_id,group,sltgene)
          
          # 再整合基因表达、蛋白丰度、免疫细胞集信息
          if(immunogenicity_yaxis_selected == 'Pathways or Signatures'){
            # 细胞集名称时，需转换为基因名称
            # cell.df <- tme.cell.df %>%
            #   dplyr::filter(short_name %in% temp.immunogenicities) %>%
            #   dplyr::select(short_name,gene)
            # 
            # img.cell.sets <- split(cell.df$gene,       # gene
            #                        cell.df$short_name) # pathway name
            # img.expr.dat行为基因列为样本，为转录组数据计算ssGSEA score
            # img.gene.expr <- img.expr.dat %>%             
            #   tibble::column_to_rownames(var = 'hugo_symbol') %>%
            #   t() %>% 
            #   scale() %>%  # 转置后将表达谱进行Z-score处理
            #   t()    
            # 
            # img.ssgsea.df <- gsva(expr = as.matrix(img.gene.expr),
            #                       gset.idx.list = img.cell.sets,
            #                       kcdf = "Gaussian",
            #                       parallel.sz = 4,
            #                       method = "ssgsea",
            #                       mx.diff = TRUE)
            # 
            # img.ssgsea.out <- img.ssgsea.df %>% # img.ssgsea.df：行为细胞，列为样本
            #   t() %>% as.data.frame()  %>%
            #   tibble::rownames_to_column(var = 'sample_id')
            
            clc.cli <- clc.cli %>%
              dplyr::inner_join(img.ssgsea.out ,by='sample_id')
            
            
          }else{
            img.temp.df <- img.expr.dat %>%
              dplyr::filter(hugo_symbol %in% temp.immunogenicities) %>%
              tibble::column_to_rownames(var = 'hugo_symbol') %>%
              t() %>% as.data.frame() %>%
              tibble::rownames_to_column(var = 'sample_id')
            
            clc.cli <- clc.cli %>%
              dplyr::inner_join(img.temp.df,by='sample_id')
          }
          
        }
      }
      
      # 根据临床属性定义输出的tab项
      cli.tab.names <- temp.immunogenicities
      cli.tab.names <- cli.tab.names[which(cli.tab.names != '')]
      cat(cat_prefix_img,"-待分析的免疫原性指标: ",cli.tab.names,"\n")
      # print(clc.cli[1:5,])
      
      # 绘Boxplot图
      img.plot.list <- list()
      img.tabl.list <- list()
      # lapply(cli.tab.names, function(cli.name) 
      for(cli.name in cli.tab.names){
        
        # cli.tab.names 为不同的变异原性指标：TMB，neoantigen,PDL1，tumor_purity等
        # browser()
        km.df <- clc.cli %>%
          dplyr::select(sample_id,img_name = cli.name,group,sltgene)%>%
          dplyr::filter(!is.na(img_name))  %>% # 删除NA的记录
          dplyr::mutate(img_name = as.numeric(img_name))
        
        km.graphic <- func.scatterbwplot(km.df,cli.name,gname,colorg2,colorg1)
        
        output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- km.df
        output.graphic[[paste0(gname,"_",tolower(cli.name))]]$km <- km.graphic
        
        # 执行完绘图， 更新数据列名为当前分析因素
        colnames(km.df) <- c("sample_id", cli.name, paste0(gname,"_group"),gname)
        
        img.plot.list[[cli.name]] <- km.graphic
        img.tabl.list[[cli.name]] <- km.df
        
      }
      
      # 整合当前输出图表
      {
        # browser()
        # 图
        n.col <- min(5,length(img.plot.list))
        n.row <- max(1,ceiling(length(img.plot.list)/n.col))
        if(data_type == 'Mutation') {
          out.plot <- ggarrange(plotlist = img.plot.list,
                                ncol = n.col,
                                nrow = n.row,
                                # labels = names(img.plot.list),
                                widths = 400*n.col,
                                heights = 500 * n.row)
          output.width <- 400*n.col
          output.height <- 300 * n.row
        }else{
          n.col = 3
          n.row <- max(1,ceiling(length(img.plot.list)/n.col))
          out.plot <- ggarrange(plotlist = img.plot.list,
                                ncol = n.col,
                                nrow = n.row,
                                # labels = names(img.plot.list),
                                widths = 700*n.col,
                                heights = 400 * n.row
                                )
          output.width <- 700*n.col
          output.height <- 300 * n.row
        }
        
        
        # 表
        tabl.names <- names(img.tabl.list)
        out.tabl <- img.tabl.list[[tabl.names[1]]]
        if(data_type == 'Mutation') {
          out.tabl <- out.tabl %>%
            dplyr::select(-gname) %>%
            dplyr::select(sample_id,paste0(gname,'_group'),everything())
        }else{
          out.tabl <- out.tabl %>%
            dplyr::select(sample_id,gname,paste0(gname,'_group'),everything())
        }
        
        if(length(tabl.names)>1){
          for (i in c(2:length(img.tabl.list))) {
            tmp.tabl<- img.tabl.list[[tabl.names[i]]]
            out.tabl <- out.tabl  %>%
              dplyr::full_join(tmp.tabl %>%
                                 dplyr::select(-gname,-paste0(gname,'_group')),
                               by='sample_id') 
          }
        }
        colnames(out.tabl) <- toupper(colnames(out.tabl))
        output.graphic[[paste0(gname,"_img_tabl")]]$tbl <- out.tabl
        output.graphic[[paste0(gname,"_img_plot")]]$imgplot <- out.plot
    }
    {
      output[[paste0(gname,"_imgen")]] <- renderUI({
        
        tagList(
          shinycssloaders::withSpinner(plotOutput(paste0(gname, "_img_plot"),height = output.height,width = 'auto')),
          h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queried Genes: mutation of at least one of query genes. Mut: mutation, WT: wild-type. The Mann-Whitney U test is used to compare differences between two groups", "</span>"))),
          downloadButton(paste0(gname, "_img_plot_dl"),
                         tags$span(
                           "DLGraph",
                           add_prompt(tags$span(icon(name = "circle-question")),
                                      message = "Save plot in a pdf file.",
                                      position = "left")),
                         style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
          downloadButton(paste0(gname, "_img_tabl_dl"),
                         tags$span(
                           "DLTable",
                           add_prompt(tags$span(icon(name = "circle-question")),
                                      message = "Save data of plot in a txt file.",
                                      position = "left")),
                         style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),  
          br(),
          DT::dataTableOutput(paste0(gname, "_img_tabl"))
        )
        
      })
        # 图输出到页面
        output[[paste0(gname,"_img_plot")]] <- renderPlot({
          print(out.plot)
        })
        
        # 表输出到页面
        output[[paste0(gname,"_img_tabl")]] <- renderDataTable({
          datatable(out.tabl, 
                    style = 'bootstrap', 
                    rownames = FALSE, 
                    selection = list(mode = "single", target = "row"),
                    options = list(scrollX = TRUE, 
                                   orderClasses = TRUE,
                                   keys = TRUE,dom = 'Bfrtip', searching = F,
                                   language = list(zeroRecords = "No records found matching your selection"),
                                   columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                    colnames = c("Sample ID", colnames(out.tabl)[-1])
          )
        })
        
        # 下载图
        output[[paste0(gname, "_img_plot_dl")]] <- downloadHandler(
          filename = function() {
            if(grepl("#",gname)) gname <- "Comb"
            paste0(prefix_output_file,"_Immunnogenicity_Plot_",gname,"_",Sys.Date(),".pdf", sep = "")
          },
          content = function(file) {
            pdf(file,onefile = F, width = (output.width/100)*0.8, height = output.height/100, pointsize = 10)
            print(output.graphic[[paste0(gname,"_img_plot")]]$imgplot)
            dev.off()
          }
        )
        # 下载表
        output[[paste0(gname, "_img_tabl_dl")]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_Immunnogenicity_Data_",gname,"_",Sys.Date(), '.txt', sep='')
          },
          content = function(file) {
            readr::write_delim(x = output.graphic[[paste0(gname,"_img_tabl")]]$tbl, path = file, delim = "\t")
          }
        )
      }
      # Increment the progress bar, and update the detail text.
      # incProgress(1/n, detail = paste("Analyzing gene: ", gname))
      Sys.sleep(0.01)
    })
    remove_modal_progress()
  })
  
  
}
