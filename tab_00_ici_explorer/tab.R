#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-18
# @IDE     : RStudio
# @Desc    : 第0个Tab功能：免疫治疗预测因子探索
#===============================================================================

source("./r/func.survivalplot.R")
source("./r/func.ggboxplot.R")
source("./r/func.clinical_wilcoxon_test.R")

# 预测：初始化ICI治疗突变队列 ----
{
  # 步骤：1. 选择瘤种 --> 2. 选择队列 --> 3.选择基因 --> 4.分析输入基因（单个/全部组合）的突变与WT的生存获益，以及其他指标的差异 --> 4.表格输出结果；
  # 获取ICI队列名称
  icicohort.df <- study.df %>%
    dplyr::filter(study_type == 'ICIs',
                  grepl("Mutation",data_type))
  # 肿瘤名称
  ici.tumor.detail <- unique(icicohort.df$tumor_detail)
  cat("========>",ici.tumor.detail,"\n")
  
  # 预先将免疫队列的突变和临床数据加载到缓存
  # 遍历肿瘤
  ici.cohort.list <- list()
  length(ici.cohort.list) <- length(unique(ici.tumor.detail))
  for (j in seq(1,length(unique(icicohort.df$tumor_detail)))) {
    # j=1
    cancer <- unique(icicohort.df$tumor_detail)[j]
    
    # sid.list <- unique(icicohort.df[which(icicohort.df$tumor_detail == cancer),]$study_id)
    sid.df <- icicohort.df %>%
      dplyr::filter(tumor_detail == cancer) %>%
      dplyr::select(study_id,genomic_platform) %>%
      dplyr::distinct(study_id,.keep_all = T)
    sid.list <- sid.df$study_id
    # 遍历队列
    ici.mut.list <- list()
    ici.cli.list <- list()
    length(ici.mut.list) <- length(sid.list)
    length(ici.cli.list) <- length(sid.list)
    ici.symbols <- c()
    for(i in seq(1,length(sid.list))){
      # i =1
      sid <-  sid.list[i]
      tmp.df <- icicohort.df %>%
        dplyr::filter(study_id == sid)
      if(grepl("Mutation",tmp.df$data_type[1])){
        
        names(ici.mut.list)[[i]] = paste0(sid," (",sid.df$genomic_platform[i],")")
        ici.mut.list[[i]] <- data.frame()
        
        names(ici.cli.list)[[i]] = paste0(sid," (",sid.df$genomic_platform[i],")")
        ici.cli.list[[i]] <- data.frame()
        
        tbl.name <- paste0(tolower(sid),"_mutation")
        tmp.mut.df <- queryDataFromMySQL(tbl.name)
        ici.mut.list[[i]] <- tmp.mut.df
        
        tmp.cli.df <- queryDataFromMySQL(paste0(tolower(sid),"_clinical"))
        ici.cli.list[[i]] <- tmp.cli.df
        
        if(i == 1){
          ici.symbols <- unique(tmp.mut.df$hugo_symbol)
        }else{
          ici.symbols <- union(ici.symbols,unique(tmp.mut.df$hugo_symbol))
        }
        
      }
    }
    names(ici.cohort.list)[[j]] = cancer
    ici.cohort.list[[j]] <- list()
    ici.cohort.list[[j]] <- list("mutation" = ici.mut.list,
                                 "clinical" = ici.cli.list,
                                 "symbols" = ici.symbols)
  }
  # 所有免疫队列中的基因symbol并集
  # ici.symbols.df <- do.call(rbind,lapply(seq(1,length(ici.mut.list)), function(i){
  #   # 遍历所有队列获取并集
  #   # i = 1
  #   tmp.mut.df <-  ici.mut.list[[i]]
  #   
  #   if("hugo_symbol" %in% colnames(tmp.mut.df)){
  #     tmp.out <- data.frame(study_id = sid,
  #                           hugo_symbol = unique(tmp.mut.df$hugo_symbol))
  #   }else{
  #     tmp.out <- data.frame(study_id = sid,
  #                           hugo_symbol = '')
  #   }
  #   return(tmp.out)
  # }))
  # ici.symbols <- unique(ici.symbols.df$hugo_symbol)
  cat("免疫治疗泛队列分析数据加载完成!\n")
}

tab_00_ici_explorer <- list()
cat_prefix_ici <- 'ICIExplore'
sidebar <- sidebarPanel(
  id = 'iciexplore_sidebar',
  width = 3,
  h3("Immunotherapy Predictive Biomarkers Exploration"),
  
  pickerInput(inputId = "iciexplore_cancer_id",
              label = "Select one cancer type, E.g. 'LUAD'",
              choices = NULL,
              multiple = FALSE,
              selected = NULL),
  pickerInput(inputId = "iciexplore_study_id",
              label = "Select one or more cohorts, E.g. 'Hellmann_2018'",
              choices = NULL,
              multiple = TRUE,
              selected = NULL),
  
  selectizeInput(inputId = "iciexplore_preset_id", 
                 label = "Pre-defined gene set or user-defined list:",
                 choices = NULL, 
                 width = '100%',
                 selected = NULL,
                 multiple = F),
  
  selectizeInput(inputId = "iciexplore_symbol_id", 
                 # label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                 label = tags$span(
                   add_prompt(tags$span(icon(name = "circle-question")),
                              message = "if more then 11 genes, only two forms (the signle gene and all genes) will be calculated.", 
                              position = "right"),
                   "Select/Input gene symbols, E.g. 'EGFR STK11'"),
                 choices = NULL, 
                 width = '100%',
                 multiple = T, 
                 options = list(delimiter = " ", create = T,maxItems = 50,
                                placeholder = "No more than 50 genes.",
                                plugins = list('remove_button', 'drag_drop'))),
  awesomeRadio(
    inputId = "ici_logical",
    label = "Mutations in all(AND)/any(OR) quried genes",
    inline = T,
    choices = c("AND" ,"OR"),
    selected = "AND",
    status = "success",
    checkbox = TRUE
  ),
  # 提交按钮
  useShinyjs(),
  # actionButton(inputId = "iciexplore_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "iciexplore_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_ici", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
)

mainpage <- mainPanel(
  id = 'iciexplore_mainpage',
  width = 9,
  uiOutput(outputId='iciexplore_maintabs')
)

tab_00_ici_explorer$ui <- sidebarLayout(sidebar, mainpage)

tab_00_ici_explorer$server <- function(input, output,session) {
  cat("========================= Start Explore=============================\n")
  # 定义变量存储输出对象
  middle.graphic <- reactiveValues()
  output.graphic <- reactiveValues()
  output.metalist <- reactiveValues()
  
  observeEvent(input$reset_input_ici, {
    shinyjs::reset("iciexplore_sidebar")
  })
  
  observeEvent(input$reset_input_ici, {
    
    output$iciexplore_maintabs  <- NULL
    # output$iciexplore_maintabs <- renderUI(
    #   fluidPage(
    #     box(title = "User Guide", width = 11, solidHeader = T, status = "primary",
    #         column(12,
    #                tags$img(
    #                  src = "www/tour_icip.png"
    #                )
    #         )
    #     )
    #     
    #   )
    # )
      
    # 引入操作流程，此处整合后会影响分析过程的进度条，
    # output$iciexplore_maintabs <- renderUI(
    #   fluidRow(
    #     box(title = "User Guide", width = 12, solidHeader = T, 
    #         (div(style='width:100%;height:600px;overflow-y: scroll;',
    #              column(12,
    #                     includeHTML(
    #                       rmarkdown::render(
    #                         input="tab_00_ici_explorer/impact_pred_wf.Rmd"))))))))
  })
  
  # 将获取到的肿瘤名称更新到页面
  observe({
    updatePickerInput(session = session, 
                      inputId = "iciexplore_cancer_id",
                      choices = c(ici.tumor.detail), 
                      selected = ici.tumor.detail[1])
  })
  
  # 根据选择的瘤种，确定突变队列
  ici.data.list <- eventReactive(input$iciexplore_cancer_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    select_tumor <- input$iciexplore_cancer_id
    info(file_logger, paste0(cat_prefix_ici,"-选择的瘤种是:",select_tumor))
    output.list <- ici.cohort.list[select_tumor][[1]]
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  # 将获取到的队列名称更新到页面
  observe({
    updatePickerInput(session = session, 
                      inputId = "iciexplore_study_id",
                      choices = c(names(ici.data.list()[['mutation']])), 
                      selected = c(names(ici.data.list()[['mutation']])),
                      options = list(
                        `actions-box` = TRUE))
  })
  # 基因集定义：自选或者已有基因集合
  ici.geneset.list <- eventReactive(input$iciexplore_study_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    output.list <- c("User-defined List",names(pre_genesets_list))
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  observe({
    # 更新基因集
    updateSelectizeInput(session = session, 
                         inputId = "iciexplore_preset_id", 
                         choices = c(ici.geneset.list()), 
                         selected = "User-defined List", 
                         server   = TRUE)
  })
  
  # 基因选择确定
  ici.gene.list <- eventReactive(c(input$iciexplore_preset_id),{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    selected_set_name <- input$iciexplore_preset_id       
    if(selected_set_name != 'User-defined List'){
      output.list <- intersect(ici.data.list()[['symbols']],pre_genesets_list[[selected_set_name]])
    }else{
      output.list <- ''
    }
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  observe({
    # 更新基因名
    updateSelectizeInput(session = session, 
                         inputId = "iciexplore_symbol_id", 
                         choices = c(ici.data.list()[['symbols']]), 
                         selected = ici.gene.list(), 
                         server   = TRUE)
  })
  
  observe({
    shinyjs::toggleState("iciexplore_goButton", 
                         !is.null(input$iciexplore_symbol_id) && input$iciexplore_symbol_id != "")
  })

  selected.row <- eventReactive(input$pancancer_table_rows_selected,{
    slted.row <- input$pancancer_table_rows_selected
    info(file_logger, paste0(cat_prefix_ici,"-选择的行是:",slted.row))
    return(slted.row)
  })
  
  # 输入为空验证
  iv_iciexplore <- InputValidator$new()
  iv_iciexplore$add_rule("iciexplore_study_id", sv_required())
  iv_iciexplore$add_rule("iciexplore_symbol_id", sv_required())
  iv_iciexplore$enable()
  
  # 业务层：KM分析
  observeEvent(input$iciexplore_goButton,{
    cat("====================== Server Explore=============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting exploration ..."
    )
    
    # 先遍历队列获取肿瘤突变和临床信息，再遍历基因，再遍历outcomes
    # 外层Tab
    output$iciexplore_maintabs <- renderUI({
      fluidPage(
        fluidRow(
          column(width = 9),
          # tabPanel(
          #   style = "padding-top:15px",
          #   title = paste0("tbl_tab"), status = "primary", solidHeader = TRUE, collapsible = TRUE,
          #   
          # )
          DT::dataTableOutput(paste0('pancancer', "_table")),
          h5(HTML(paste("<span style=color:black;font-size:12px;>", "gene1#gene2#...: mutation of at least one of these genes. 
                        mut: mutation, wt: wild type.
                        cat: categorical variable. 
                        P value of continuous clinical variables was calculated from the wilcoxon signed rank test. 
                        While the chi-square test was used for categorical variable.", "</span>"))),
          downloadButton(paste0('pancancer', "_table_dl"), 
                         tags$span(
                           "DLTable",
                           add_prompt(tags$span(icon(name = "circle-question")),
                                      message = "Save results in a txt file.", 
                                      position = "left")),
                         style = "display:inline-block;float:left;color: #fff; 
                         background-color: #27ae60; border-color: #fff;
                         padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
          downloadButton(paste0("selected_row_plot_dl"), 
                         tags$span(
                           "DLGraph",
                           add_prompt(tags$span(icon(name = "circle-question")),
                                      message = "Save KM cure and boxplot in a PDF file.", 
                                      position = "left")),
                         style = "display:inline-block;float:left;color: #fff; 
                         background-color: #27ae60; border-color: #fff;
                         padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; ")
        ),
        fluidRow(
          column(width = 9),
          # tabPanel(
            # style = "padding-top:15px",
            # title = paste0("plot_tab"), status = "primary", solidHeader = TRUE, collapsible = TRUE,
            # shinycustomloader::withLoader(plotOutput(paste0("selected_row_plot"),
            #                                          height = "350px", width = "100%"),
            #                               type = 'html',loader = 'loader2')
          # )
          shinycssloaders::withSpinner(plotOutput(paste0("selected_row_plot"),height = "350px", width = "100%")),
          
        ),
        fluidRow(
          column(width = 9),
          # fluidRow(
          #   column(width = 4.5),
          #   shinycssloaders::withSpinner(plotOutput(paste0("selected_row_plot_pfs")))
          # ),
          # fluidRow(
          #   column(width = 4.5),
          #   shinycssloaders::withSpinner(plotOutput(paste0("selected_row_plot_os")))
          # )
          box(
            title = "Meta Analysis for PFS",height = '80%', status = "primary", solidHeader = FALSE, collapsible = FALSE,
            shinycssloaders::withSpinner(plotOutput(paste0("selected_row_plot_pfs")))
          ),
          box(
            title = "Meta Analysis for OS",height = '80%',  status = "primary", solidHeader = FALSE, collapsible = FALSE,
            shinycssloaders::withSpinner(plotOutput(paste0("selected_row_plot_os")))
          )
        )
      )
      
    })
    # 单元格点击展示
    # observeEvent(input$iciexplore_pancancer_table_cells_selected, {
    #   req(input$iciexplore_pancancer_table_cells_selected)
    #   showModal(modalDialog(
    #     title = "message",
    #     plotOutput(paste0("selected_row_plot")),
    #     easyClose = TRUE,
    #     footer = NULL))
    # })
    
    # 遍历队列
    ## 获取前端输入
    cancer_detail_in <- input$iciexplore_cancer_id
    info(file_logger, paste0(cat_prefix_ici,"-选择的瘤种: ", cancer_detail_in))
    cancer_study_in <- input$iciexplore_study_id
    info(file_logger, paste0(cat_prefix_ici,"-选择的队列: ", cancer_study_in))
    symbol <- input$iciexplore_symbol_id
    info(file_logger, paste0(cat_prefix_ici,"-选择的基因: ", symbol))
    ici_logical_type <- input$ici_logical
    info(file_logger, paste0(cat_prefix_ici,"-选择的多基因突变逻辑关系: ", ici_logical_type))
    # 获取队列名称
    input.genes <- symbol
    # input.study.df <- icicohort.df %>%
    #   dplyr::filter(tumor_detail == cancer_detail_in)
    ici.cancer.list <- ici.cohort.list[[cancer_detail_in]]
    n <- length(unique(cancer_study_in))
    study.out <- do.call(rbind,lapply(1:n, function(i) {
      # browser()
      # sid = 'Allen_2015 (WES)'
      sid <- unique(cancer_study_in)[i]
      
      ici.cli.df <- ici.cancer.list[['clinical']][[sid]]
      ici.mut.df <- ici.cancer.list[['mutation']][[sid]]
      
      update_modal_progress(
        value = i / n,
        text =  paste("Analyzing cohort:", sid,sprintf("(%1.0f%%)", i/n*100)) 
      )
      # browser()
      # 过滤样本：选择该瘤种的样本的临床信息和突变数据
      tumor.samples <- intersect(ici.cli.df$sample_id,unique(ici.mut.df$sample_id))
      tumor.cli.df <- ici.cli.df %>%
        dplyr::filter(sample_id %in% tumor.samples,      # 同时有突变和临床信息
                      cancer_detail == cancer_detail_in, # 筛选该瘤种的数据
                      ici_treatment == 'Yes'             # 接受ICI治疗的人群
        )
      tumor.mut.df <- ici.mut.df %>%
        dplyr::filter(sample_id %in% tumor.cli.df$sample_id)
      
      # 遍历基因及其组合（2/All）
      # combn.types <- c('Single','Compound','All')
      # 使用combn函数列出所有的组合情况
      genes.list = c()
      if(length(input.genes) >= 1){
        # combn.n 表示每个组合的基因数目
        combn.n <- c()
        for(i in 1:length(input.genes)){
          # 输入基因小于等于10个时：三种形式
          if(length(input.genes) <= 10){
            if(i %in% c(1,2,length(input.genes))){
              combn.n = append(combn.n,i)
            }
          }else{
            # 输入基因大于10个时：两种形式
            if(i %in% c(1,length(input.genes))){
              combn.n = append(combn.n,i)
            }
          }
          
        }
        # 遍历生成所有组合（待修改：只遍历1，2，All三种形式）
        for(i in combn.n){
          if(length(input.genes) >= i){
            grp.df <- as.data.frame(t(combn(input.genes, i) )) 
            genes.list <- append(genes.list,
                                 apply(grp.df,1,function(x) paste(x,collapse = '#')))
          }
        }
      } 
      # 遍历生成的基因组合
      gene.out <- do.call(rbind,lapply(genes.list, function(gname){
        # browser()
        middle.plot.list <- list() # 新建一个空list，用于存贮图
        # sid = 'Samstein_2018'
        # cancer_detail_in = 'Melanoma'
        # gname = 'NTRK2'
        cat(cat_prefix_ici,"-分析队列: ",sid,", 瘤种: ",cancer_detail_in,", 基因: ",gname,"\n")
        info(file_logger, paste0(cat_prefix_ici,"-分析队列: ",sid,", 瘤种: ",cancer_detail_in,", 基因: ",gname))
        
        var.gene <- unlist(strsplit(gname,"#"))
        # browser()
        # 筛选出突变样本
        if(!grepl("#",gname)){
          # 单个基因时，不考虑逻辑与/或
          tmp.mut.df <- tumor.mut.df %>%
            dplyr::filter(hugo_symbol %in% var.gene) %>%
            dplyr::distinct(sample_id,.keep_all = T)
        }else{
          # 多个基因时
          # browser()
          if(ici_logical_type == 'AND'){
            # logical 为 AND 时
            cat(cat_prefix_ici,"-逻辑与选择多基因突变模式！\n")
            info(file_logger, paste0(cat_prefix_ici,"-逻辑与选择多基因突变模式!"))
            
            tmp.mut.samples <- tumor.mut.df %>%
              dplyr::filter(hugo_symbol %in% var.gene) %>%
              dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
              dplyr::group_by(sample_id) %>%
              dplyr::summarise(sample_count = n()) %>%
              dplyr::ungroup() %>%
              dplyr::filter(sample_count == length(var.gene)) # 取
          }
          if(ici_logical_type == 'OR'){
            # logical 为 OR 时
            info(file_logger, paste0(cat_prefix_ici,"- 逻辑或选择多基因突变模式!"))
            tmp.mut.samples <- tumor.mut.df %>%
              dplyr::filter(hugo_symbol %in% var.gene) %>%
              dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
              dplyr::group_by(sample_id) %>%
              dplyr::summarise(sample_count = n()) %>%
              dplyr::ungroup() %>%
              dplyr::filter(sample_count >= 1) # 取
          }
          
          tmp.mut.df <- tumor.mut.df %>%
            dplyr::filter(hugo_symbol %in% var.gene) %>%
            dplyr::filter(sample_id %in% tmp.mut.samples$sample_id) %>%
            dplyr::distinct(sample_id,.keep_all = T)
        }
        
        # 初始化输出对象
        cox.df.pfs <- data.frame('PFS_m_mut' = NA,'PFS_m_wt' = NA,
                                 # 'PFS_Coef' = NA,
                                 'PFS_HR' = NA,'PFS_Lower' = NA,'PFS_Upper' = NA,
                                 "PFS_HR95CI" = '',"PFS_Cox_P" = NA,"PFS_Logrank_P" = NA) 
        cox.df.os <- data.frame('OS_m_mut' = NA,'OS_m_wt' = NA,
                                # 'OS_Coef' = NA,
                                'OS_HR' = NA,'OS_Lower' = NA,'OS_Upper' = NA,
                                "OS_HR95CI" = '',"OS_Cox_P" = NA,"OS_Logrank_P" = NA) 
        tmb.res.df <-  data.frame("TMB_p" = NA,"TMB_m_mut" = NA,"TMB_m_wt" = NA)
        neo.res.df <-  data.frame("Neoantigen_p" = NA,"Neoantigen_m_mut" = NA,"Neoantigen_m_wt"  = NA)
        pdl1exp.res.df <-  data.frame("PDL1exp_p" = NA,"PDL1_m_mut" = NA,"PDL1_m_wt"  = NA)
        tp.res.df <-  data.frame("Tumor_Purity_p" = NA,"Tumor_Purity_m_mut" = NA,"Tumor_Purity_m_wt"  = NA)
        pdl1.res.df =  data.frame("PDL1_P.method" = NA,"PDL1_P" = NA)
        orr.res.df  = data.frame("ORR_P.method" = NA,"ORR_P" = NA)
        response.res.df = data.frame("Resp_P.method" = NA,"Resp_P" = NA)
        if(nrow(tmp.mut.df) == 0){
          info(file_logger, paste0(cat_prefix_ici,"-未检测到",gname,"突变样本!"))
          base.info.df <- data.frame(Cohort = sid,
                                     Cancer = cancer_detail_in,
                                     Gene = gname,
                                     Desciption = "All WT",
                                     NoOfPts = nrow(tumor.cli.df),
                                     NoOfMut = 0)
        }else{
          plot.cli <- tumor.cli.df %>%
            dplyr::mutate(is_mut = ifelse(sample_id %in% tmp.mut.df$sample_id,'Mut','WT')) 
          
          if(length(unique(plot.cli$is_mut)) == 1){
            # 全部为突变
            info(file_logger, paste0(cat_prefix_ici,"-全部为",unique(plot.cli$is_mut),"样本!"))
            base.info.df <- data.frame(Cohort = sid,
                                       Cancer = cancer_detail_in,
                                       Gene = gname,
                                       Desciption = paste0("All ",unique(plot.cli$is_mut)),
                                       NoOfPts = nrow(plot.cli),
                                       NoOfMut = nrow(plot.cli))
          }else{
            
            ici.plot.cli <- plot.cli %>%
              dplyr::mutate(is_mut = factor(is_mut,levels = c('WT','Mut'))) %>%
              dplyr::rename(type = is_mut)
            # Cox/Log-rank分析
            {
              # Extract data 
              univ_cox <- function(x.data){ 
                # x.data <- tmp.dat
                x.data$times <- as.numeric(x.data$times)
                x.data$status <- as.numeric(x.data$status)
                # 单因素Cox分析
                x.cox <- coxph(Surv(times, status)~ type, data = x.data)
                x.sum <- summary(x.cox)
                Cox.p <- signif(as.numeric(x.sum$wald["pvalue"]),2)    # 科学计数法保持两位小数,
                Logrank.p <- signif(as.numeric(x.sum$sc["pvalue"]),2)  # sc表示输出logrank检验结果 
                Coef <- round(x.sum$coef[1], 2);  #coeficient beta
                HR   <- round(x.sum$coef[2], 2);    #exp(beta) 
                L95  <- round(x.sum$conf.int[,"lower .95"], 2)
                H95  <- round(x.sum$conf.int[,"upper .95"],2)
                HR95CI <- paste0(HR, " (", L95, "-", H95, ")")
                
                # 中位生存时间
                x.sfit <- survfit(Surv(times,status) ~ type,data = x.data)
                x.mtable <- surv_median(x.sfit) # 求中位生存时间
                x.mtable <- x.mtable %>%
                  dplyr::select(strata,Mid = median) %>%
                  dplyr::mutate(strata = substr(strata,6,nchar(strata)),
                                Mid = round(Mid,2))
                if(nrow(x.mtable) == 2){
                  median.mut <- x.mtable[which(x.mtable$strata == 'Mut'),]$Mid
                  median.wt <- x.mtable[which(x.mtable$strata == 'WT'),]$Mid
                }else{
                  median.mut <- NA
                  median.wt <- x.mtable$Mid
                }
                
                
                tmp.res <- c(median.mut,median.wt,
                             # Coef,
                             HR,L95,H95,
                             HR95CI,Cox.p,Logrank.p)
                names(tmp.res)<-c("m_mut","m_wt",
                                  # 'Coef',
                                  'HR','Lower','Upper',
                                  "HR95CI","Cox_P","Logrank_P")
                return(tmp.res)
              }
              
              # PFS
              if("pfs_months" %in% colnames(ici.plot.cli)){
                tmp.dat <- ici.plot.cli %>%
                  dplyr::select(sample_id,times = pfs_months,status = pfs_status,type) %>%
                  dplyr::filter(!is.na(times),!is.na(status)) %>%
                  dplyr::arrange(type)
                if(length(unique(tmp.dat$type)) == 1){
                  info(file_logger, paste0(cat_prefix_ici,"-全部有PFS的样本为",unique(tmp.dat$type),"类型，不执行Cox分析!"))
                  km.graphic <- NULL
                }else{
                  cox.df.pfs <- as.data.frame(t(as.data.frame(univ_cox(tmp.dat),check.names = FALSE)))
                  colnames(cox.df.pfs) <- paste0('PFS_',colnames(cox.df.pfs))
                  
                  #绘图
                  km.graphic <- tmp.dat %>%
                    dplyr::rename(risk = type) %>%
                    func.survivalplot('',"PFS",
                                      time.break = time_break_survival(max(tmp.dat$times,na.rm = T)),
                                      tbl.loc = 'topright',
                                      color.dis = c("#2196F3","#FF9800"),
                                      y.title =  'Progression-free Survival')
                }
                middle.graphic[[paste0(sid,"_",gname)]]$pfs <- km.graphic
                middle.plot.list[['pfs']] <- ggarrange(km.graphic$plot, km.graphic$table, heights = c(2, 0.7),
                                                       ncol = 1, nrow = 2, align = "v")   
                
                
              }
              # OS
              if("os_months" %in% colnames(ici.plot.cli)){
                tmp.dat <- ici.plot.cli %>%
                  dplyr::select(sample_id,times = os_months,status = os_status,type) %>%
                  dplyr::filter(!is.na(times),!is.na(status)) %>%
                  dplyr::mutate(times = as.numeric(times),
                                status = as.numeric(status)) %>%
                  dplyr::arrange(type)
                if(length(unique(tmp.dat$type)) == 1){
                  info(file_logger, paste0(cat_prefix_ici,"-全部有OS的样本为",unique(tmp.dat$type),"类型，不执行Cox分析!"))
                  km.graphic <- NULL
                }else{
                  cox.df.os <- as.data.frame(t(as.data.frame(univ_cox(tmp.dat),check.names = FALSE)))
                  colnames(cox.df.os) <- paste0('OS_',colnames(cox.df.os))
                  
                  #绘图
                  km.graphic <- tmp.dat %>%
                    dplyr::rename(risk = type) %>%
                    func.survivalplot('',"OS",
                                      time.break = time_break_survival(max(tmp.dat$times,na.rm = T)),
                                      tbl.loc = 'topright',
                                      color.dis = c("#2196F3","#FF9800"), # color1 = "#2196F3",color2 = "#FF9800",
                                      y.title =  'Overall Survival')
                }
                middle.graphic[[paste0(sid,"_",gname)]]$os <- km.graphic
                middle.plot.list[['os']] <- ggarrange(km.graphic$plot, km.graphic$table, heights = c(2, 0.7),
                                                      ncol = 1, nrow = 2, align = "v")   
                
                
              }
            }
            # 临床指标分析:秩和检验
            con.wilcox <- function(dat,con.index){
              fml <- as.formula(paste0(con.index," ~ type")) 
              wilcox.result <- wilcox.test(fml,data = dat, alternative= "two.sided")
              p <- wilcox.result$p.value
              p <- ifelse(p <= 0.001,round(p,4),0.001)
              return(p)
            }
            # 临床指标分析:卡方检验
            cat.chisqtest <- function(chi.data,clinical.factor){
              
              in.df <- chi.data %>%
                dplyr::select(sample_id,type,group = clinical.factor) %>%
                dplyr::filter(!is.na(group))
              # 卡方检验
              x.table <- xtabs(~ type + group, data = in.df) # 构建列联表，进行四格表检验
              chisq.res <- chisq.test(x.table,correct = T)  # correct = T 表示Yates校正
              chisq.res$statistic
              vcd::assocstats(x.table)
              chisq.res.exp <- as.data.frame(chisq.res$expected) %>%
                tibble::rownames_to_column(var = 'test')%>%
                tidyr::gather(type,value,-test)
              
              # 所有理论数T≥5
              T5 <- as.data.frame(apply(chisq.res$expected, 2, function(x) x>=5)) %>%
                tibble::rownames_to_column(var = 'test')%>%
                tidyr::gather(type,value,-test)  
              # 所有理论数5>T≥1
              T1 <- as.data.frame(apply(chisq.res$expected, 2, function(x) x>=1 & x<5))%>%
                tibble::rownames_to_column(var = 'test')%>%
                tidyr::gather(type,value,-test) 
              
              if(sum(as.numeric(chisq.res.exp$value)) >= 40 & !(FALSE %in% as.logical(T5$value))){
                p.res <- chisq.test(x.table)$p.value
                p.method <- c('Pearson Chisq')
                # cat("卡方检验: ",clinical.factor,",所有理论数T≥5并且总样本量n≥40,用Pearson卡方进行检验:",p.res,"\n")
              }else if(sum(as.numeric(chisq.res.exp$value)) >= 40 & !(FALSE %in% as.logical(T5$value))){
                p.res <- chisq.test(x.table,correct = T)$p.value
                p.method <- c('Yates Chisq')
                # cat("卡方检验: ",clinical.factor,",所有理论数5>T≥1,并且n≥40,用连续性校正的卡方进行检验:",p.res,"\n")
              }else{
                p.res <- fisher.test(x.table)$p.value
                p.method <- c('Fisher')
                # cat("卡方检验: ",clinical.factor,",有理论数T＜1或n＜40,则用Fisher’s检验:",p.res,"\n")
              }
              p.df <- data.frame("P.method" = p.method,
                                 "P.value" = round(p.res,4))
              return(p.df)
            }
            
            # 20220720：新增批量处理方法，暂未执行
            # for(var in c("tmb",'neoantigen','pdl1_exp','tumor_purity')){
            #   tmp.res <- func.clinical_wilcoxon_test(var,plot.cli)
            #   if(length(tmp.res) ==0){
            #     cat("队列",sid,"不存在",var,"临床属性\n")
            #   }else{
            #     middle.table <- tmp.res$table
            #     middle.graphic[[paste0(sid,"_",gname)]][[paste0("box_",var)]] <- tmp.res$plot
            #     middle.plot.list[[var]] <- tmp.res$plot
            #   }
            # }
            
            # TMB 分析
            if(("tmb" %in% colnames(ici.plot.cli))){
              # browser()
              ici.plot.cli$tmb <- as.numeric(ici.plot.cli$tmb)
              ici.plot.cli$type <- as.character(ici.plot.cli$type)
              
              temp.plot.cli <- ici.plot.cli %>%
                dplyr::filter(!is.na(tmb))
              if(length(unique(temp.plot.cli$type)) == 2){
                tmp.p <- con.wilcox(temp.plot.cli,'tmb')
                median.tmb.mut = median(temp.plot.cli[which(temp.plot.cli$type == 'Mut'),]$tmb,na.rm = T)
              }else{
                tmp.p <- NA
                median.tmb.mut <- NA
              }
              median.tmb.wt = median(temp.plot.cli[which(temp.plot.cli$type == 'WT'),]$tmb,na.rm = T)
              
              tmb.res.df <-  data.frame("TMB_p" = tmp.p,
                                        "TMB_m_mut" = median.tmb.mut,
                                        "TMB_m_wt" = median.tmb.wt)
              
              #绘图
              km.graphic <- temp.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = tmb) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color2 = "#FF9800",color1 = "#2196F3",ytitle = 'TMB')
              middle.graphic[[paste0(sid,"_",gname)]]$box_tmb <- km.graphic
              middle.plot.list[['tmb']] <- km.graphic
            }
            # Neoantigen 分析
            if(("neoantigen" %in% colnames(ici.plot.cli))){
              ici.plot.cli$neoantigen <- as.numeric(ici.plot.cli$neoantigen)
              ici.plot.cli$type <- as.character(ici.plot.cli$type)
              
              temp.plot.cli <- ici.plot.cli %>%
                dplyr::filter(!is.na(neoantigen))
              if(length(unique(temp.plot.cli$type)) == 2){
                neo.p <- con.wilcox(temp.plot.cli,'neoantigen')
                median.neo.mut = median(temp.plot.cli[which(temp.plot.cli$type == 'Mut'),]$neoantigen,na.rm = T)
              }else{
                neo.p <- NA
                median.neo.mut <- NA
              }
              median.neo.wt = median(temp.plot.cli[which(temp.plot.cli$type == 'WT'),]$neoantigen,na.rm = T)
              neo.res.df <-  data.frame("Neoantigen_p" = neo.p,
                                        "Neoantigen_m_mut" = median.neo.mut,
                                        "Neoantigen_m_wt"  = median.neo.wt)
              
              #绘图
              km.graphic <- temp.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = neoantigen) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color2 = "#FF9800",color1 = "#2196F3",ytitle = 'Neoantigen')
              middle.graphic[[paste0(sid,"_",gname)]]$box_neoantigen <- km.graphic
              middle.plot.list[['neoantigen']] <- km.graphic
            }
            # PD_L1_Exp 分析
            if(("pdl1_exp" %in% colnames(ici.plot.cli))){
              ici.plot.cli$pdl1_exp <- as.numeric(ici.plot.cli$pdl1_exp)
              ici.plot.cli$type <- as.character(ici.plot.cli$type)
              
              temp.plot.cli <- ici.plot.cli %>%
                dplyr::filter(!is.na(pdl1_exp))
              if(length(unique(temp.plot.cli$type)) == 2){
                pdl1.p <- con.wilcox(temp.plot.cli,'pdl1_exp')
                median.pdl1.mut = median(temp.plot.cli[which(temp.plot.cli$type == 'Mut'),]$pdl1_exp,na.rm = T)
              }else{
                pdl1.p <- NA
                median.pdl1.mut <- NA
              }
              
              median.pdl1.wt = median(temp.plot.cli[which(temp.plot.cli$type == 'WT'),]$pdl1_exp,na.rm = T)
              pdl1exp.res.df <-  data.frame("PDL1exp_p" = pdl1.p,
                                            "PDL1_m_mut" = median.pdl1.mut,
                                            "PDL1_m_wt"  = median.pdl1.wt)
              #绘图
              km.graphic <- temp.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = pdl1_exp) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color2 = "#FF9800",color1 = "#2196F3",ytitle = 'PD-L1 Expression')
              middle.graphic[[paste0(sid,"_",gname)]]$box_pdl1_exp <- km.graphic
              middle.plot.list[['pdl1_exp']] <- km.graphic
            }
            # tumor_purity 分析
            if(("tumor_purity" %in% colnames(ici.plot.cli))){
              # browser()
              ici.plot.cli$tumor_purity <- as.numeric(ici.plot.cli$tumor_purity)
              ici.plot.cli$type <- as.character(ici.plot.cli$type)
              
              temp.plot.cli <- ici.plot.cli %>%
                dplyr::filter(!is.na(tumor_purity))
              if(length(unique(temp.plot.cli$type)) == 2){
                tp.p <- con.wilcox(temp.plot.cli,'tumor_purity')
                median.tp.mut = median(temp.plot.cli[which(temp.plot.cli$type == 'Mut'),]$tumor_purity,na.rm = T)
              }else{
                tp.p <- NA
                median.tp.mut <- NA
              }
              median.tp.wt = median(temp.plot.cli[which(temp.plot.cli$type == 'WT'),]$tumor_purity,na.rm = T)
              tp.res.df <-  data.frame("Tumor_Purity_p" = tp.p,
                                       "Tumor_Purity_m_mut" = median.tp.mut,
                                       "Tumor_Purity_m_wt"  = median.tp.wt)
              km.graphic <- temp.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = tumor_purity) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color2 = "#FF9800",color1 = "#2196F3",ytitle = 'Tumor Purity')
              
              middle.graphic[[paste0(sid,"_",gname)]]$box_tumor_purity <- km.graphic
              middle.plot.list[['tumor_purity']] <- km.graphic
            }
            
            pdl1.res.df =  data.frame("PDL1_P.method" = NA,"PDL1_P" = NA)
            orr.res.df  = data.frame("ORR_P.method" = NA,"ORR_P" = NA)
            response.res.df = data.frame("Resp_P.method" = NA,"Resp_P" = NA)
            
            if("pdl1" %in% colnames(ici.plot.cli)) {
              temp.plot.cli <- ici.plot.cli %>%
                dplyr::filter(!is.na(pdl1))
              if(length(unique(temp.plot.cli$type)) == 2){
                pdl1.res.df <- cat.chisqtest(temp.plot.cli,'pdl1')
              }else{
                pdl1.res.df <- data.frame("P.method" = '',
                                          "P.value" = NA)
              }
              colnames(pdl1.res.df) <- c('PDL1_P.method','PDL1_P')
            }
            if("orr" %in% colnames(ici.plot.cli)) {
              temp.plot.cli <- ici.plot.cli %>%
                dplyr::filter(!is.na(orr))
              if(length(unique(temp.plot.cli$type)) == 2){
                orr.res.df <- cat.chisqtest(temp.plot.cli,'orr')
              }else{
                orr.res.df <- data.frame("P.method" = '',
                                         "P.value" = NA)
              }
              
              colnames(orr.res.df) <- c('ORR_P.method','ORR_P')
            }
            if("clinical_benefit" %in% colnames(ici.plot.cli)) {
              temp.plot.cli <- ici.plot.cli %>%
                dplyr::filter(!is.na(clinical_benefit))
              if(length(unique(temp.plot.cli$type)) == 2){
                response.res.df <- cat.chisqtest(temp.plot.cli,'clinical_benefit')
              }else{
                response.res.df <- data.frame("P.method" = '',
                                              "P.value" = NA)
              }
              colnames(response.res.df) <- c('Resp_P.method','Resp_P')
            }
            
            base.info.df <- data.frame(Cohort = sid,
                                       Cancer = cancer_detail_in,
                                       Gene = gname,
                                       Desciption = "MUT/WT",
                                       NoOfPts = nrow(ici.plot.cli),
                                       NoOfMut = nrow(ici.plot.cli[which(ici.plot.cli$type == 'Mut'),]))
            
            
          }
        }
        # 合并结果
        info(file_logger, paste0(cat_prefix_ici,"-存在突变且分析执行完成!"))
        if(length(middle.plot.list) > 0){
          # for(i in seq(1,length(middle.plot.list))){
          #   info(file_logger, paste0(cat_prefix_ici,"-middle.plot.list[",i,"]名称:",names(middle.plot.list[i])))
          #   pdf(paste0("./logs/ici_pred_",gname,"_",names(middle.plot.list[i]),".pdf"),onefile = F)
          #   print(middle.plot.list[[names(middle.plot.list[i])]])
          #   dev.off()
          # }
          middle.plot <- ggarrange(plotlist = middle.plot.list,nrow = 1)
          output.graphic[[paste0(sid,"_", gname)]]$plot <- middle.plot
        }
        
        #合并为一行结果
        tmp.row.res.df <- cbind(base.info.df,
                                cox.df.pfs,
                                cox.df.os,
                                tmb.res.df,
                                neo.res.df,
                                pdl1exp.res.df,
                                tp.res.df,
                                pdl1.res.df,
                                orr.res.df,
                                response.res.df)
        info(file_logger, paste0(cat_prefix_ici,"-tmp.row.res.df整合完成!"))
        # browser()
        return(tmp.row.res.df)
      }))  # 遍历基因结束
      
      info(file_logger, paste0(cat_prefix_ici,"-基因组合结果分析完成!"))
      return(gene.out)
    }))    # 遍历队列结束
    info(file_logger, paste0(cat_prefix_ici,"-遍历队列结束!"))
    
    # meta 分析
    {
      info(file_logger, paste0(cat_prefix_ici,"-开始meta分析!"))
      
      meta.plot.list <- list()
      meta.in <- study.out
      lapply(unique(meta.in$Gene), function(gene_grp){
        
        # 选出基因/组合的结果数据，过滤掉无突变的队列
        tmp.meta.ia <- meta.in %>%
          dplyr::filter(Gene == gene_grp) %>%
          dplyr::filter(NoOfMut != 0)
        # 如果队列数大于0，则进行meta分析
        if(nrow(tmp.meta.ia) > 0){
          # PFS
          tmp.meta.ib.pfs <- tmp.meta.ia %>%
            dplyr::select(Cohort,Cancer,HR = PFS_HR,L95 = PFS_Lower,U95 = PFS_Upper) %>%
            dplyr::filter(!is.na(HR),!is.na(L95),!is.na(U95)) %>%
            dplyr::mutate(HR = as.numeric(HR),L95 = as.numeric(L95),U95 = as.numeric(U95))
          if(nrow(tmp.meta.ib.pfs) > 0){
            clc.meta <- tmp.meta.ib.pfs %>%
              dplyr::mutate(lghr = log(HR),
                            lglci = log(L95),lghci = log(U95)) %>%
              dplyr::mutate(selghr = (lghci - lglci)/(2*1.96)) 
            
            settings.meta('revman5')
            meta.res.pfs <- metagen(TE = lghr,seTE = selghr,studlab = clc.meta$Cohort,data = clc.meta,sm = 'HR',
                                    text.w.fixed = "w.fixed",
                                    text.w.random = 'w.random',
                                    # 确定方法之后，可以将不使用的方法设置为F，不输出图
                                    # 异质性检验，p<0.05或I2>50%，认为各研究存在异质性，选用随机效应模型；P>0.05且I2<50%,不能认为各研究存在异质性，选用固定效应模型
                                    comb.fixed = F,comb.random = T,
                                    prediction = F,
                                    overall.hetstat = F)
            meta.plot.list[[paste0(gene_grp,'_pfs')]] <- clc.meta
            output.metalist[[paste0(gene_grp,'_pfs')]]$data <- clc.meta
          }else{
            meta.plot.list[[paste0(gene_grp,'_pfs')]] <- NULL
            output.metalist[[paste0(gene_grp,'_pfs')]]$data <- NULL
          }
          # OS
          tmp.meta.ib.os <- tmp.meta.ia %>%
            dplyr::select(Cohort,Cancer,HR = OS_HR,L95 = OS_Lower,U95 = OS_Upper) %>%
            dplyr::filter(!is.na(HR),!is.na(L95),!is.na(U95)) %>%
            dplyr::mutate(HR = as.numeric(HR),L95 = as.numeric(L95),U95 = as.numeric(U95))
          if(nrow(tmp.meta.ib.os) > 0){
            clc.meta <- tmp.meta.ib.os %>%
              dplyr::mutate(lghr = log(HR),
                            lglci = log(L95),lghci = log(U95)) %>%
              dplyr::mutate(selghr = (lghci - lglci)/(2*1.96)) 
            settings.meta('revman5')
            meta.res.os <- metagen(TE = lghr,seTE = selghr,studlab = clc.meta$Cohort,data = clc.meta,sm = 'HR',
                                   text.w.fixed = "w.fixed",
                                   text.w.random = 'w.random',
                                   # 确定方法之后，可以将不使用的方法设置为F，不输出图
                                   # 异质性检验，p<0.05或I2>50%，认为各研究存在异质性，选用随机效应模型；P>0.05且I2<50%,不能认为各研究存在异质性，选用固定效应模型
                                   comb.fixed = F,comb.random = T,
                                   prediction = F,
                                   overall.hetstat = F)
            meta.plot.list[[paste0(gene_grp,'_os')]] <- clc.meta
            output.metalist[[paste0(gene_grp,'_os')]]$data <- clc.meta
          }else{
            meta.plot.list[[paste0(gene_grp,'_os')]] <- NULL
            output.metalist[[paste0(gene_grp,'_os')]]$data <- NULL
          }
          
        }else{
          meta.plot.list[[paste0(gene_grp,'_pfs')]] <- NULL
          meta.plot.list[[paste0(gene_grp,'_os')]] <- NULL
          output.metalist[[paste0(gene_grp,'_pfs')]]$data <- NULL
          output.metalist[[paste0(gene_grp,'_os')]]$data <- NULL
        }
        
      })
      info(file_logger, paste0(cat_prefix_ici,"-完成meta分析!"))
      
    }
    # browser()
    info(file_logger, paste0(cat_prefix_ici,"-整理分析结果输出表!"))
    
    display.out <- study.out %>%
      dplyr::select(-PFS_HR,-PFS_Lower,-PFS_Upper,
                    -OS_HR,-OS_Lower,-OS_Upper,
                    -PDL1_P.method,-ORR_P.method,-Resp_P.method)
    
    sketch_pred = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Cohort'),
          th(rowspan = 2, 'Cancer Type'),
          th(rowspan = 2, 'Gene Symbol'),
          th(rowspan = 2, 'Description'),
          th(rowspan = 2, 'Sample Size'),
          th(rowspan = 2, 'Mutant Sample Size'),
          th(colspan = 5, 'PFS'),
          th(colspan = 5, 'OS'),
          th(colspan = 3, 'TMB'),
          th(colspan = 3, 'TNB'),
          th(colspan = 3, 'PD-L1 expression'),
          th(colspan = 3, 'Tumor purity(TP)'),
          th(colspan = 1, 'PD-L1(cat)'),
          th(colspan = 1, 'ORR'),
          th(colspan = 1, 'Response')
          
        ),
        tr(
          lapply(c("mPFS(mut)","mPFS(wt)","HR(95%CI)","P value (Cox)","P value (Logrank)",
                   "mOS(mut)","mOS(wt)","HR(95%CI)","P value (Cox)","P value (Logrank)",
                   "P value","mTMB(mut)","mTMB(wt)",
                   "P value","mTNB(mut)","mTNB(wt)",
                   "P value","mPDL1(mut)","mPDL1(wt)",
                   "P value","mTP(mut)","mTP(wt)",
                   "P value","P value","P value"), th)
        )
      )
    ))
    
    output[[paste0('pancancer', "_table")]] <- renderDataTable({
      datatable(display.out, 
                container = sketch_pred,
                style = 'bootstrap', rownames = FALSE,
                selection = list(mode = "single", target = "row"), # target = "cell"：选择单元格
                options = list(scrollX = TRUE, keys = TRUE,
                               paging = TRUE,   # 分页
                               orderClasses = TRUE,dom = "Bfrtip", # 'tp', 
                               language = list(
                  zeroRecords = "No records found matching your selection"),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")))
                )
    })
    
    output[[paste0('pancancer', "_table_dl")]] <- downloadHandler(
      filename = function() {
        paste0(prefix_output_file,"_PredExplore_Result_",cancer_detail_in,"_",Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        readr::write_delim(x = display.out, path = file, delim = "\t")
      }
    )

    output[["selected_row_plot"]] <- renderPlot({
      study.selected.df <- display.out[selected.row(),]
      plot.clinicals <- names(middle.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]])
      info(file_logger, paste0(cat_prefix_ici,"-临床指标:",plot.clinicals))
      output.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]]$plot
    })
    output[["selected_row_plot_pfs"]] <- renderImage({
      
      study.selected.df <- display.out[selected.row(),]
      if(!is.null(output.metalist[[paste0(study.selected.df$Gene[1],"_pfs")]]$data)){
        cat("There have meta plot for pfs\n")
        # clc.meta <- meta.plot.list[[paste0(study.selected.df$Gene[1],"_pfs")]]
        clc.meta <- output.metalist[[paste0(study.selected.df$Gene[1],"_pfs")]]$data
        settings.meta('revman5')
        meta.res.pfs <- metagen(TE = lghr,seTE = selghr,studlab = clc.meta$Cohort,data = clc.meta,sm = 'HR',
                               text.w.fixed = "w.fixed",
                               text.w.random = 'w.random',
                               # 确定方法之后，可以将不使用的方法设置为F，不输出图
                               # 异质性检验，p<0.05或I2>50%，认为各研究存在异质性，选用随机效应模型；P>0.05且I2<50%,不能认为各研究存在异质性，选用固定效应模型
                               comb.fixed = F,comb.random = T,
                               prediction = F,
                               overall.hetstat = F)
        
        outfile_pfs <- tempfile(fileext='.png')
        CairoPNG(outfile_pfs,width = 600,height = 500)
        meta::forest(meta.res.pfs)
        dev.off()
        
        # Return a list containing the filename
        list(src = outfile_pfs,
             width = 600,
             # height = height,
             alt = "There is no plot for PFS")
    
      }else{
        
        p.noinfo <- ggplot2::ggplot() +
          theme_void() +
          geom_text(aes(0, 0, label = "PFS NOT APPLICABLE!"),
                    colour = "grey", fontface = "bold", size = 8
          ) +
          xlab(NULL)
        outfile_pfs <- tempfile(fileext='.png')
        CairoPNG(outfile_pfs,width = 600,height = 500)
        p.noinfo
        dev.off()
        
        # filename <- normalizePath(file.path('./www',paste('demo.not.applicable.png', sep='')))
        
        # Return a list containing the filename
        list(src = outfile_pfs,
             alt = "PFS data is not available")
      }
    },deleteFile = TRUE)
    output[["selected_row_plot_os"]] <- renderImage({
      
      study.selected.df <- display.out[selected.row(),]
      if(!is.null(output.metalist[[paste0(study.selected.df$Gene[1],"_os")]]$data)){
        cat("There have meta plot for os\n")
        # clc.meta <- meta.plot.list[[paste0(study.selected.df$Gene[1],"_os")]]
        clc.meta <- output.metalist[[paste0(study.selected.df$Gene[1],"_os")]]$data
        settings.meta('revman5')
        meta.res.os <- metagen(TE = lghr,seTE = selghr,studlab = clc.meta$Cohort,data = clc.meta,sm = 'HR',
                                text.w.fixed = "w.fixed",
                               text.w.random = 'w.random',
                               # 确定方法之后，可以将不使用的方法设置为F，不输出图
                               # 异质性检验，p<0.05或I2>50%，认为各研究存在异质性，选用随机效应模型；P>0.05且I2<50%,不能认为各研究存在异质性，选用固定效应模型
                               comb.fixed = F,comb.random = T,
                                prediction = F,
                                overall.hetstat = F)
        outfile_os <- tempfile(fileext='.png')
        CairoPNG(outfile_os,width = 600,height = 500)
        meta::forest(meta.res.os)
        dev.off()
        
        # Return a list containing the filename
        list(src = outfile_os,
             width = 600,
             # height = 500,
             alt = "OS data is not available")
      }else{
        # filename <- normalizePath(file.path('./www',paste('demo.not.applicable.png', sep='')))
        p.noinfo <- ggplot2::ggplot() +
          theme_void() +
          geom_text(aes(0, 0, label = "OS NOT APPLICABLE!"),
                    colour = "grey", fontface = "bold", size = 8
          ) +
          xlab(NULL)
        outfile_os <- tempfile(fileext='.png')
        CairoPNG(outfile_os,width = 600,height = 500)
        p.noinfo
        dev.off()
        # Return a list containing the filename
        list(src = outfile_os,
             alt = "There is no plot for OS")
        # p.noinfo
      }
    },deleteFile = TRUE)
    output[[paste0('selected_row_plot_dl')]] <- downloadHandler(
      filename = function() {
        paste0(prefix_output_file,"_PredExplore_Plot_",cancer_detail_in,"_",Sys.Date(),".pdf", sep = "")
      },
      content = function(file) {
        study.selected.df <- display.out[selected.row(),]
        pdf(file,onefile = F, height = 4,width = 12, pointsize = 10)
        print(output.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]]$plot)
        dev.off()
      }
    )
    remove_modal_progress()
  })
  # remove_modal_progress()
  
}
