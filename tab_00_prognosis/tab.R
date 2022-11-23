#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-18
# @IDE     : RStudio
# @Desc    : 第0个Tab功能：预后预测因子探索
#===============================================================================

source("./r/func.survivalplot.R")
source("./r/func.ggboxplot.R")

# 预后：初始化非ICI治疗突变队列 ----
# time-cost and mem-cost!!!
{
  # 步骤：1. 选择瘤种 --> 2. 选择基因 --> 3.分析输入基因（单个/两两/全部组合）的突变与WT的生存获益，以及其他指标的差异 --> 4.表格输出结果；
  # 获取非ICI突变数据队列名称
  nonicicohort.df <- study.df %>%
    dplyr::filter(study_type == 'nonICIs',
                  grepl("Mutation",data_type))
  # 肿瘤名称
  prog.tumor.detail <- unique(nonicicohort.df$tumor_detail)
  # cat("非ICI队列涵盖的具有突变数据的瘤种: ",prog.tumor.detail,"\n")
  
  prog.cohort.list <- list()
  length(prog.cohort.list) <- length(prog.tumor.detail)
  names(prog.cohort.list) <- prog.tumor.detail
  for (t in seq(1,length(prog.tumor.detail))) {
    # t = 1
    tumor <- prog.tumor.detail[t]
    # 初始化输出list
    prog.cohort.list[[t]] <- list()
    output.list <- list()
    length(output.list) <- 3
    names(output.list)[[1]] <- 'clinical'
    names(output.list)[[2]] <- 'mutation'
    
    output.list[[1]] <- list()
    output.list[[2]] <- list()
    output.list[[3]] <- c()
    
    # 确定队列list
    # select_tumor <- input$prognostic_cancer_id
    # cat(cat_prefix_prog,"-选择的瘤种是:",select_tumor,"\n")
    nonici.study.df <- nonicicohort.df[which(nonicicohort.df$tumor_detail == tumor),] %>%
      dplyr::distinct(study_id,.keep_all = T)
    
    # 将上述队列的突变和临床数据加载到缓存
    nonici.mut.list <- list()
    nonici.cli.list <- list()
    length(nonici.mut.list) <- nrow(nonici.study.df)
    length(nonici.cli.list) <- nrow(nonici.study.df)
    
    for (i in seq(1,length(unique(nonici.study.df$study_id)))) {
      
      sid <-  unique(nonici.study.df$study_id)[i]
      
      tmp.df <- nonici.study.df %>%
        dplyr::filter(study_id == sid)
      # tmp.df是一个行数>=1的数据框
      if(grepl("Mutation",tmp.df$data_type[1])){
        
        names(nonici.cli.list)[[i]] = sid
        nonici.cli.list[[i]] <- data.frame()
        
        tmp.cli.df <- queryDataFromMySQL(paste0(tolower(sid),"_clinical"))
        if("sample_type" %in% colnames(tmp.cli.df)){
          tmp.cli.df <- tmp.cli.df %>%
            # GEO队列存在非肿瘤样本，需选择出肿瘤样本
            dplyr::filter(sample_type == 'T') 
        }
        nonici.cli.list[[i]] <- tmp.cli.df
        
        names(nonici.mut.list)[[i]] = sid
        nonici.mut.list[[i]] <- data.frame()
        
        tbl.name <- paste0(tolower(sid),"_mutation")
        tmp.mut.df <- queryDataFromMySQL(tbl.name)
        tmp.mut.df <- tmp.mut.df %>%
          dplyr::filter(sample_id %in% tmp.cli.df$sample_id)
        nonici.mut.list[[i]] <- tmp.mut.df
      }
    }
    
    output.list[[1]] <- nonici.cli.list
    output.list[[2]] <- nonici.mut.list
    
    # 将突变队列的基因symbol输出交集
    nonici.symbols.df <- do.call(rbind,lapply(seq(1,length(nonici.mut.list)), function(i){
      # 遍历所有队列获取并集
      tmp.mut.df <-  nonici.mut.list[[i]]
      
      if("hugo_symbol" %in% colnames(tmp.mut.df)){
        tmp.out <- data.frame(study_id = sid,
                              hugo_symbol = unique(tmp.mut.df$hugo_symbol))
      }else{
        tmp.out <- data.frame(study_id = sid,
                              hugo_symbol = '')
      }
      return(tmp.out)
    }))
    nonici.symbols <- unique(nonici.symbols.df$hugo_symbol)
    
    output.list[[3]] <- nonici.symbols
    names(output.list)[[3]] <- 'symbols'
    
    # remove_modal_spinner()  # 搜索结束后消除缓冲显示
    # return(output.list)
    prog.cohort.list[[t]] <- output.list
    
    cat("预后分析，瘤种 ",tumor," 的分析数据加载完成!\n")
  }
  cat("泛队列预后分析数据加载完成!\n")
  
}

tab_00_prognosis <- list()
cat_prefix_prog <- 'Prognostic'
sidebar <- sidebarPanel(
  id = 'prognostic_sidebar',
  width = 3,
  h3("Cancer Prognostic Biomarkers Exploration"),
  pickerInput(inputId = "prognostic_cancer_id",
              label = "Select one cancer type, E.g. 'LUAD'",
              choices = NULL,
              multiple = FALSE,
              selected = NULL),
  selectizeInput(inputId = "prognostic_preset_id", 
                 label = "Pre-defined gene set or user-defined list:",
                 choices = NULL, 
                 width = '100%',
                 selected = NULL,
                 multiple = F),
  selectizeInput(inputId = "prognostic_symbol_id", 
                 label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                 choices = NULL, 
                 width = '100%',
                 multiple = T, 
                 options = list(delimiter = " ", create = T,maxItems = 10,
                                placeholder = "No more than 10 genes.",
                                plugins = list('remove_button', 'drag_drop'))),
  awesomeRadio(
    inputId = "prognostic_logical",
    label = "Mutations in all(AND)/any(OR) quried genes",
    inline = T,
    choices = c("AND" ,"OR"),
    selected = "AND",
    status = "success",
    checkbox = TRUE
  ),
  # 提交按钮
  useShinyjs(),
  # actionButton(inputId = "prognostic_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "prognostic_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_prog", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
)

mainpage <- mainPanel(
  id = 'prognostic_mainpage',
  width = 9,
  uiOutput(outputId='prognostic_maintabs')
)

tab_00_prognosis$ui <- sidebarLayout(sidebar, mainpage)

tab_00_prognosis$server <- function(input, output,session) {
  cat("========================= Start Prognosis=============================\n")
  # 定义变量存储输出对象
  middle.prog.graphic <- reactiveValues()
  output.prog.graphic <- reactiveValues()
  
  observeEvent(input$reset_input_prog, {
    shinyjs::reset("prognostic_sidebar")
  })
  observeEvent(input$reset_input_prog, {
    output$prognostic_maintabs <- NULL
  })
  
  # 将获取到的肿瘤名称更新到页面
  observe({
    updatePickerInput(session = session, 
                      inputId = "prognostic_cancer_id",
                      choices = c(prog.tumor.detail), 
                      selected = NULL)
  })
  
  # 基因集定义：自选或者已有基因集合
  prog.geneset.list <- eventReactive(input$prognostic_cancer_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    output.list <- c("User-defined List",names(pre_genesets_list))
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  observe({
    # 更新基因集
    updateSelectizeInput(session = session, 
                         inputId = "prognostic_preset_id", 
                         choices = c(prog.geneset.list()), 
                         selected = "User-defined List", 
                         server   = TRUE)
  })
  
  # 根据选择的瘤种，确定突变队列，选择所有队列的基因的交集
  nonici.data.list <- eventReactive(input$prognostic_cancer_id,{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    select_tumor <- input$prognostic_cancer_id
    cat(cat_prefix_prog,"-选择的瘤种是:",select_tumor,"\n")
    output.list <- prog.cohort.list[select_tumor][[1]]
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  
  prog.gene.list <- eventReactive(input$prognostic_preset_id,{
    # 页面缓冲显示
    # show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    selected_set_name <- input$prognostic_preset_id       
    if(selected_set_name != 'User-defined List'){
      output.list <- intersect(nonici.data.list()[['symbols']],pre_genesets_list[[selected_set_name]])
    }else{
      output.list <- ''
    }
    # remove_modal_spinner()  # 搜索结束后消除缓冲显示
    return(output.list)
  })
  observe({
    # 更新基因名
    updateSelectizeInput(session = session, 
                         inputId = "prognostic_symbol_id", 
                         choices = c(nonici.data.list()[['symbols']]), 
                         selected = prog.gene.list(), 
                         server   = TRUE)
  })
  
  observe({
    shinyjs::toggleState("prognostic_goButton", 
                         !is.null(input$prognostic_symbol_id) && input$prognostic_symbol_id != "")
  })
  
  selected.row <- eventReactive(input$prog_pancancer_table_rows_selected,{
    slted.row <- input$prog_pancancer_table_rows_selected
    cat(cat_prefix_prog,"-选择的行是:",slted.row,"\n")
    return(slted.row)
  })
  
  # 输入为空验证
  iv_prognostic <- InputValidator$new()
  iv_prognostic$add_rule("prognostic_symbol_id", sv_required())
  iv_prognostic$enable()
  
  # 业务层：KM分析
  observeEvent(input$prognostic_goButton,{
    cat("====================== Server Explorer=============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting exploration ..."
    )
    
    ## 获取前端输入
    cancer_detail_in <- input$prognostic_cancer_id
    cat(cat_prefix_prog,"-选择的瘤种: ", cancer_detail_in, "\n")
    symbol <- input$prognostic_symbol_id
    cat(cat_prefix_prog,"-选择的基因: ", symbol, "\n")
    prognostic_logical_type <- input$prognostic_logical
    cat(cat_prefix_prog,"-选择的多基因突变逻辑关系: ", prognostic_logical_type, "\n")
    
    # 获取基因列表和队列名称
    input.genes <- symbol
    input.study.df <- nonicicohort.df %>%
      dplyr::filter(tumor_detail == cancer_detail_in)
    
    # 第一层tab，只输出一层tab
    # output$prognostic_maintabs <- renderUI({
    #   tabPanel(
    #     title = "prog_pancancer-Explore",
    #     uiOutput(paste0("prog_pancancer_ici"))
    #   )
    #   # 
    #   # tabs <- lapply("prog_pancancer-Explore", function(name) {
    #   #   tabPanel(
    #   #     title = name,
    #   #     uiOutput(paste0("prog_pancancer_ici"))
    #   #   )
    #   # })
    #   # do.call(tabsetPanel, tabs)
    # })
    # 执行数据分析
    # 先遍历队列获取肿瘤突变和临床信息，再遍历基因，再遍历outcomes
    # 外层Tab
    output$prognostic_maintabs <- renderUI({
      fluidPage(
        fluidRow(
          column(width = 9),
          DT::dataTableOutput(paste0('prog_pancancer', "_table")),
          h5(HTML(paste("<span style=color:black;font-size:12px;>", "gene1#gene2#...: mutation of at least one of these genes. 
                        mut: mutation, wt: wild type.", "</span>"))),
          
          downloadButton(paste0('prog_pancancer', "_table_dl"), 
                         tags$span(
                           "DLTable",
                            add_prompt(tags$span(icon(name = "circle-question")),
                                       message = "Save results in a txt file.", 
                                       position = "left")),
                         style = "display:inline-block;float:left;color: #fff; 
                         background-color: #27ae60; border-color: #fff;
                         padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
          downloadButton(paste0('prog_selected_row_plot_dl'), 
                         tags$span(
                           "DLGraph",
                            add_prompt(tags$span(icon(name = "circle-question")),
                                       message = "Save plot in a PDF file.", 
                                       position = "left")),
                         style = "display:inline-block;float:left;color: #fff; 
                         background-color: #27ae60; border-color: #fff;
                         padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; ")
        ),
        fluidRow(
          column(width = 9),
          tabPanel(
            title = paste0("plot_tab"), status = "primary", solidHeader = TRUE, collapsible = TRUE,
            shinycssloaders::withSpinner(plotOutput(paste0("prog_selected_row_plot"),height = "350px", width = "100%"))
          )
        )
      )
      
    })
    
    # 遍历队列
    n <- length(names(nonici.data.list()[[1]])) 
    study.out <- do.call(rbind,lapply(1:n, function(i) {
      
      # sid = 'TCGA_LUAD'
      sid <- unique(input.study.df$study_id)[i]
      prog.cli.df <- nonici.data.list()[[1]][[sid]]
      prog.mut.df <- nonici.data.list()[[2]][[sid]]
      
      update_modal_progress(
        value = i / n,
        text =  paste("Analyzing cohort:", sid,sprintf("(%1.0f%%)", i/n*100)) 
      )
      
      # 过滤样本：选择该瘤种的样本的临床信息和突变数据
      tumor.samples <- intersect(prog.cli.df$sample_id,unique(prog.mut.df$sample_id))
      tumor.cli.df <- prog.cli.df %>%
        dplyr::filter(sample_id %in% tumor.samples,      # 同时有突变和临床信息
                      cancer_detail == cancer_detail_in, # 筛选该瘤种的数据
                      sample_type == 'T'             # 肿瘤人群
        )
      tumor.mut.df <- prog.mut.df %>%
        dplyr::filter(sample_id %in% tumor.cli.df$sample_id)
      
      # 遍历基因及其组合（2/All）
      # combn.types <- c('Single','Compound','All')
      # 使用combn函数列出所有的组合情况
      genes.list = c()
      if(length(input.genes) >= 1){
        # combn.n 表示每个组合的基因数目
        combn.n <- c()
        for(i in 1:length(input.genes)){
          # 只列出1、2、max的组合
          if(i %in% c(1,2,length(input.genes))){
            combn.n = append(combn.n,i)
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
        middle.plot.list <- list() # 新建一个空list，用于存贮图
        # sid = 'Samstein_2018'
        # cancer_detail_in = 'Melanoma'
        # gname = 'NTRK2'
        cat(cat_prefix_prog,"-分析队列: ",sid,", 瘤种: ",cancer_detail_in,", 基因: ",gname,"\n")
        
        var.gene <- unlist(strsplit(gname,"#"))
        
        # 筛选出突变样本
        if(!grepl("#",gname)){
          tmp.mut.df <- tumor.mut.df %>%
            dplyr::filter(hugo_symbol %in% var.gene) %>%
            dplyr::distinct(sample_id,.keep_all = T)
        }else{
          # 多个基因时
          # browser()
          if(prognostic_logical_type == 'AND'){
            # logical 为 AND 时
            cat(cat_prefix_prog,"- 逻辑与选择多基因突变模式！\n")
            tmp.mut.samples <- tumor.mut.df %>%
              dplyr::filter(hugo_symbol %in% var.gene) %>%
              dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
              dplyr::group_by(sample_id) %>%
              dplyr::summarise(sample_count = n()) %>%
              dplyr::ungroup() %>%
              dplyr::filter(sample_count == length(var.gene)) # 取
          }
          if(prognostic_logical_type == 'OR'){
            # logical 为 OR 时
            cat(cat_prefix_prog,"- 逻辑或选择多基因突变模式！\n")
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
                                 # 'PFS_Coef' = NA,'PFS_HR' = NA,'PFS_Lower' = NA,'PFS_Upper' = NA,
                                 "PFS_HR95CI" = '',"PFS_Cox_P" = NA) 
        cox.df.os <- data.frame('OS_m_mut' = NA,'OS_m_wt' = NA,
                                # 'OS_Coef' = NA,'OS_HR' = NA,'OS_Lower' = NA,'OS_Upper' = NA,
                                "OS_HR95CI" = '',"OS_Cox_P" = NA) 
        tmb.res.df <-  data.frame("TMB_p" = NA,"TMB_m_mut" = NA,"TMB_m_wt" = NA)
        neo.res.df <-  data.frame("Neoantigen_p" = NA,"Neo_m_mut" = NA,"Neo_m_wt"  = NA)
        pdl1exp.res.df <-  data.frame("PDL1exp_p" = NA,"PDL1_m_mut" = NA,"PDL1_m_wt"  = NA)
        tp.res.df <-  data.frame("Tumor_Purity_p" = NA,"Tumor_Purity_m_mut" = NA,"Tumor_Purity_m_wt"  = NA)
        # pdl1.res.df =  data.frame("PDL1_P.method" = NA,"PDL1_P" = NA)
        # orr.res.df  = data.frame("ORR_P.method" = NA,"ORR_P" = NA)
        # response.res.df = data.frame("Resp_P.method" = NA,"Resp_P" = NA)
        
        if(nrow(tmp.mut.df) == 0){
          cat(cat_prefix_prog,"-未检测到",gname,"突变样本!\n")
          base.info.df <- data.frame(Cohort = sid,
                                     Cancer = cancer_detail_in,
                                     Gene = gname,
                                     Desciption = "All WT",
                                     NoOfPts = nrow(tumor.cli.df),
                                     NoOfMut = 0)
        }else{
          # browser()
          plot.cli <- tumor.cli.df %>%
            dplyr::mutate(is_mut = ifelse(sample_id %in% tmp.mut.df$sample_id,'Mut','WT')) 
          
          if(length(unique(plot.cli$is_mut)) == 1){
            # 全部为突变
            cat(cat_prefix_prog,"-全部为",unique(plot.cli$is_mut),"样本!\n")
            base.info.df <- data.frame(Cohort = sid,
                                       Cancer = cancer_detail_in,
                                       Gene = gname,
                                       Desciption = paste0("All ",unique(plot.cli$is_mut)),
                                       NoOfPts = nrow(plot.cli),
                                       NoOfMut = nrow(plot.cli))
          }else{
            
            prog.plot.cli <- plot.cli %>%
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
                Cox.p <- signif(as.numeric(x.sum$sc["pvalue"]),2) # 科学计数法保持两位小数,sc表示输出logrank检验结果
                Logrank.p <- round(x.sum$sc["test"], 2)
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
                median.mut <- x.mtable[which(x.mtable$strata == 'Mut'),]$Mid
                median.wt <- x.mtable[which(x.mtable$strata == 'WT'),]$Mid
                
                tmp.res <- c(median.mut,median.wt,
                             # Coef,HR,L95,H95,
                             HR95CI,Cox.p)
                names(tmp.res)<-c("m_mut","m_wt",
                                  # 'Coef','HR','Lower','Upper',
                                  "HR95CI","Cox_P")
                return(tmp.res)
              }
              
              # PFS
              if("pfs_months" %in% colnames(prog.plot.cli)){
                tmp.dat <- prog.plot.cli %>%
                  dplyr::select(sample_id,times = pfs_months,status = pfs_status,type) %>%
                  dplyr::filter(!is.na(times),!is.na(status)) %>%
                  dplyr::arrange(type)
                cox.df.pfs <- t(as.data.frame(univ_cox(tmp.dat),check.names = FALSE))
                colnames(cox.df.pfs) <- paste0('PFS_',colnames(cox.df.pfs))
                
                #绘图
                km.graphic <- tmp.dat %>%
                  dplyr::rename(risk = type) %>%
                  func.survivalplot('',"PFS",
                                    time.break = time_break_survival(max(tmp.dat$times,na.rm = T)),
                                    tbl.loc = 'topright',
                                    color.dis = c("#2196F3","#FF9800"),
                                    y.title =  'Progression-free Survival')
                middle.prog.graphic[[paste0(sid,"_",gname)]]$pfs <- km.graphic
                middle.plot.list[['pfs']] <- ggarrange(km.graphic$plot, km.graphic$table, heights = c(2, 0.7),
                                                       ncol = 1, nrow = 2, align = "v") 
              }
              # PFI，TCGA数据为PFI，以此替代PFS输出
              if("pfi_months" %in% colnames(prog.plot.cli)){
                tmp.dat <- prog.plot.cli %>%
                  dplyr::select(sample_id,times = pfi_months,status = pfi_status,type) %>%
                  dplyr::filter(!is.na(times),!is.na(status)) %>%
                  dplyr::arrange(type)
                cox.df.pfs <- t(as.data.frame(univ_cox(tmp.dat),check.names = FALSE))
                colnames(cox.df.pfs) <- paste0('PFS_',colnames(cox.df.pfs))
                
                #绘图
                km.graphic <- tmp.dat %>%
                  dplyr::rename(risk = type) %>%
                  func.survivalplot('',"PFS",
                                    time.break = time_break_survival(max(tmp.dat$times,na.rm = T)),
                                    tbl.loc = 'topright',
                                    color.dis = c("#2196F3","#FF9800"),
                                    y.title =  'Progression-free Survival')
                middle.prog.graphic[[paste0(sid,"_",gname)]]$pfs <- km.graphic
                middle.plot.list[['pfs']] <- ggarrange(km.graphic$plot, km.graphic$table, heights = c(2, 0.7),
                                                       ncol = 1, nrow = 2, align = "v") 
              }
              # OS
              if("os_months" %in% colnames(prog.plot.cli)){
                # browser()
                tmp.dat <- prog.plot.cli %>%
                  dplyr::select(sample_id,times = os_months,status = os_status,type) %>%
                  dplyr::mutate(times = as.numeric(times),
                                status = as.numeric(status)) %>%
                  dplyr::filter(!is.na(times),!is.na(status)) %>%
                  dplyr::arrange(type)
                cox.df.os <- t(as.data.frame(univ_cox(tmp.dat),check.names = FALSE))
                colnames(cox.df.os) <- paste0('OS_',colnames(cox.df.os))
                
                #绘图
                km.graphic <- tmp.dat %>%
                  dplyr::rename(risk = type) %>%
                  func.survivalplot('',"OS",
                                    time.break = time_break_survival(max(tmp.dat$times,na.rm = T)),
                                    tbl.loc = 'topright',
                                    color.dis = c("#2196F3","#FF9800"), # color1 = "#2196F3",color2 = "#FF9800",
                                    y.title =  'Overall Survival')
                middle.prog.graphic[[paste0(sid,"_",gname)]]$os <- km.graphic
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
            
            # TMB 分析
            if(("tmb" %in% colnames(prog.plot.cli))){
              prog.plot.cli$tmb <- as.numeric(prog.plot.cli$tmb)
              tmp.p <- con.wilcox(prog.plot.cli,'tmb')
              median.tmb.mut = median(prog.plot.cli[which(prog.plot.cli$type == 'Mut'),]$tmb,na.rm = T)
              median.tmb.wt = median(prog.plot.cli[which(prog.plot.cli$type == 'WT'),]$tmb,na.rm = T)
              
              tmb.res.df <-  data.frame("TMB_p" = tmp.p,
                                        "TMB_m_mut" = median.tmb.mut,
                                        "TMB_m_wt" = median.tmb.wt)
              
              #绘图
              km.graphic <- prog.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = tmb) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'TMB')
              middle.prog.graphic[[paste0(sid,"_",gname)]]$box_tmb <- km.graphic
              middle.plot.list[['tmb']] <- km.graphic
            }
            
            # Neoantigen 分析
            if(("neoantigen" %in% colnames(prog.plot.cli))){
              prog.plot.cli$neoantigen <- as.numeric(prog.plot.cli$neoantigen)
              neo.p <- con.wilcox(prog.plot.cli,'neoantigen')
              median.neo.mut = median(prog.plot.cli[which(prog.plot.cli$type == 'Mut'),]$neoantigen,na.rm = T)
              median.neo.wt = median(prog.plot.cli[which(prog.plot.cli$type == 'WT'),]$neoantigen,na.rm = T)
              neo.res.df <-  data.frame("Neoantigen_p" = neo.p,
                                        "Neo_m_mut" = median.neo.mut,
                                        "Neo_m_wt"  = median.neo.wt)
              
              #绘图
              km.graphic <- prog.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = neoantigen) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'Neoantigen')
              middle.prog.graphic[[paste0(sid,"_",gname)]]$box_neoantigen <- km.graphic
              middle.plot.list[['neoantigen']] <- km.graphic
            }
            
            # PD_L1_Exp 分析
            if(("pdl1_exp" %in% colnames(prog.plot.cli))){
              prog.plot.cli$pdl1_exp <- as.numeric(plot.cli$pdl1_exp)
              pdl1.p <- con.wilcox(prog.plot.cli,'pdl1_exp')
              median.pdl1.mut = median(prog.plot.cli[which(prog.plot.cli$type == 'Mut'),]$pdl1_exp,na.rm = T)
              median.pdl1.wt = median(prog.plot.cli[which(prog.plot.cli$type == 'WT'),]$pdl1_exp,na.rm = T)
              pdl1exp.res.df <-  data.frame("PDL1exp_p" = pdl1.p,
                                            "PDL1_m_mut" = median.pdl1.mut,
                                            "PDL1_m_wt"  = median.pdl1.wt)
              #绘图
              km.graphic <- prog.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = pdl1_exp) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'PD-L1 Expression')
              middle.prog.graphic[[paste0(sid,"_",gname)]]$box_pdl1_exp <- km.graphic
              middle.plot.list[['pdl1_exp']] <- km.graphic
            }
            
            # tumor_purity 分析
            if(("tumor_purity" %in% colnames(prog.plot.cli))){
              prog.plot.cli$tumor_purity <- as.numeric(prog.plot.cli$tumor_purity)
              tp.p <- con.wilcox(prog.plot.cli,'tumor_purity')
              median.tp.mut = median(prog.plot.cli[which(prog.plot.cli$type == 'Mut'),]$tumor_purity,na.rm = T)
              median.tp.wt = median(prog.plot.cli[which(prog.plot.cli$type == 'WT'),]$tumor_purity,na.rm = T)
              tp.res.df <-  data.frame("Tumor_Purity_p" = tp.p,
                                       "Tumor_Purity_m_mut" = median.tp.mut,
                                       "Tumor_Purity_m_wt"  = median.tp.wt)
              #绘图
              km.graphic <- prog.plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = tumor_purity) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'WT',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'Tumor Purity')
              middle.prog.graphic[[paste0(sid,"_",gname)]]$box_tumor_purity <- km.graphic
              middle.plot.list[['tumor_purity']] <- km.graphic
            }
            
            # pdl1.res.df =  data.frame("PDL1_P.method" = NA,"PDL1_P" = NA)
            # orr.res.df  = data.frame("ORR_P.method" = NA,"ORR_P" = NA)
            # response.res.df = data.frame("Resp_P.method" = NA,"Resp_P" = NA)
            
            if("pdl1" %in% colnames(prog.plot.cli)) {
              pdl1.res.df <- cat.chisqtest(prog.plot.cli,'pdl1')
              colnames(pdl1.res.df) <- c('PDL1_P.method','PDL1_P')
            }
            if("orr" %in% colnames(prog.plot.cli)) {
              orr.res.df <- cat.chisqtest(prog.plot.cli,'orr')
              colnames(orr.res.df) <- c('ORR_P.method','ORR_P')
            }
            if("response" %in% colnames(prog.plot.cli)) {
              response.res.df <- cat.chisqtest(prog.plot.cli,'response')
              colnames(response.res.df) <- c('Resp_P.method','Resp_P')
            }
            
            base.info.df <- data.frame(Cohort = sid,
                                       Cancer = cancer_detail_in,
                                       Gene = gname,
                                       Desciption = "MUT/WT",
                                       NoOfPts = nrow(prog.plot.cli),
                                       NoOfMut = nrow(prog.plot.cli[which(prog.plot.cli$type == 'Mut'),]))
            
            
          }
        }
        # 合并结果
        # cat("=========1213>",names(middle.plot.list),"\n")
        if(length(middle.plot.list) > 0){
          middle.plot <- ggarrange(plotlist = middle.plot.list,nrow = 1)
          output.prog.graphic[[paste0(sid,"_", gname)]]$plot <- middle.plot
        }
        
        #合并为一行结果
        tmp.row.res.df <- cbind(base.info.df,
                                cox.df.pfs,
                                cox.df.os
                                # tmb.res.df,
                                # neo.res.df,
                                # pdl1exp.res.df,
                                # tp.res.df,
                                # pdl1.res.df,
                                # orr.res.df,
                                # response.res.df
                                )
        return(tmp.row.res.df)
      }))  # 遍历基因结束
      # cat(cat_prefix_prog,"-遍历基因组合结束!\n")
      return(gene.out)
    }))    # 遍历队列结束
    cat(cat_prefix_prog,"-遍历队列结束!\n")
    
    display.out <- study.out #%>%
      # dplyr::select(-PFS_coef,-PFS_HR,-PFS_Lower,-PFS_Upper,
      #               -OS_coef,-OS_HR,-OS_Lower,-OS_Upper)
    
    sketch_porg = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Cohort'),
          th(rowspan = 2, 'Cancer Type'),
          th(rowspan = 2, 'Gene Symbol'),
          th(rowspan = 2, 'Description'),
          th(rowspan = 2, 'Sample Size'),
          th(rowspan = 2, 'Mutant Sample Size'),
          th(colspan = 4, 'PFS'),
          th(colspan = 4, 'OS')
          
        ),
        tr(
          lapply(c("mPFS(mut)","mPFS(wt)","HR(95%CI)","P value",
                   "mOS(mut)","mOS(wt)","HR(95%CI)","P value"), th)
        )
      )
    ))
    
    output[[paste0('prog_pancancer', "_table")]] <- renderDataTable({
      datatable(display.out, 
                container = sketch_porg,
                style = 'bootstrap', rownames = FALSE,
                selection = list(mode = "single", target = "row"), # target = "cell"：选择单元格
                options = list(scrollX = TRUE, keys = TRUE,
                               orderClasses = TRUE,dom = 'Bfrtip', language = list(
                  zeroRecords = "No records found matching your selection"),
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output[[paste0('prog_pancancer', "_table_dl")]] <- downloadHandler(
      filename = function() {
        paste0(prefix_output_file,"_ProgExplore_Result_",cancer_detail_in,"_",Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        readr::write_delim(x = display.out, path = file, delim = "\t")
      }
    )
    
    output[["prog_selected_row_plot"]] <- renderPlot({
      
      study.selected.df <- display.out[selected.row(),]
      plot.clinicals <- names(middle.prog.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]])
      cat(cat_prefix_prog,"-选择的临床字段:",plot.clinicals,"\n")
      output.prog.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]]$plot
    })
    output[[paste0("prog_selected_row_plot_dl")]] <- downloadHandler(
      filename = function() {
        paste0(prefix_output_file,"_ProgExplore_Plot_",cancer_detail_in,"_",Sys.Date(),".pdf", sep = "")
      },
      content = function(file) {
        study.selected.df <- display.out[selected.row(),]
        pdf(file,onefile = F, height = 4,width = 12, pointsize = 10)
        print(output.prog.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]]$plot)
        dev.off()
      }
    )
    
    remove_modal_progress()
  })
  
}
