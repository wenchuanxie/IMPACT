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

# 预测：初始化ICI治疗突变队列 ----
{
  # 步骤：1. 选择基因 --> 2. 选择瘤种 --> 3.分析输入基因（单个/全部组合）的突变与WT的生存获益，以及其他指标的差异 --> 4.表格输出结果；
  # 获取ICI队列名称
  icicohort.df <- study.df %>%
    dplyr::filter(study_type == 'ICIs',
                  grepl("Mutation",data_type))
  # 肿瘤名称
  ici.tumor.detail <- unique(icicohort.df$tumor_detail)
  cat("========>",ici.tumor.detail,"\n")
  
  # 预先将免疫队列的突变和临床数据加载到缓存
  ici.mut.list <- list()
  ici.cli.list <- list()
  length(ici.mut.list) <- length(unique(icicohort.df[which(grepl("Mutation",icicohort.df$data_type)),]$study_id))
  length(ici.cli.list) <- length(unique(icicohort.df[which(grepl("Mutation",icicohort.df$data_type)),]$study_id))
  for (i in seq(1,length(unique(icicohort.df$study_id)))) {
    sid <-  unique(icicohort.df$study_id)[i]
    
    tmp.df <- icicohort.df %>%
      dplyr::filter(study_id == sid)
    if(grepl("Mutation",tmp.df$data_type[1])){
      
      names(ici.mut.list)[[i]] = sid
      ici.mut.list[[i]] <- data.frame()
      
      names(ici.cli.list)[[i]] = sid
      ici.cli.list[[i]] <- data.frame()
      
      tbl.name <- paste0(tolower(sid),"_mutation")
      tmp.mut.df <- queryDataFromMySQL(tbl.name)
      ici.mut.list[[i]] <- tmp.mut.df
      
      tmp.cli.df <- queryDataFromMySQL(paste0(tolower(sid),"_clinical"))
      ici.cli.list[[i]] <- tmp.cli.df
    }
  }
  # 所有免疫队列中的基因symbol并集
  ici.symbols.df <- do.call(rbind,lapply(seq(1,length(ici.mut.list)), function(i){
    # 遍历所有队列获取并集
    # i = 1
    tmp.mut.df <-  ici.mut.list[[i]]
    
    if("hugo_symbol" %in% colnames(tmp.mut.df)){
      tmp.out <- data.frame(study_id = sid,
                            hugo_symbol = unique(tmp.mut.df$hugo_symbol))
    }else{
      tmp.out <- data.frame(study_id = sid,
                            hugo_symbol = '')
    }
    return(tmp.out)
  }))
  ici.symbols <- unique(ici.symbols.df$hugo_symbol)
  
}

tab_00_ici_explorer <- list()
cat_prefix_ici <- 'ICIExplore'
sidebar <- sidebarPanel(
  id = 'iciexplore_sidebar',
  width = 3,
  h3("Immunotherapy Predictive Biomarkers Exploration"),
  selectizeInput(inputId = "iciexplore_symbol_id", 
                 label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                 choices = NULL, 
                 width = '100%',
                 multiple = T, 
                 options = list(delimiter = " ", create = T,maxItems = 10,
                                placeholder = "No more than 10 genes.",
                                plugins = list('remove_button', 'drag_drop'))),
  pickerInput(inputId = "iciexplore_cancer_id",
              label = "Select one cancer type, E.g. 'LUAD'",
              choices = NULL,
              multiple = FALSE,
              selected = NULL),
  # 提交按钮
  actionButton(inputId = "iciexplore_goButton",
               label = "Submit",
               class ="btn-primary"
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
  observe({
    # 更新基因名
    updateSelectizeInput(session = session, 
                         inputId = "iciexplore_symbol_id", 
                         choices = c(ici.symbols), 
                         selected = NULL, 
                         server   = TRUE)
  })
  # 将获取到的肿瘤名称更新到页面
  observe({
    updatePickerInput(session = session, 
                      inputId = "iciexplore_cancer_id",
                      choices = c(ici.tumor.detail), 
                      selected = NULL)
  })
  

  # 从DB获取数据
  # 临床信息
  # db.dat.cli <- eventReactive(input$iciexplore_study_id,{
  #   cat(cat_prefix_ici,"-查询Clinical，待分析的研究队列: ",input$iciexplore_study_id,"\n")
  #   cat(cat_prefix_ici,"-查询Clinical，待分析的肿瘤（亚型）: ",input$iciexplore_cancer_id,"\n")
  #   
  #   study <- input$iciexplore_study_id
  #   cancer_id <- input$iciexplore_cancer_id # cancer_detail
  #   temp.df <- queryDataFromMySQL(paste0(tolower(study),'_clinical'))
  #   temp.df <- temp.df %>%
  #     dplyr::filter(cancer_detail %in% cancer_id)
  #   if("ici_treatment" %in% colnames(temp.df)){
  #     temp.df <- temp.df %>%
  #       # 免疫治疗队列有ici_treatment字段，需过滤出免疫治疗人群
  #       dplyr::filter(ici_treatment == 'Yes') 
  #   }
  #   if("sample_type" %in% colnames(temp.df)){
  #     temp.df <- temp.df %>%
  #       # GEO队列存在非肿瘤样本，需选择出肿瘤样本
  #       dplyr::filter(sample_type == 'T') 
  #   }
  #   return(temp.df)
  # })
  # 突变或者表达谱
  # db.dat <- eventReactive(c(input$iciexplore_study_id,
  #                           input$iciexplore_data_type),{
  #                             
  #                             data_type <- input$iciexplore_data_type
  #                             study <- input$iciexplore_study_id
  #                             cat(cat_prefix_ici,-"查询Mutation/Expr，待分析的研究队列: ",data_type,"\n")
  #                             
  #                             tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
  #                                                   data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
  #                                                   TRUE ~ '')
  #                             temp.df <- queryDataFromMySQL(tbl_name)
  #                             return(temp.df)
  #                           })
  # 
  selected.row <- eventReactive(input$pancancer_table_rows_selected,{
    slted.row <- input$pancancer_table_rows_selected
    cat(cat_prefix_ici,"-选择的行是:",slted.row,"\n")
    return(slted.row)
  })
  
  # 业务层：KM分析
  observeEvent(input$iciexplore_goButton,{
    cat("====================== Server Explore=============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#e95420",
      duration = 90,
      easing = "easeOut",
      text = "Starting Explore..."
    )
    ## 获取前端输入
    symbol <- input$iciexplore_symbol_id
    cat(cat_prefix_ici,"-选择的基因: ", symbol, "\n")
    cancer_detail_in <- input$iciexplore_cancer_id
    cat(cat_prefix_ici,"-选择的瘤种: ", cancer_detail_in, "\n")
    
    # 获取队列名称
    input.genes <- symbol
    input.study.df <- icicohort.df %>%
      dplyr::filter(tumor_detail == cancer_detail_in)
    
    # 第一层tab，只输出一层tab
    # output$iciexplore_maintabs <- renderUI({
    #   tabPanel(
    #     title = "Pancancer-Explore",
    #     uiOutput(paste0("pancancer_ici"))
    #   )
    #   # 
    #   # tabs <- lapply("Pancancer-Explore", function(name) {
    #   #   tabPanel(
    #   #     title = name,
    #   #     uiOutput(paste0("pancancer_ici"))
    #   #   )
    #   # })
    #   # do.call(tabsetPanel, tabs)
    # })
    # 执行数据分析
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
                           add_prompt(tags$span(icon(name = "question-circle")),
                                      message = "Save results in a txt file.", 
                                      position = "left")),
                         style = "display:inline-block;float:left;color: #fff; 
                         background-color: #27ae60; border-color: #fff;
                         padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
          downloadButton(paste0("selected_row_plot_dl"), 
                         tags$span(
                           "DLGraph",
                           add_prompt(tags$span(icon(name = "question-circle")),
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
    n <- length(unique(input.study.df$study_id))
    study.out <- do.call(rbind,lapply(1:n, function(i) {
      
      # sid = 'Allen_2015'
      sid <- unique(input.study.df$study_id)[i]
      ici.cli.df <- ici.cli.list[[sid]]
      ici.mut.df <- ici.mut.list[[sid]]
      
      update_modal_progress(
        value = i / n,
        text =  paste("Analyzing cohort:", sid,sprintf("(%1.0f%%)", i/n*100)) 
      )
      
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
        cat(cat_prefix_ici,"-分析队列: ",sid,", 瘤种: ",cancer_detail_in,", 基因: ",gname,"\n")
        
        var.gene <- unlist(strsplit(gname,"#"))
        
        # 筛选出突变样本
        tmp.mut.df <- tumor.mut.df %>%
          dplyr::filter(hugo_symbol %in% var.gene) %>%
          dplyr::distinct(sample_id,.keep_all = T)
        
        # 初始化输出对象
        cox.df.pfs <- data.frame('PFS_m_mut' = NA,'PFS_m_wt' = NA,
                                 # 'PFS_Coef' = NA,
                                 'PFS_HR' = NA,'PFS_Lower' = NA,'PFS_Upper' = NA,
                                 "PFS_HR95CI" = '',"PFS_Cox_P" = NA) 
        cox.df.os <- data.frame('OS_m_mut' = NA,'OS_m_wt' = NA,
                                # 'OS_Coef' = NA,
                                'OS_HR' = NA,'OS_Lower' = NA,'OS_Upper' = NA,
                                "OS_HR95CI" = '',"OS_Cox_P" = NA) 
        tmb.res.df <-  data.frame("TMB_p" = NA,"TMB_m_mut" = NA,"TMB_m_wt" = NA)
        neo.res.df <-  data.frame("Neoantigen_p" = NA,"Neo_m_mut" = NA,"Neo_m_wt"  = NA)
        pdl1exp.res.df <-  data.frame("PDL1exp_p" = NA,"PDL1_m_mut" = NA,"PDL1_m_wt"  = NA)
        tp.res.df <-  data.frame("Tumor_Purity_p" = NA,"Tumor_Purity_m_mut" = NA,"Tumor_Purity_m_wt"  = NA)
        pdl1.res.df =  data.frame("PDL1_P.method" = NA,"PDL1_P" = NA)
        orr.res.df  = data.frame("ORR_P.method" = NA,"ORR_P" = NA)
        response.res.df = data.frame("Resp_P.method" = NA,"Resp_P" = NA)
        
        if(nrow(tmp.mut.df) == 0){
          cat(cat_prefix_ici,"-未检测到",gname,"突变样本!\n")
          base.info.df <- data.frame(Cohort = sid,
                                     Cancer = cancer_detail_in,
                                     Gene = gname,
                                     Desciption = "All WT",
                                     NoOfPts = nrow(tumor.cli.df),
                                     NoOfMut = 0)
        }else{
          plot.cli <- tumor.cli.df %>%
            dplyr::mutate(is_mut = ifelse(sample_id %in% tmp.mut.df$sample_id,'Mut','Wt')) 
          
          if(length(unique(plot.cli$is_mut)) == 1){
            # 全部为突变
            cat(cat_prefix_ici,"-全部为",unique(plot.cli$is_mut),"样本!\n")
            base.info.df <- data.frame(Cohort = sid,
                                       Cancer = cancer_detail_in,
                                       Gene = gname,
                                       Desciption = paste0("All ",unique(plot.cli$is_mut)),
                                       NoOfPts = nrow(plot.cli),
                                       NoOfMut = nrow(plot.cli))
          }else{
            
            plot.cli <- plot.cli %>%
              dplyr::mutate(is_mut = factor(is_mut,levels = c('Wt','Mut'))) %>%
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
                median.wt <- x.mtable[which(x.mtable$strata == 'Wt'),]$Mid
                
                tmp.res <- c(median.mut,median.wt,
                             # Coef,
                             HR,L95,H95,
                             HR95CI,Cox.p)
                names(tmp.res)<-c("m_mut","m_wt",
                                  # 'Coef',
                                  'HR','Lower','Upper',
                                  "HR95CI","Cox_P")
                return(tmp.res)
              }
              
              # PFS
              if("pfs_months" %in% colnames(plot.cli)){
                tmp.dat <- plot.cli %>%
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
                middle.graphic[[paste0(sid,"_",gname)]]$pfs <- km.graphic
                middle.plot.list[['pfs']] <- ggarrange(km.graphic$plot, km.graphic$table, heights = c(2, 0.7),
                                                       ncol = 1, nrow = 2, align = "v") 
              }
              # OS
              if("os_months" %in% colnames(plot.cli)){
                tmp.dat <- plot.cli %>%
                  dplyr::select(sample_id,times = os_months,status = os_status,type) %>%
                  dplyr::filter(!is.na(times),!is.na(status)) %>%
                  dplyr::mutate(times = as.numeric(times),
                                status = as.numeric(status)) %>%
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
            
            # TMB 分析
            if(("tmb" %in% colnames(plot.cli))){
              plot.cli$tmb <- as.numeric(plot.cli$tmb)
              tmp.p <- con.wilcox(plot.cli,'tmb')
              median.tmb.mut = median(plot.cli[which(plot.cli$type == 'Mut'),]$tmb,na.rm = T)
              median.tmb.wt = median(plot.cli[which(plot.cli$type == 'Wt'),]$tmb,na.rm = T)
              
              tmb.res.df <-  data.frame("TMB_p" = tmp.p,
                                        "TMB_m_mut" = median.tmb.mut,
                                        "TMB_m_wt" = median.tmb.wt)
              
              #绘图
              km.graphic <- plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = tmb) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'Wt',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'TMB')
              middle.graphic[[paste0(sid,"_",gname)]]$box_tmb <- km.graphic
              middle.plot.list[['tmb']] <- km.graphic
            }
            
            # Neoantigen 分析
            if(("neoantigen" %in% colnames(plot.cli))){
              plot.cli$neoantigen <- as.numeric(plot.cli$neoantigen)
              neo.p <- con.wilcox(plot.cli,'neoantigen')
              median.neo.mut = median(plot.cli[which(plot.cli$type == 'Mut'),]$neoantigen,na.rm = T)
              median.neo.wt = median(plot.cli[which(plot.cli$type == 'Wt'),]$neoantigen,na.rm = T)
              neo.res.df <-  data.frame("Neoantigen_p" = neo.p,
                                        "Neo_m_mut" = median.neo.mut,
                                        "Neo_m_wt"  = median.neo.wt)
              
              #绘图
              km.graphic <- plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = neoantigen) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'Wt',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'Neoantigen')
              middle.graphic[[paste0(sid,"_",gname)]]$box_neoantigen <- km.graphic
              middle.plot.list[['neoantigen']] <- km.graphic
            }
            
            # PD_L1_Exp 分析
            if(("pdl1_exp" %in% colnames(plot.cli))){
              plot.cli$pdl1_exp <- as.numeric(plot.cli$pdl1_exp)
              pdl1.p <- con.wilcox(plot.cli,'pdl1_exp')
              median.pdl1.mut = median(plot.cli[which(plot.cli$type == 'Mut'),]$pdl1_exp,na.rm = T)
              median.pdl1.wt = median(plot.cli[which(plot.cli$type == 'Wt'),]$pdl1_exp,na.rm = T)
              pdl1exp.res.df <-  data.frame("PDL1exp_p" = pdl1.p,
                                            "PDL1_m_mut" = median.pdl1.mut,
                                            "PDL1_m_wt"  = median.pdl1.wt)
              #绘图
              km.graphic <- plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = pdl1_exp) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'Wt',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'PD-L1 Expression')
              middle.graphic[[paste0(sid,"_",gname)]]$box_pdl1_exp <- km.graphic
              middle.plot.list[['pdl1_exp']] <- km.graphic
            }
            
            # tumor_purity 分析
            if(("tumor_purity" %in% colnames(plot.cli))){
              plot.cli$tumor_purity <- as.numeric(plot.cli$tumor_purity)
              tp.p <- con.wilcox(plot.cli,'tumor_purity')
              median.tp.mut = median(plot.cli[which(plot.cli$type == 'Mut'),]$tumor_purity,na.rm = T)
              median.tp.wt = median(plot.cli[which(plot.cli$type == 'Wt'),]$tumor_purity,na.rm = T)
              tp.res.df <-  data.frame("Tumor_Purity_p" = tp.p,
                                       "Tumor_Purity_m_mut" = median.tp.mut,
                                       "Tumor_Purity_m_wt"  = median.tp.wt)
              #绘图
              km.graphic <- plot.cli %>%
                dplyr::select(sample_id,group = type,img_name = tumor_purity) %>%
                func.ggboxplot(group1 = 'Mut',group2 = 'Wt',color1 = "#FF9800",color2 = "#2196F3",ytitle = 'Tumor Purity')
              middle.graphic[[paste0(sid,"_",gname)]]$box_tumor_purity <- km.graphic
              middle.plot.list[['tumor_purity']] <- km.graphic
            }
            
            pdl1.res.df =  data.frame("PDL1_P.method" = NA,"PDL1_P" = NA)
            orr.res.df  = data.frame("ORR_P.method" = NA,"ORR_P" = NA)
            response.res.df = data.frame("Resp_P.method" = NA,"Resp_P" = NA)
            
            if("pdl1" %in% colnames(plot.cli)) {
              pdl1.res.df <- cat.chisqtest(plot.cli,'pdl1')
              colnames(pdl1.res.df) <- c('PDL1_P.method','PDL1_P')
            }
            if("orr" %in% colnames(plot.cli)) {
              orr.res.df <- cat.chisqtest(plot.cli,'orr')
              colnames(orr.res.df) <- c('ORR_P.method','ORR_P')
            }
            if("response" %in% colnames(plot.cli)) {
              response.res.df <- cat.chisqtest(plot.cli,'response')
              colnames(response.res.df) <- c('Resp_P.method','Resp_P')
            }
            
            base.info.df <- data.frame(Cohort = sid,
                                       Cancer = cancer_detail_in,
                                       Gene = gname,
                                       Desciption = "MUT/WT",
                                       NoOfPts = nrow(plot.cli),
                                       NoOfMut = nrow(plot.cli[which(plot.cli$type == 'Mut'),]))
            
            
          }
        }
        # 合并结果
        # cat("=========1213>",names(middle.plot.list),"\n")
        if(length(middle.plot.list) > 0){
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
        return(tmp.row.res.df)
      }))  # 遍历基因结束
      # cat(cat_prefix_ici,"-遍历基因组合结束!\n")
      return(gene.out)
    }))    # 遍历队列结束
    cat(cat_prefix_ici,"-遍历队列结束!\n")
    
    # meta 分析
    {
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
          # browser()
          if(nrow(tmp.meta.ib.pfs) > 0){
            clc.meta <- tmp.meta.ib.pfs %>%
              dplyr::mutate(lghr = log(HR),
                            lglci = log(L95),lghci = log(U95)) %>%
              dplyr::mutate(selghr = (lghci - lglci)/(2*1.96)) 
            
            settings.meta('revman5')
            meta.res.pfs <- metagen(TE = lghr,seTE = selghr,studlab = clc.meta$Cohort,data = clc.meta,sm = 'HR',
                                    text.w.fixed = "w.fixed",
                                    # 确定方法之后，可以将不使用的方法设置为F，不输出图
                                    comb.fixed = T,comb.random = F,
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
            dplyr::mutate(HR = as.numeric(HR),L95 = as.numeric(L95),U95 = as.numeric(U95))
          if(nrow(tmp.meta.ib.os) > 0){
            clc.meta <- tmp.meta.ib.os %>%
              dplyr::mutate(lghr = log(HR),
                            lglci = log(L95),lghci = log(U95)) %>%
              dplyr::mutate(selghr = (lghci - lglci)/(2*1.96)) 
            settings.meta('revman5')
            meta.res.os <- metagen(TE = lghr,seTE = selghr,studlab = clc.meta$Cohort,data = clc.meta,sm = 'HR',
                                    text.w.fixed = "w.fixed",
                                    # 确定方法之后，可以将不使用的方法设置为F，不输出图
                                    comb.fixed = T,comb.random = F,
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
      
    }
    
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
          th(colspan = 4, 'PFS'),
          th(colspan = 4, 'OS'),
          th(colspan = 3, 'TMB'),
          th(colspan = 3, 'TNB'),
          th(colspan = 3, 'PD-L1 expression'),
          th(colspan = 3, 'Tumor purity(TP)'),
          th(colspan = 1, 'PD-L1(cat)'),
          th(colspan = 1, 'ORR'),
          th(colspan = 1, 'Response')
          
        ),
        tr(
          lapply(c("mPFS(mut)","mPFS(wt)","HR(95%CI)","P value",
                   "mOS(mut)","mOS(wt)","HR(95%CI)","P value",
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
        paste0(prefix_output_file,"_PredExplore_Resu;t_",cancer_detail_in,"_",Sys.Date(), '.txt', sep='')
      },
      content = function(file) {
        readr::write_delim(x = display.out, path = file, delim = "\t")
      }
    )

    output[["selected_row_plot"]] <- renderPlot({
      
      study.selected.df <- display.out[selected.row(),]
      plot.clinicals <- names(middle.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]])
      cat(cat_prefix_ici,"-临床指标:",plot.clinicals,"\n")
      output.graphic[[paste0(study.selected.df$Cohort[1],"_",study.selected.df$Gene[1])]]$plot
    })
    output[["selected_row_plot_pfs"]] <- renderImage({
      
      study.selected.df <- display.out[selected.row(),]
      # browser()
      if(!is.null(output.metalist[[paste0(study.selected.df$Gene[1],"_pfs")]]$data)){
        cat("There have meta plot for pfs\n")
        # clc.meta <- meta.plot.list[[paste0(study.selected.df$Gene[1],"_pfs")]]
        clc.meta <- output.metalist[[paste0(study.selected.df$Gene[1],"_pfs")]]$data
        settings.meta('revman5')
        meta.res.pfs <- metagen(TE = lghr,seTE = selghr,studlab = clc.meta$Cohort,data = clc.meta,sm = 'HR',
                               text.w.fixed = "w.fixed",
                               # 确定方法之后，可以将不使用的方法设置为F，不输出图
                               comb.fixed = T,comb.random = F,
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
          geom_text(aes(0, 0, label = "NOT APPLICABLE!"),
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
             alt = "There is no plot for PFS")
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
                                # 确定方法之后，可以将不使用的方法设置为F，不输出图
                                comb.fixed = T,comb.random = F,
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
             alt = "There is no plot for OS")
      }else{
        # filename <- normalizePath(file.path('./www',paste('demo.not.applicable.png', sep='')))
        p.noinfo <- ggplot2::ggplot() +
          theme_void() +
          geom_text(aes(0, 0, label = "NOT APPLICABLE!"),
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
