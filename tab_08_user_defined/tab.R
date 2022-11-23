#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-07-15
# @IDE     : RStudio
# @Desc    : 用户自定义数据分析
#===============================================================================

source("./r/func.survivalplot.R")
source("./r/func.bubbleplot.R")
source("./r/func.triangleheatmap.R")
source("./r/func.psychcorrwide2long.R")
source("./r/func.cutpointcalc.R")
source("./r/func.cutpointplot.R")

tab_08_user_defined <- list()
cat_prefix_mut <- 'userdefined'

sidebar <- sidebarPanel(
  id = 'userdefined_sidebar',
  width = 3,
  h3("User-defined Analysis"),
  
  # 研究类型
  awesomeRadio(inputId = "diy_funid",
               label = "Select function", 
               choices = c("Cutpoint Analysis",
                           "Kaplan-Meier Curve", 
                           # "Cox Regression",
                           'Subgroup Analysis',
                           'Correlation Analysis'
                           # 'Heatmap',
                           # 'GSVA'
                           ),
               selected = "Cutpoint Analysis",
               # inline = T,
               status = "success"),
  
  # actionButton(inputId = "showdemo", 
  #              label = "Example Input",
  #              class ="btn-primary"),
  # 文件输入
  fileInput("upload_fileid", 
            # "Upload data file",
            HTML("Upload data file (tab-delimited text or csv)", 
                 as.character(actionLink(inputId = 'showdemo', label = '(example)'))),
            multiple = FALSE,
            accept = c("text",".txt",".csv")),
  # conditionalPanel(
  #   condition = "input.diy_funid != 'Correlation Analysis'",
  #   
  # ),
  conditionalPanel(
    condition = "input.diy_funid  == 'Correlation Analysis' ",
    fileInput("upload_fileid_y", 
              # "Upload data file",
              HTML("(optional) Upload data file 2", 
                   as.character(actionLink(inputId = 'showdemo_y', label = '(example)'))),
              multiple = FALSE,
              accept = c("text",".txt",".csv")),
    helpText("Corr between two file will be calculated if provided the 2nd file!"),
  ),
  
  # 参数控制按钮
  radioButtons("arg", "Change parameters:",
               c("No" = "no",
                 "Yes" = "yes")),
  # 提交按钮
  actionButton(inputId = "userdefined_goButton",
               label = "Submit",
               class ="btn-primary"),
  actionButton(inputId = 'reset',
               label = 'Delete file',
               class ="btn-warning"),
  
  # hr(color="black"),
  tags$hr(style="border-color: purple;"),
  h3("Download Plot", style = "margin-bottom: 25px;color:#00a064"),
  fluidRow(
    # column(width = 7),
    # column(width = 3, #offset = 1,
    #        box(title = "Download Plot",#solidHeader = T, collapsed = T,collapsible = T,status = "warning",
    #            width = 12,
    #            
    #        )
    # )
    column(width = 6,
           numericInput("plotheigh",label = "Plot heigh value (inches)",value = 3)
    ),
    column(width = 6,
           numericInput("plotwidth",label = "Plot width value (inches)",value = 5)
    ),
    column(width = 12,
           textInput(inputId = "plotoutputname", label = "Output file name", width = "60%", placeholder = "")
    ),
    column(width = 12,
           radioButtons("plottype",label = "Select the graph type",inline = T,choices = c("png","pdf"))
    ),
    column(width = 12,
           # downloadButton("diy_dl_plot",label = "Download Plot"),
           downloadButton("diy_dl_plot", 
                          tags$span(
                            "Download Plot",
                            add_prompt(tags$span(icon(name = "circle-question")),
                                       message = "Save plot in a PDF file.", 
                                       position = "left")),
                          style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "))
  ),
  
  # 示例文件展示
  bsModal(
    id = "readme", title = "User Guide", trigger = "showdemo", size = "large",
    # tags$br(),
    tags$p("1. Take a function you need."),
    tags$p("2. Follow the example below to prepare the input file."),
    tags$p("3. Upload data with menu on the left."),
    tags$p("4. Change the parameters of the plot if exist."),
    tags$p("5. If you change the parameters of the plot, example plot will be displayed, and re-Submit is need!"),
    # tags$br(),
    tags$h5("The example data:", style = "color:#47a3da; font-family: sans-serif"),
    DTOutput("example_input")
  ),
  
  bsModal(
    id = "readme_y", title = "User Guide", trigger = "showdemo_y", size = "large",
    # tags$br(),
    tags$p("1. Take the corr function."),
    tags$p("2. Follow the example below to prepare the annother input file."),
    tags$p("3. Upload data with menu on the left."),
    tags$p("4. Change the parameters of the plot if exist."),
    tags$p("5. If you change the parameters of the plot, example plot will be displayed, and re-Submit is need!"),
    # tags$br(),
    tags$h5("The example data:", style = "color:#47a3da; font-family: sans-serif"),
    DTOutput("example_input_y")
  )
    
)
  
# 参数列表
parametersDataInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #UI Definition
  column(
    3,
    conditionalPanel(
      condition = "input.arg == 'yes' && input.diy_funid == 'Cutpoint Analysis'",
      wellPanel(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Parameters of cutpoint analysis:",solidHeader = T,  status = "primary",
              width = 12,
              selectInput("diy_cf_outcome", "Choose outome:",
                          c("PFS","DFS","OS","PFI")),
              # textInput(inputId = "diy_km_title", label = "Graphic title", width = "60%", placeholder = ""),
              colourpicker::colourInput(inputId = "diy_cf_colour", 
                                        label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Color for High/Mut group", 
                                                     position = "right"),
                                          'Colour of plot'), 
                                        value = "#FF9800", 
                                        showColour = c("both", "text","background"), 
                                        palette = c("square", "limited"), 
                                        allowTransparent = FALSE, 
                                        returnName = TRUE)
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.arg == 'yes' && input.diy_funid == 'Kaplan-Meier Curve'",
      wellPanel(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Parameters of KM curve:",solidHeader = T,  status = "primary",
              width = 12,
              selectInput("diy_km_outcome", "Choose outome:",
                          c("PFS","DFS","OS","PFI")),
              # textInput(inputId = "diy_km_title", label = "Graphic title", width = "60%", placeholder = ""),
              colourpicker::colourInput(inputId = "diy_colorg1_id", 
                                        label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Color for High/Mut group", 
                                                     position = "right"),
                                          'Group-1'), 
                                        value = "#FF9800", 
                                        showColour = c("both", "text","background"), 
                                        palette = c("square", "limited"), 
                                        allowTransparent = FALSE, 
                                        returnName = TRUE),
              colourpicker::colourInput(inputId = "diy_colorg2_id", 
                                        label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Color for Low/Wt group", 
                                                     position = "right"),
                                          'Group-2'),  
                                        value = "#2196F3", 
                                        showColour = c("both", "text","background"), 
                                        palette = c("square", "limited"), 
                                        allowTransparent = FALSE, 
                                        returnName = TRUE)
              )
            )
        )
      )
    ),
    conditionalPanel(
      condition = "input.arg == 'yes' && input.diy_funid == 'Cox Regression'",
      wellPanel(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Parameters of Cox:",solidHeader = T,  status = "primary",
              width = 12,
              colourpicker::colourInput(inputId = "diy_colorg3_id",
                                        label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Color for High/Mut group",
                                                     position = "right"),
                                          'Group-1'),
                                        value = "#FF9800",
                                        showColour = c("both", "text","background"),
                                        palette = c("square", "limited"),
                                        allowTransparent = FALSE,
                                        returnName = TRUE),
              colourpicker::colourInput(inputId = "diy_colorg4_id",
                                        label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Color for Low/Wt group",
                                                     position = "right"),
                                          'Group-2'),
                                        value = "#2196F3",
                                        showColour = c("both", "text","background"),
                                        palette = c("square", "limited"),
                                        allowTransparent = FALSE,
                                        returnName = TRUE)
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.arg == 'yes' && input.diy_funid == 'Subgroup Analysis'",
      wellPanel(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Parameters of Subgroup:",solidHeader = T,  status = "primary",
              width = 12, 
              column(width = 12,
                     numericInput("subgroup_base_size",label = "Plot base size",value = 18)
              ), 
              column(width = 12,
                     colourpicker::colourInput(inputId = "subgroup_refline_col",
                                               label = tags$span(
                                                 add_prompt(tags$span(icon(name = "circle-question")),
                                                            message = "Color for refline",
                                                            position = "right"),
                                                 'Refline'),
                                               value = "#FA0043",
                                               showColour = c("both", "text","background"),
                                               palette = c("square", "limited"),
                                               allowTransparent = FALSE,
                                               returnName = TRUE)
              ), 
              column(width = 12,
                     colourpicker::colourInput(inputId = "subgroup_footnote_col",
                                               label = tags$span(
                                                 add_prompt(tags$span(icon(name = "circle-question")),
                                                            message = "Color for footnnote text",
                                                            position = "right"),
                                                 'Footnote text'),
                                               value = "#0A12F7",
                                               showColour = c("both", "text","background"),
                                               palette = c("square", "limited"),
                                               allowTransparent = FALSE,
                                               returnName = TRUE)
              ),
              column(width = 12,
                     numericInput("subgroup_height",label = "Plot heigh value (px)",value = 900)
              ),
              column(width = 12,
                     numericInput("subgroup_width",label = "Plot width value (px)",value = 1000)
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      #Correlation Analyses
      condition = "input.arg == 'yes' && input.diy_funid == 'Correlation Analysis'",
      wellPanel(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Parameters of Subgroup:",solidHeader = T,  status = "primary",
              width = 12,
              column(12,
                     awesomeRadio(
                       inputId = "diy_corr_figure_type",label = "Figure Type:",inline = F,
                       choices = c("Bubble" = "bubble","tri-Heatmap" = "theatmap"
                                   # ,"Heatmap" = "cheatmap"
                                   ),
                       selected = "bubble",
                       status = "success",
                       checkbox = TRUE)
              ), 
              column(12,
                     awesomeRadio(inputId = "diy_cor_method",label = "Method:",inline = F,
                                  choices = c("pearson"  = "pearson",
                                              "spearman" = "spearman",
                                              "kendall" = "kendall"),
                                  selected = "spearman",
                                  status = "success",
                                  checkbox = TRUE)
              ),
              conditionalPanel(
                condition = "input.diy_corr_figure_type == 'cheatmap'",
                column(width = 12,
                       awesomeRadio(inputId = "diy_cor_shape", label = "Disply Type", inline = F,
                                    choices = c("circle", "color", "square", "ellipse", "pie", "number", "shade"), 
                                    selected = "circle",
                                    status = "success",
                                    checkbox = TRUE)
                )
              ),
              column(12,
                     colourpicker::colourInput(inputId = "diy_cor_colorg1_id", 
                                               label = tags$span(
                                                 add_prompt(tags$span(icon(name = "circle-question")),
                                                            message = "Color for positive correlation", 
                                                            position = "right"),
                                                 'Color for high corr'), 
                                               value = "#FF9800", 
                                               showColour = c("both", "text","background"), 
                                               palette = c("square", "limited"), 
                                               allowTransparent = FALSE, 
                                               returnName = TRUE)
              ),
              column(12,
                     colourpicker::colourInput(inputId = "diy_cor_colorg2_id", 
                                               label = tags$span(
                                                 add_prompt(tags$span(icon(name = "circle-question")),
                                                            message = "Color for nagative correlation", 
                                                            position = "right"),
                                                 'Color for low corr'),  
                                               value = "#2196F3", 
                                               showColour = c("both", "text","background"), 
                                               palette = c("square", "limited"), 
                                               allowTransparent = FALSE, 
                                               returnName = TRUE)
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.arg == 'yes' && input.diy_funid == 'GSVA'",
      wellPanel(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Parameters of GSVA:",solidHeader = T,  status = "primary",
              width = 12,
              selectInput("method", "Choose method:",
                          c("gsva","ssgsea","zscore","plage")),
              selectInput("kcdf", "Choose kcdf:",
                          c("Gaussian","Poisson","none")),
              radioButtons("absRanking", "abs.ranking:",
                           c("False" = FALSE,
                             "True" = TRUE)),
              numericInput("minSz","min.sz:",value = 1),
              numericInput("maxSz","max.sz (Write 0 for infinite):",value = 0),
              numericInput("parallelSz","parallel.sz:",value = 0),
              selectInput("parallelType", "parallel.type:",
                          c("SOCK","MPI","NWS")),
              radioButtons("mxDiff", "mx.diff:",
                           c("True" = TRUE,
                             "False" = FALSE)),
              conditionalPanel(
                condition = "input.method == 'gsva'",
                numericInput("tau1","tau:",value = 1)
              ),
              conditionalPanel(
                condition = "input.method == 'ssgsea'",
                numericInput("tau2","tau:",value = 0.25)
              ),
              conditionalPanel(
                condition = "input.method == 'zscore' || input.method == 'plage'"
              ),
              radioButtons("ssgseaNorm", "ssgsea.norm:",
                           c("True" = TRUE,
                             "False" = FALSE)),
              radioButtons("verbose", "verbose:",
                           c("True" = TRUE,
                             "False" = FALSE))
            )
          )
        )
      )
    )
  )
}

mainpage <- mainPanel(
  id = 'userdefined_mainpage',
  width = 9,
  # uiOutput(outputId='userdefined_maintabs')

  fluidPage(
    fluidRow(
      column(9,
             plotOutput("user_out_plot",height = '100%',width = 'auto') 
      ),
      # column(3,
      #        parametersDataInput("parametersInput") 
      # )
      parametersDataInput("parametersInput")
    )
  )
)

tab_08_user_defined$ui <- sidebarLayout(sidebar, mainpage)

tab_08_user_defined$server <- function(input, output,session) {
  cat("========================= Start DIY ======================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  # browser()
  observe({
    shinyjs::toggleState("userdefined_goButton", 
                         !is.null(input$upload_fileid) && input$upload_fileid != "" )
  })
  
  observe({
    shinyjs::toggleState("diy_dl_plot", 
                         !is.null(input$plotoutputname) && input$plotoutputname != "" )
  })
  
  observe({
    
    observeEvent(input$reset, {
      output.graphic$plot <- NULL
      reset('upload_fileid')
      # output$user_out_plot <- NULL
    })
    
    input.funcid <- reactive({
      cat("=====> user selected function: ", input$diy_funid, "\n")
      inputFuncid  <- input$diy_funid
      return(inputFuncid)
    })
    # 展示示例图
    # Cutpoint Analyses
    if(input.funcid() == 'Cutpoint Analysis'){
      example_file <- reactive({
        read.table(file = "example_files/example_input_cutpoint.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F,encoding = "UTF-8",fileEncoding = 'UTF-8')
      })
      # 展示示例图
      {
        input.color <- input$diy_cf_colour
        input.outcome <- input$diy_cf_outcome
        
        # 输入文件要求：前三列分别为sample_id,times,status;第四列开始是分析变量
        tmp.vars <- colnames(example_file())[4:ncol(example_file())]
        
        plot.list <- list()
        for (var in tmp.vars) {
          cat("demo - current vars: ",var,"\n")
          # browser()
          tmp.data <- example_file() %>%
            dplyr::select(sample_id,times,status,var)
          # 阈值分析
          cutoff.out.tabl <- do.call(rbind,lapply(c(var),func.cutpointcalc,tmp.data))
          # 对分析结果绘图
          cutoff.out.plot <- lapply(c(var), func.cutpointplot,cutoff.out.tabl,toupper(input.outcome),input.color)
          
          plot.list[var] = cutoff.out.plot
        }
        
        # p.out <- do.call("grid.arrange", c(plot.list, ncol = 2))
        p.out <- ggarrange(plotlist = plot.list,
                           ncol = 2,
                           nrow = ceiling(length(plot.list)/2),
                           labels = 'AUTO',
                           widths = 2 *500, 
                           heights = ceiling(length(plot.list)/2) *300 )
        output.width <- 2 *500
        output.height <- ceiling(length(plot.list)/2) *300
        
        output$user_out_plot <- renderPlot({
          print(p.out) + 
            draw_label("Example Plot", color = "#00a064", size = 100, angle = 45)
          # geom_text(
          #   data = data.frame(x = 0.5,
          #                     y = 0.5, label = "Example Plot"),
          #   aes(x, y, label = label),
          #   hjust = 0.5, vjust = 0.5, angle = 45, size = 100/.pt,
          #   color = "#00a064",
          #   inherit.aes = FALSE)
        },width = output.width,height = output.height)
        
      }
    }
    # Kaplan-Meier Curve
    if(input.funcid() == 'Kaplan-Meier Curve'){
      example_file <- reactive({
        read.table(file = "example_files/example_input_km.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F,encoding = "UTF-8",fileEncoding = 'UTF-8')
      })
      # 展示示例图
      {
        input.color <- c(input$diy_colorg1_id,input$diy_colorg2_id)
        # input.title <- input$diy_km_title
        input.outcome <- input$diy_km_outcome
        
        tmp.vars <- colnames(example_file())[4:ncol(example_file())]
        
        plot.list <- list()
        for (var in tmp.vars) {
          cat("demo - current vars: ",var,"\n")
          tmp.data <- example_file() %>%
            dplyr::select(sample_id,times,status,risk = var)
          p <- func.survivalplot(tmp.data,
                                 var,#input.title,
                                 input.outcome,
                                 time.break = time_break_survival(max(tmp.data$times,na.rm = T)),
                                 tbl.loc = 'topright',
                                 color.dis = input.color,
                                 y.title = case_when(input.outcome == 'PFS' ~ 'Progression-free Survival',
                                                     input.outcome == 'DFS' ~ 'Disease-free Survival',
                                                     input.outcome == 'PFI' ~ 'Progression-free Interval',
                                                     input.outcome == 'DFI' ~ 'Disease-free Interal',
                                                     input.outcome == 'DSS' ~ 'Disease-specific Survival',
                                                     input.outcome == 'OS' ~ 'Overall Survival',
                                                     TRUE ~ 'Survival'))
          plot.list[var] = p
        }
        
        # p.out <- do.call("grid.arrange", c(plot.list, ncol = 2))
        p.out <- ggarrange(plotlist = plot.list,
                           ncol = 2,
                           nrow = ceiling(length(plot.list)/2),
                           labels = 'AUTO',
                           widths = 2 *500, 
                           heights = ceiling(length(plot.list)/2) *300 )
        output.width <- 2 *500
        output.height <- ceiling(length(plot.list)/2) *300
        
        output$user_out_plot <- renderPlot({
          print(p.out) + 
            draw_label("Example Plot", color = "#00a064", size = 100, angle = 45)
          # geom_text(
          #   data = data.frame(x = 0.5,
          #                     y = 0.5, label = "Example Plot"),
          #   aes(x, y, label = label),
          #   hjust = 0.5, vjust = 0.5, angle = 45, size = 100/.pt,
          #   color = "#00a064",
          #   inherit.aes = FALSE)
        },width = output.width,height = output.height)
      }
    }
    # Subgroup Analysis
    if(input.funcid() == 'Subgroup Analysis') {
      example_file <- reactive({
        read.table(file = "example_files/example_input_subgroup.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F,encoding = "UTF-8",fileEncoding = 'UTF-8')
      })
      # 展示示例图
      {
        # 因子化
        clc.dat <- example_file() %>%
          dplyr::select(sample_id,times,status) 
        col.names <- colnames(example_file())[4:ncol(example_file())]
        for(i in seq(1,length(col.names))){
          # i = 1
          var = col.names[i]
          # 字符串 转 因子化
          tmp.df <- example_file() %>%
            dplyr::select(obj.var = var) 
          if (is.character(class(tmp.df$obj.var))) {
            tmp.df$obj.var <- factor(tmp.df$obj.var,levels = unique(tmp.df$obj.var))
          }
          colnames(tmp.df) <- var
          clc.dat <- bind_cols(clc.dat,tmp.df)
        }
        
        interaction.vars <- col.names[-1] #  第一个是分组因素：risk
        sub.df <- clc.dat %>%
          dplyr::mutate(risk = factor(risk,labels = c(0,1))) %>% # 0为ref组
          as.data.frame()
        cox.fit <- coxph(Surv(times,status) ~ risk,data=sub.df)
        sub.out <- subgroupAnalysis(cox.fit,sub.df,treatment="risk",subgroups=interaction.vars) 
        # 将亚组分析结果整理成森林图绘制数据
        data.df <- data.frame(Group = sub.out$subgroups,
                              Characteristic = as.character(sub.out$level),
                              Size = as.numeric(ifelse(is.na(sub.out$sample_1),0,sub.out$sample_1)) + as.numeric(ifelse(is.na(sub.out$sample_0),0,sub.out$sample_0)),
                              HR = round(sub.out$HazardRatio,2),
                              L95 = round(sub.out$Lower,2),
                              U95 = ifelse(sub.out$Upper == Inf,50,round(sub.out$Upper,2)),
                              P.value = round(sub.out$Pvalue,4),
                              P.interaction = round(sub.out$pinteraction,4)) %>%
          dplyr::mutate(HR95CI = paste0(HR,'(',L95,'-',U95,')')) 
        # 筛选出结果为NA的记录，并剔除
        factors.with.na <- unique(data.df %>% dplyr::filter(is.na(P.value)) %>%.$Group)
        data.withoutna.df <- data.df %>%
          dplyr::filter(!Group %in% factors.with.na)
        
        table.df <- do.call(rbind,lapply(unique(data.withoutna.df$Group),func.table4subplot2,data.df))
        table.df$P.value <- ifelse(table.df$P.value < 0.001,'<0.001',as.character(table.df$P.value))
        # 绘图
        # Add blank column for the forest plot to display CI.
        table.df <- table.df %>%
          tibble::add_column(` ` = paste(rep(" ", 25), collapse = " "),.after = 'Size') 
        table.df$se <- (log(table.df$U95) - log(table.df$HR))/1.96
        
        # NA to blank
        table.df$Size <- ifelse(is.na(table.df$Size), "", table.df$Size)
        table.df$HR95CI<- ifelse(is.na(table.df$HR95CI), "", table.df$HR95CI)
        table.df$P.value <- ifelse(is.na(table.df$P.value), "", table.df$P.value)
        table.df$P.interaction <- ifelse(is.na(table.df$P.interaction), "", table.df$P.interaction)
        
        colnames(table.df)[7:9] <- c('HR (95% CI)','P value','P interaction')
        
        # Define theme
        tm <- forest_theme(base_size = input$subgroup_base_size,
                           refline_col = input$subgroup_refline_col,
                           footnote_cex = 0.9,
                           footnote_col = input$subgroup_footnote_col)
        p.sub <- forestploter::forest(table.df[,c(1:3,7:9)],
                                  est = table.df$HR,
                                  lower = table.df$L95, 
                                  upper = table.df$U95,
                                  sizes = table.df$se,
                                  ci_column = 3,
                                  ref_line = 1,
                                  arrow_lab = c(levels(clc.dat$risk)[1],levels(clc.dat$risk)[2]),
                                  xlim = c(0, 4),
                                  ticks_at = c(0,0.5, 1, 2, 3, 4),
                                  footnote = "This is the demo data.",
                                  theme = tm)
        
        output.width <- input$subgroup_width #1000
        output.height <- input$subgroup_height #23 * nrow(table.df)
        
        output$user_out_plot <- renderPlot({
          print(as.ggplot(p.sub)) + 
            geom_text(
              data = data.frame(x = 0.5,
                                y = 0.5, label = "Example Plot"),
              aes(x, y, label = label),
              hjust = 0.5, vjust = 0.5, angle = 45, size = 100/.pt,
              color = "#00a064",
              inherit.aes = FALSE)
        },width = output.width,height = output.height)
      }
    }
    # Correlation Analyses
    if(input.funcid() == 'Correlation Analysis'){
      example_file <- reactive({
        read.table(file = "example_files/example_input_corr_x.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F)
      })
      example_file2 <- reactive({
        read.table(file = "example_files/example_input_corr_y.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F)
      })
      # 展示示例图
      {
        
        # 要求dat.x和dat.y有相同的行数
        dat.x <- example_file()[,-1]
        dat.y <- example_file2()[,-1]
        corr.df <- psych::corr.test(dat.x,dat.y,use="complete",method = input$diy_cor_method,adjust = 'fdr')
        corr.long <- func.psychcorrwide2long(corr.df)
        
        colorg1 <- input$diy_cor_colorg1_id
        colorg2 <- input$diy_cor_colorg2_id
        if(input$diy_corr_figure_type == 'bubble'){
          cat(cat_prefix_tme_corr,"-绘制相关系数气泡图\n")
          p.corr <- func.bubbleplot(corr.long,
                                    color1 = colorg1,
                                    color2 = colorg2,
                                    xlevel = colnames(dat.y),
                                    ylevel = colnames(dat.x))
        }
        if(input$diy_corr_figure_type == 'theatmap'){
          cat(cat_prefix_tme_corr,"-绘制相关系数三角热图\n")
          p.corr <- func.triangleheatmap(corr.long,
                                         color1 = colorg1,
                                         color2 = colorg2)
        }
        if(input$diy_corr_figure_type == 'cheatmap'){
          # corrplot只支持对称数据绘图
          cat(cat_prefix_tme_corr,"-绘制普通相关系数热图\n")
          # cor(tmp.df, use = "everything", method = 'spearman')
          corrplot(corr.df$r, 
                   method = input$diy_cor_shape,
                   is.corr = FALSE, 
                   # col.lim  = c(-1,1),
                   col = COL2('PRGn'),
                   # addCoef.col = coefcol, 
                   # tl.col = 'red', 
                   # tl.pos = 'd',
                   p.mat = corr.df$p,
                   sig.level = '0.05',
                   order = 'hclust',
                   addgrid.col = 'grey', 
                   number.cex = 1)
          ## grab the scene as a grid object & save it to P1
          grid.echo()
          p.corr <- grid.grab()
          p.corr <- as.ggplot(p.corr) 
          
        }
        
        output.width <-   ncol(dat.y) * 38  # input$subgroup_width #1000
        output.height <-  ncol(dat.x) * 40 # input$subgroup_height #23 * nrow(table.df)
        
        output$user_out_plot <- renderPlot({
          print(p.corr) + 
            draw_label("Example Plot",x = ncol(dat.y)/2, y = ncol(dat.x)/2,
                       color = "#00a064", size = 60, angle = 45)
            # geom_text(
            #   data = data.frame(x = ncol(dat.y)/2,
            #                     y = ncol(dat.x)/2, label = "Example Plot"),
            #   aes(x, y, label = label),
            #   hjust = 0.5, vjust = 0.5, angle = 45, size = 100/.pt,
            #   color = "#00a064",
            #   inherit.aes = FALSE)
          
        },width = output.width,height = output.height)
      }
    }
    # GSVA
    if(input.funcid() == 'GSVA'){
      example_file <- reactive({
        read.table(file = "example_files/example_input_gsva.txt", sep = "\t", header = T, stringsAsFactors = F, check.names = F)
      })
    }
    
    observeEvent(input$showdemo, {
      output$example_input <- renderDT(example_file(), rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
    })
    
    observeEvent(input$showdemo_y, {
      output$example_input_y <- renderDT(example_file2(), rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
    })
  })
  

  # 业务层
  observeEvent(input$userdefined_goButton,{
    cat("===================== Server DIY =======================\n")
    
    # 输出到页面
    # output$userdefined_maintabs <- renderUI({
    #   tabPanel(
    #     title = name,
    #     uiOutput(paste0(name,"_mut"))
    #   )
    # })
    
    # 获取页面输入
    # browser()
    input.funcid <- input$diy_funid
    input.file   <- input$upload_fileid$datapath
    input.data <- data.table::fread(input.file,encoding = "UTF-8")
    
    output.width = 768
    output.height = 400
    
    if(input.funcid == 'Cutpoint Analysis'){
      
      input.color <- c(input$diy_cf_colour)
      input.outcome <- input$diy_cf_outcome
      
      tmp.vars <- colnames(input.data)[4:ncol(input.data)]
      
      plot.list <- list()
      for (var in tmp.vars) {
        cat("demo - current vars: ",var,"\n")
        tmp.data <- input.data %>%
          dplyr::select(sample_id,times,status,var)
        # 阈值分析
        cutoff.out.tabl <- do.call(rbind,lapply(c(var),func.cutpointcalc,tmp.data))
        # 对分析结果绘图
        cutoff.out.plot <- lapply(c(var), func.cutpointplot,cutoff.out.tabl,toupper(input.outcome),input.color)
        
        plot.list[var] = cutoff.out.plot
      }
      
      # p.out <- do.call("grid.arrange", c(plot.list, ncol = 2))
      p.out <- ggarrange(plotlist = plot.list,
                         ncol = 2,
                         nrow = ceiling(length(plot.list)/2),
                         labels = 'AUTO',
                         widths = 2 *500, 
                         heights = ceiling(length(plot.list)/2) *300 )
      output.width <- 2 *500
      output.height <- ceiling(length(plot.list)/2) *300
      output.graphic[["userdef"]]$plot <- p.out
    }
    
    if(input.funcid == 'Kaplan-Meier Curve'){
      
      input.color <- c(input$diy_colorg1_id,input$diy_colorg2_id)
      # input.title <- input$diy_km_title
      input.outcome <- input$diy_km_outcome
      
      tmp.vars <- colnames(input.data)[4:ncol(input.data)]
      
      plot.list <- list()
      for (var in tmp.vars) {
        cat("current vars: ",var,"\n")
        tmp.data <- input.data %>%
          dplyr::select(sample_id,times,status,risk = var)
        p <- func.survivalplot(tmp.data,
                               var,#input.title,
                               input.outcome,
                               time.break = time_break_survival(max(tmp.data$times,na.rm = T)),
                               tbl.loc = 'topright',
                               color.dis = input.color,
                               y.title = case_when(input.outcome == 'PFS' ~ 'Progression-free Survival',
                                                   input.outcome == 'DFS' ~ 'Disease-free Survival',
                                                   input.outcome == 'PFI' ~ 'Progression-free Interval',
                                                   input.outcome == 'DFI' ~ 'Disease-free Interal',
                                                   input.outcome == 'DSS' ~ 'Disease-specific Survival',
                                                   input.outcome == 'OS' ~ 'Overall Survival',
                                                   TRUE ~ 'Survival'))
        plot.list[var] = p
      }
      
      # p.out <- do.call("grid.arrange", c(plot.list, ncol = 2))
      p.out <- ggarrange(plotlist = plot.list,
                         ncol = 2,
                         nrow = ceiling(length(plot.list)/2),
                         labels = 'AUTO',
                         widths = 2 *500, 
                         heights = ceiling(length(plot.list)/2) *300 )
      output.width <- 2 *500
      output.height <- ceiling(length(plot.list)/2) *300
      output.graphic[["userdef"]]$plot <- p.out
    }
    
    if(input.funcid == 'Subgroup Analysis'){
      
      # 因子化
      clc.dat <- input.data%>%
        dplyr::select(sample_id,times,status) 
      col.names <- colnames(input.data)[4:ncol(input.data)]
      for(i in seq(1,length(col.names))){
        # i = 1
        var = col.names[i]
        # 字符串 转 因子化
        tmp.df <- input.data %>%
          dplyr::select(obj.var = var) 
        if (is.character(class(tmp.df$obj.var))) {
          tmp.df$obj.var <- factor(tmp.df$obj.var,levels = unique(tmp.df$obj.var))
        }
        colnames(tmp.df) <- var
        clc.dat <- bind_cols(clc.dat,tmp.df)
      }
      
      interaction.vars <- col.names[-1] #  第一个是分组因素：risk
      sub.df <- clc.dat %>%
        dplyr::mutate(risk = factor(risk,labels = c(0,1))) %>% # 0为ref组
        as.data.frame()
      cox.fit <- coxph(Surv(times,status) ~ risk,data=sub.df)
      sub.out <- subgroupAnalysis(cox.fit,sub.df,treatment="risk",subgroups=interaction.vars) 
      # 将亚组分析结果整理成森林图绘制数据
      data.df <- data.frame(Group = sub.out$subgroups,
                            Characteristic = as.character(sub.out$level),
                            Size = as.numeric(ifelse(is.na(sub.out$sample_1),0,sub.out$sample_1)) + as.numeric(ifelse(is.na(sub.out$sample_0),0,sub.out$sample_0)),
                            HR = round(sub.out$HazardRatio,2),
                            L95 = round(sub.out$Lower,2),
                            U95 = ifelse(sub.out$Upper == Inf,50,round(sub.out$Upper,2)),
                            P.value = round(sub.out$Pvalue,4),
                            P.interaction = round(sub.out$pinteraction,4)) %>%
        dplyr::mutate(HR95CI = paste0(HR,'(',L95,'-',U95,')')) 
      # 筛选出结果为NA的记录，并剔除
      factors.with.na <- unique(data.df %>% dplyr::filter(is.na(P.value)) %>%.$Group)
      data.withoutna.df <- data.df %>%
        dplyr::filter(!Group %in% factors.with.na)
      
      table.df <- do.call(rbind,lapply(unique(data.withoutna.df$Group),func.table4subplot2,data.df))
      table.df$P.value <- ifelse(table.df$P.value < 0.001,'<0.001',as.character(table.df$P.value))
      # 绘图
      # Add blank column for the forest plot to display CI.
      table.df <- table.df %>%
        tibble::add_column(` ` = paste(rep(" ", 25), collapse = " "),.after = 'Size') 
      table.df$se <- (log(table.df$U95) - log(table.df$HR))/1.96
      
      # NA to blank
      table.df$Size <- ifelse(is.na(table.df$Size), "", table.df$Size)
      table.df$HR95CI<- ifelse(is.na(table.df$HR95CI), "", table.df$HR95CI)
      table.df$P.value <- ifelse(is.na(table.df$P.value), "", table.df$P.value)
      table.df$P.interaction <- ifelse(is.na(table.df$P.interaction), "", table.df$P.interaction)
      
      colnames(table.df)[7:9] <- c('HR (95% CI)','P value','P interaction')
      
      # Define theme
      tm <- forest_theme(base_size = input$subgroup_base_size,
                         refline_col = input$subgroup_refline_col,
                         footnote_cex = 0.9,
                         footnote_col = input$subgroup_footnote_col)
      p.out <- forestploter::forest(table.df[,c(1:3,7:9)],
                                est = table.df$HR,
                                lower = table.df$L95, 
                                upper = table.df$U95,
                                sizes = table.df$se,
                                ci_column = 3,
                                ref_line = 1,
                                arrow_lab = c(levels(clc.dat$risk)[1],levels(clc.dat$risk)[2]),
                                xlim = c(0, 4),
                                ticks_at = c(0,0.5, 1, 2, 3, 4),
                                footnote = "This is the demo data.",
                                theme = tm)
      
      output.width <- input$subgroup_width #1000
      output.height <- input$subgroup_height #23 * nrow(table.df)
      output.graphic[["userdef"]]$plot <- p.out
    }
    
    if(input.funcid == 'Correlation Analysis'){
      # 第二个file
      if(!is.null(input$upload_fileid_y)){
        input.file_y   <- input$upload_fileid_y$datapath
        input.data_y <- data.table::fread(input.file_y,encoding = "UTF-8")
      }else{
        input.data_y <- input.data
      }
      # 要求dat.x和dat.y有相同的行数
      dat.x <- input.data[,-1]
      dat.y <- input.data_y[,-1]
      corr.df <- psych::corr.test(dat.x,dat.y,use="complete",method = input$diy_cor_method,adjust = 'fdr')
      corr.long <- func.psychcorrwide2long(corr.df)
      
      colorg1 <- input$diy_cor_colorg1_id
      colorg2 <- input$diy_cor_colorg2_id
      if(input$diy_corr_figure_type == 'bubble'){
        cat(cat_prefix_tme_corr,"-绘制相关系数气泡图\n")
        p.corr <- func.bubbleplot(corr.long,
                                  color1 = colorg1,
                                  color2 = colorg2,
                                  xlevel = colnames(dat.y),
                                  ylevel = colnames(dat.x))
      }
      if(input$diy_corr_figure_type == 'theatmap'){
        cat(cat_prefix_tme_corr,"-绘制相关系数三角热图\n")
        p.corr <- func.triangleheatmap(corr.long,
                                       color1 = colorg1,
                                       color2 = colorg2)
      }
      if(input$diy_corr_figure_type == 'cheatmap'){
        # corrplot只支持对称数据绘图
        cat(cat_prefix_tme_corr,"-绘制普通相关系数热图\n")
        # cor(tmp.df, use = "everything", method = 'spearman')
        corrplot(corr.df$r, 
                 method = input$diy_cor_shape,
                 is.corr = FALSE, 
                 # col.lim  = c(-1,1),
                 col = COL2('PRGn'),
                 # addCoef.col = coefcol, 
                 # tl.col = 'red', 
                 # tl.pos = 'd',
                 p.mat = corr.df$p,
                 sig.level = '0.05',
                 order = 'hclust',
                 addgrid.col = 'grey', 
                 number.cex = 1)
        ## grab the scene as a grid object & save it to P1
        grid.echo()
        p.corr <- grid.grab()
        p.corr <- as.ggplot(p.corr) 
        
      }
      
      output.width <-   ncol(dat.y) * 38  # input$subgroup_width #1000
      output.height <-  ncol(dat.x) * 40 # input$subgroup_height #23 * nrow(table.df)
      output.graphic[["userdef"]]$plot <- p.corr
    }
    
    
    # 展示
    output$user_out_plot <- renderPlot({
      cat("Class:",class(output.graphic[["userdef"]]$plot),"\n")
      print(output.graphic[["userdef"]]$plot) 
    },width = output.width,height = output.height)
    
    
    # 下载 
    output[['diy_dl_plot']] <- downloadHandler(
      filename = function() {
        paste0(prefix_output_file,"_",input$plotoutputname,"_",Sys.Date(), '.',input$plottype, sep = "")
      },
      content = function(file) {
        if(grepl("png$",file)){
          png(file,width = input$plotwidth, height = input$plotheigh,units = 'in',res = 300)
          print(output.graphic[['userdef']]$plot)
          dev.off()
        }else {
          pdf(file,onefile = F, width = input$plotwidth, height = input$plotheigh)
          print(output.graphic[['userdef']]$plot)
          dev.off()
        }
        
      }
    )
  })
  
  
  
  
}
