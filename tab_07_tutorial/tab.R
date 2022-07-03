#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-18
# @IDE     : RStudio
# @Desc    : 第7个Tab功能：Tutorial
#===============================================================================

tab_07_tutorial <- list()

tab_07_tutorial$ui <- fluidPage(
  id = 'tutorial_mainpage',
  width = 10,
  tags$style(HTML(
    "h3 {
          font-size: 30px;
          font-weight: bold;
          font-family: 'Yusei Magic', sans-serif;
       }
    h4 {
          font-size: 20px;
          font-weight: bold;
          font-family: 'Yusei Magic', sans-serif;
        }
     p  {
          font-size: 15px;
          font-family: 'Yusei Magic', sans-serif;
          color: black;
     }
    img {
        display: block;
        margin-left: auto;
        margin-right: auto;
    }
    ")
  ),
  
  fluidRow(
    h3("Welcome to IMPACT"),
    tags$p("This tool provides access to a pan-cancer related transcriptomic and somatic mutation at different public databases including TCGA, CPTAC, GEO and Papers' supplementary file.", 
           style = "color:black;"),
    tags$p("Different functions can be accessed using the menu bar (the green strip) at the top of the screen.",
           style = "color:black;"),
    img(src = "tutorial-0.png", style = "margin-left:0px;margin-right:0px;margin-bottom:10px;width:80%;"),
    # br(),
    tags$p("The 'Home' option in the menu bar will return to the profiles tool homepage.",
           style = "color:black;")
  ),
  hr(),
  fluidRow(
    h4("PredExplore: Immunotherapy Predictive Biomarkers Exploration"),
    tags$p("PredExplore allows users to explore the value of interested genes as 
           immunotherapy biomarkers across multiple cohorts. 
           Here, we included 18 cohorts of 11 cancer types. As shown in **Figure 1a** and **1b**, 
           users need to fill in the name of interested genes and select a cancer type from the list. 
           The cohort will be stratified during the analysis into the mutated and wild-type groups based on the input genes. 
           Then, the results of Kaplan-Meier curves (PFI and/or OS) and boxplots (TMB level, 
           Neoantigen expression, and PD-L1 expression) will be partially or fully generated depending on the available information (as shown in Figure 1c and 1e). 
           When one item of the result list (Figure 1c) is clicked, 
           corresponding figures will be shown in Figure 1e. 
           All the generated results and figures can be downloaded by clicking the buttons shown in Figure 1d.", 
           style = "color:black;"),
    img(src = "tour_icip.png",style = "width: 80%;")
  ),
  hr(),
  fluidRow(
    h4("ProgPredictor"),
    tags$p("The tool allows you to look at a potential prognostic biomarker of cancer in multi-cohorts at the same time.", 
           style = "color:black;"),
    img(src = "tour_prog.png",style = "width: 80%;")
  ),
  hr(),
  fluidRow(
    h4("Kaplan-Meier Estimate"),
    tags$p("The aim of this function is to analyis the different of survival analysis between two groups. The following methods were used to survival analyses:
           Kaplan-Meier plots to visualize survival curves;
           Log-rank test to compare the survival curves of two groups;", 
           style = "color:black;"),
    img(src = "tour_km.png",style = "width: 80%;")
  ),
  hr(),
  fluidRow(
    h4("Cox Regression"),
    tags$p("Here's a function that contains an Cox proportional hazards regression function and forestplot implementation. The forestplot of Cox analysis was implemented by the rewrited 'ggforest' function in survminer R package.", 
           style = "color:black;"),
    img(src = "tour_cox.png",style = "width: 80%;")
  ),
  hr(),
  fluidRow(
    h4("Immunogenicity Analyses"),
    tags$p("In this tab, the continuous immune-related biomarkers were colllected and the differences between two groups were visualized by boxplot.", 
           style = "color:black;"),
    img(src = "tour_genicity.png",style = "width: 80%;")
  ),
  hr(),
  fluidRow(
    h4("TME Correaltion Analyses"),
    tags$p("The tumor microenvironment (TME), consist of all cells and components associated with a neoplasm that are not transformed cells. The correlations between selected genes and immunomodulator or pathways or signature were showed by bubble or heatmap plot.", 
           style = "color:black;"),
    img(src = "tour_corr.png",style = "width: 80%;")
  ),
  hr(),
  fluidRow(
    h4("TME Comparative Analyses"),
    tags$p("The expression of selected immunomodulator or pathways or signature between two groups were showed by heatmap plot.", 
           style = "color:black;"),
    img(src = "tour_comp.png",style = "width: 80%;")
  ),
  hr(),
  fluidRow(
    h4("Tumor Mutation Profile"),
    tags$p("The Tumor Mutation Profile function analyzes the mutation status of genes selected by the user in different tumor cohorts.", 
           style = "color:black;"),
    img(src = "tour_mutation.png",style = "width: 80%;")
  )
)

