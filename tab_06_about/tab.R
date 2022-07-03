#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-18
# @IDE     : RStudio
# @Desc    : 第6个Tab功能：About
#===============================================================================

tab_06_about <- list()

tab_06_about$ui <- fluidPage(
  id = 'about_mainpage',
  hr(),
  width = 10,
  uiOutput(outputId='about_maintabs'),
  
  tags$style(HTML(
    "h2 {
          font-size: 30px;
          font-weight: bold;
          font-family: 'Yusei Magic', sans-serif;
        }
     p  {
          font-family: 'Yusei Magic', sans-serif;
        }
    ")
  ),
  
  h2(strong("Introduction"),align="left"),
  tags$p("Immunotherapies, including antibodies to programmed death receptor 1 (PD-1) or 
         its ligand (PD-L1) and CTL-associated protein 4 (CTLA-4), have wide clinical 
         application for a range of solid tumors, but only a subset of patients can benefit from them. 
         A large number of studies have focused on the identification of predictive biomarkers for 
         immunotherapy. IMPACT is a web-based platform for the exploration in immunotherapeutic predictive 
         and cancer prognostic biomarkers using genomic, transcriptomic and proteomic data from the in-house cohorts, 
         literature and public databases, including TCGA, CPTAC, ICGC, and GEO. IMPACT can be used for survival analysis, 
         meta-analysis, interaction analysis, mutational landscape analysis, immunogenicity analysis, and tumor immune microenvironment analysis. 
         Besides of the routine exploration of biomarkers, four key clinical and scientific questions are specially covered by the above analysis: "),
  p("(1) whether mutations in a gene set (e.g. some pathways), or a single gene, have the potential to be a predictive or prognostic biomarker;"),
  p("(2) the meta-analysis of survival analysis across multiple studies;"),
  p("(3) the potential interaction effects of biomarker-biomarker or biomarker-clinical variables on the association with prognosi;"),
  p("(4) whether the biomarker is predictive or prognostic in two-arm clinical trials;"),
  
  br(),
  h2(strong("Acknowledgements"),align="left"),
  tags$p("We give our special thanks to Shiny Gallery for sharing the excellent template. 
         R packages of surviminer were adopted to implement IMPACT. 
         We acknowledge their contributions to our work."),
  
  br(),
  h2(strong("Contact us"),align="left"),
  tags$p("If you have any questions about the IMPACT, please feel free to contact ",
         strong("Zhijie Wang (jie_969@163.com)"),
         ". When describing the issues, please attach the related screenshots and 
         provide the information about the problem as detailed as possible, 
         including the information of the browser you used."
  )
)

