

# Cox分析森林图
func.ggforestplot <-function(plot.dat,
                             main = "Hazard ratio",
                             cpositions=c(0.02, 0.22, 0.4),
                             fontsize = 0.8){
  # 修改 surviminer包的ggforest函数进行绘图
  # 注意联系作者（przemyslaw.biecek@gmail.com 和 fabian.scheipl@@gmail.com）获取同意，并致谢
  # plot.dat为一个dataframe，包含Characteristic，Level，HR，L95，U95，HR95CI，P，estimate，conf.low，conf.high，stars
  
  # {
  #   upper <-  exp(5)
  #   lower <-  exp(0.1)
  #    
  #   plot.dat <- plot.dat %>%
  #     mutate(conf.high_out = ifelse(exp(conf.high) > upper, upper, NA),
  #            conf.low_out = ifelse(exp(conf.low) < lower, lower, NA))
  # }
  # 上限大与5的用5表示，
  rangeb <- range(plot.dat$conf.low, plot.dat$conf.high, na.rm = TRUE)
  breaks <- axisTicks(rangeb/2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  # make plot twice as wide as needed to create space for annotations
  rangeplot[1] <- rangeplot[1] - diff(rangeb)
  # increase white space on right for p-vals:
  rangeplot[2] <- rangeplot[2] + .15 * diff(rangeb)
  
  width <- diff(rangeplot)
  # y-coordinates for labels:
  y_variable <- rangeplot[1] +  cpositions[1] * width
  y_nlevel <- rangeplot[1]  +  cpositions[2] * width
  y_cistring <- rangeplot[1]  +  cpositions[3] * width
  y_stars <- rangeb[2]
  x_annotate <- seq_len(nrow(plot.dat))
  
  # geom_text fontsize is in mm (https://github.com/tidyverse/ggplot2/issues/1828)
  annot_size_mm <- fontsize *
    as.numeric(grid::convertX(unit(theme_get()$text$size, "pt"), "mm"))
  
  #flip order
  plot.dat <- plot.dat[nrow(plot.dat):1, ]
  
  p <- ggplot(plot.dat, aes(seq_along(Characteristic), exp(estimate))) +
    geom_rect(aes(xmin = seq_along(Characteristic) - .5, xmax = seq_along(Characteristic) + .5,
                  ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
                  fill = ordered(seq_along(Characteristic) %% 2 + 1))) +
    scale_fill_manual(values = c("#FFFFFF33", "#00000033"), guide = "none") +
    geom_point(pch = 15, size = 4) +
    # 上限大与5的用5表示，
    geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.15) +
    
    # geom_point(aes(y = conf.high_out + 1.02), shape = 24,
    #            position = position_dodge(width = 0.2), size = 4, show.legend = F) +
    # geom_point(aes(y = conf.low_out - 0.013), shape = 25,
    #            position = position_dodge(width = 0.2), size = 4, show.legend = F) +
    # scale_y_log10() +
    # coord_cartesian(ylim = c(lower, upper)) + 
  
    geom_hline(yintercept = 1, linetype = 3) +
    coord_flip(ylim = exp(rangeplot)) +
    ggtitle(main) +
    scale_y_log10(
      name = "",
      labels = sprintf("%g", breaks),
      expand = c(0.02, 0.02),
      breaks = breaks) +
    theme_light() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          panel.border=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    annotate(geom = "text", x = x_annotate, y = exp(y_variable),
             label = plot.dat$Characteristic, fontface = "bold", hjust = 0,
             size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_nlevel), hjust = 0,
             label = plot.dat$Level, vjust = 0.5, size = annot_size_mm) +
    # annotate(geom = "text", x = x_annotate, y = exp(y_nlevel),
    #          label = plot.dat$Size, fontface = "italic", hjust = 0,
    #          vjust = ifelse(plot.dat$level == "", .5, 1.1),
    #          size = annot_size_mm) +
    # annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
    #          label = plot.dat$estimate.1, size = annot_size_mm,
    #          vjust = ifelse(plot.dat$estimate.1 == "reference", .5, -0.1)) +
    annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
             label = plot.dat$HR95CI, size = annot_size_mm,
             vjust = 0.5,  fontface = "italic") +
    annotate(geom = "text", x = x_annotate, y = exp(y_stars),
             label = plot.dat$stars, size = annot_size_mm,
             hjust = -0.2,  fontface = "italic")
  # gt <- ggplot_gtable(ggplot_build(p))
  # gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # cox.plot <- ggpubr::as_ggplot(gt)
  return(p)
}