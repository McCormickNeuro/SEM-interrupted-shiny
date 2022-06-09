nytimes_theme <- function(...) {
  
  ## Colors — stick with the ggplot2() greys
  c_bg    <- "white"
  c_grid  <- "grey80"
  c_btext <- "grey5"
  c_mtext <- "grey30"
  
  # Begin construction of chart
  ggplot2::theme_bw(base_size = 12, base_family = "Helvetica") +
    
    # Region
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = c_bg, color = c_bg),
      plot.background  = ggplot2::element_rect(fill = c_bg, color = c_bg),
      panel.border     = ggplot2::element_rect(color = c_bg)
    ) +
    
    # Grid
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = c_grid, size = .25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank()
    ) +
    
    # Legend
    ggplot2::theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.key           = ggplot2::element_rect(fill = NA, color = NA),
      legend.background    = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.text          = ggplot2::element_text(color = c_mtext)
    ) +
    
    # Titles, labels, etc.
    ggplot2::theme(
      plot.title     = ggplot2::element_text(
        color = c_btext,
        vjust = 1.25,
        face = "bold",
        size = 18
      ),
      axis.text      = ggplot2::element_text(size = 10, color = c_mtext),
      axis.title.x   = ggplot2::element_text(
        size = 12,
        color = c_mtext,
        hjust = 0.5
      ),
      axis.title.y   = ggplot2::element_text(
        size = 12,
        color = c_mtext,
        hjust = 0.5
      )
    ) +
    # Facets
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = c_bg, color = c_btext),
      strip.text = ggplot2::element_text(size = 10, color = c_btext)
    ) +
    
    # Plot margins
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
    
    # Additionals
    ggplot2::theme(...)
}

## Individual-Item Mediation
#medDat <- read.csv("~/Dropbox/Professional/projects/donders/sem-interrupted/2.scripts/SEM-interrupted-shiny/data/example02-10items.csv", header = TRUE)
#medDat <- read.csv("~/Dropbox/Professional/projects/donders/sem-interrupted/2.scripts/SEM-interrupted-shiny/data/example01-2items.csv", header = TRUE)
#medDat$M <- colMeans(medDat[,4:13], na.rm = TRUE)
#medDat$M <- colMeans(medDat[,4:5], na.rm = TRUE)

itemMediation <- function(data = NULL, idvar = NULL, 
                          xvar = NULL, yvar = NULL,
                          itemvars = NULL, compvar = NULL,
                          estimator = "ML", missing = "ML",
                          fixed.x = TRUE, se = "standard",
                          bootstrap = 1000){
  
  fits <- lapply(c(itemvars, compvar), function(mvar){
    model <- paste0("
                    # A Path
                    ", mvar," ~ a*", xvar,"
                    
                    # B Path
                    ", yvar," ~ b*", mvar,"
                    
                    # Direct Effect
                    ", yvar," ~ c*", xvar,"
                    
                    # Mediator Variance
                    ", mvar," ~~ mVar*", mvar,"
                    
                    # Computed Parameters
                    indirect := a*b
                    prop := (b * mVar) / a
                    total := c + (a*b)
                    ")
    lavaan::sem(model = model, data = data, estimator = estimator,
                missing = missing, fixed.x = fixed.x, se = se,
                bootstrap = bootstrap)
  })
  
  names(fits) <- c(itemvars, compvar)
  
  return(fits)
}
#fits <- itemMediation(data = medDat, idvar = "id", xvar = "X", yvar = "Y", itemvars = names(medDat)[4:13], compvar = "M")
#fits <- itemMediation(data = medDat, idvar = "id", xvar = "X", yvar = "Y", itemvars = names(medDat)[4:5], compvar = "M")

tableParameters <- function(fits = NULL){
  `%>%` <- magrittr::`%>%`
  
  paramTabs <- lapply(fits, function(x){
    lavaan::standardizedSolution(x) %>%
      dplyr::filter(label != "")
  })
  
  return(paramTabs)
}
#paramTabs <- tableParameters(fits = fits)

plotItemMediation <- function(paramTabs = NULL, compvar = NULL){
  
  `%>%` <- magrittr::`%>%`
  effects <- c("a", "b", "c", "mVar", "indirect", "prop", "direct")

  gList <- lapply(effects, function(eff){
    effs <- data.frame(
      y = sapply(paramTabs, '[[', "est.std")[which(effects == eff),],
      ymin = sapply(paramTabs, '[[', "ci.lower")[which(effects == eff),],
      ymax = sapply(paramTabs, '[[', "ci.upper")[which(effects == eff),])
    
    effs <- effs %>%
      dplyr::mutate(
        type = ifelse(rownames(effs) == compvar,
                      "composite", "item"),
        x1 = ifelse(rownames(effs) == compvar, 0,
                    stats::reorder(rownames(effs), 
                            abs(effs[compvar,"y"] - effs$y))),
        x2 = ifelse(rownames(effs) == compvar, 0,
                    abs(match(rownames(effs), 
                              sort(rownames(effs))) -
                        max(match(rownames(effs), 
                                  sort(rownames(effs)))))),
        var = rownames(effs)
      )
    
    # g <- ggplot2::ggplot(effs) +
    #   ggplot2::geom_pointrange(
    #     mapping=ggplot2::aes(x=stats::reorder(var, x1), 
    #                          y=y, 
    #                          ymin=ymin,
    #                          ymax=ymax, 
    #                          color=type)) + 
    #   ggplot2::coord_flip() + 
    #   ggplot2::geom_hline(yintercept = 0, 
    #                       color = "black", 
    #                       linetype="dashed") +
    #   ggplot2::labs(y=paste0(stringr::str_to_sentence(eff),
    #                          " Effect Estimate"), 
    #                 x="Item") + 
    #   ggplot2::theme(legend.position = 'none') + 
    #   ggsci::scale_color_npg() + 
    #   nytimes_theme(legend.position='none')
    q <- ggplot2::ggplot(effs) +
      ggplot2::geom_pointrange(
        mapping=ggplot2::aes(x=stats::reorder(var, x2), 
                             y=y, 
                             ymin=ymax,
                             ymax=ymin, 
                             color=type)) + 
      ggplot2::coord_flip() + 
      ggplot2::geom_hline(yintercept = 0, 
                          color = "black", 
                          linetype="dashed") +
      ggplot2::labs(y=paste0(stringr::str_to_sentence(eff),
                             " Effect Estimate"), 
                    x="Item") + 
      ggplot2::theme(legend.position = 'none') + 
      ggsci::scale_color_npg() + 
      nytimes_theme(legend.position='none')
    
    return(q)
  })
  
  pe <- paramTabs[[compvar]][paramTabs[[compvar]]$label == "prop", "est.std"]
  se <- paramTabs[[compvar]][paramTabs[[compvar]]$label == "prop", "se"]
  props <- sapply(paramTabs, '[[', "est.std")[which(effects == "prop"),]
  bounds <- props > (pe + 10*se) | props < (pe - 10*se)
  library(patchwork)
  # gq = ((gList[[1]]$g + ggplot2::labs(title="Items by Effect Discrepancy") +
  #          ggplot2::theme(plot.title=ggplot2::element_text(size=14))) | 
  #         gList[[2]]$g | 
  #         gList[[4]]$g |
  #         gList[[5]]$g) / 
  #   ((gList[[1]]$q + 
  #       ggplot2::ggtitle("Items by Name") + 
  #       ggplot2::labs(title="Items by Name") +
  #       ggplot2::theme(plot.title=ggplot2::element_text(size=14))) | 
  #      gList[[2]]$q | 
  #      gList[[4]]$q |
  #      gList[[5]]$q)
  g <- gList[[1]] | 
    gList[[2]] | 
    gList[[5]] | 
    (gList[[6]] + 
       ggplot2::coord_flip(ylim = c((pe - (10*se)), (pe + 10*se))) +
       ggplot2::labs(y = ifelse(any(bounds),
         "Proportional Effect Estimate\n(some estimates outside the frame)",
         "Proportional Effect Estimate"
         )))
  
  g <- g +
    plot_annotation(title = "Distribution of Mediation Effects",
                    theme = ggplot2::theme(
                      plot.title = ggplot2::element_text(
                        size = 18,
                        face = "bold"))) & 
    ggplot2::theme(plot.background = ggplot2::element_rect(
      fill = "white", color = "gray95"))
  
  return(g)
  
}
#paramTabs <- paramTabs[c(1:4,7:9,11)]
#testplot <- plotItemMediation(paramTabs = paramTabs, compvar = "M")
#testplot

proportionalTest <- function(data = NULL, idvar = NULL,
                             xvar = NULL, yvar = NULL,
                             itemvars = NULL, compvar = NULL,
                             estimator = "ML", missing = "ML",
                             fixed.x = TRUE, se = "standard",
                             bootstrap = 1000){
  
  `%>%` <- magrittr::`%>%`
  
  M.model <- paste0("
                    # A Path
                    ", compvar," ~ a*", xvar,"
                    
                    # B Covariance
                    ", yvar," ~~ b*", compvar,"
                    
                    # Direct Effect
                    ", yvar," ~ c*", xvar,"
                    
                    # Computed Parameters
                    prop := b / a
                    ")
  M.fit <- lavaan::sem(model = M.model, data = data, estimator = estimator,
                       missing = missing, fixed.x = fixed.x, se = se,
                       bootstrap = bootstrap)
  M.prop <- lavaan::parameterEstimates(M.fit) %>%
    dplyr::filter(label == "prop")
  
  item.model.free <- paste0("
    # A Paths
    ", paste(
      sapply(1:length(itemvars), function(x){
        paste0(itemvars[x], " ~ a", x, "*", xvar)
      }),
      collapse = "\n"),"
      
    # B Covariances
    ", paste(
      sapply(1:length(itemvars), function(x){
        paste0(yvar, " ~~ b", x, "*", itemvars[x])
      }),
      collapse = "\n"),"
      
    # Direct Effect
    ", yvar," ~ c*", xvar,"
    
    # Computed Parameters
    ", paste(
      sapply(1:length(itemvars), function(x){
        paste0("prop", x, " := b", x, " / a", x)
      }),
      collapse = "\n"),"
  ")
  free.fit <- lavaan::sem(model = item.model.free, data = data, estimator = estimator,
                          missing = missing, fixed.x = fixed.x, se = se,
                          bootstrap = bootstrap)
  
  item.model.const <- paste0("
    # A Paths
    ", paste(
      sapply(1:length(itemvars), function(x){
        paste0(itemvars[x], " ~ a", x, "*", xvar)
      }),
      collapse = "\n"),"
      
    # B Covariances
    ", paste(
      sapply(1:length(itemvars), function(x){
        paste0(yvar, " ~~ b", x, "*", itemvars[x])
      }),
      collapse = "\n"),"
      
    # Direct Effect
    ", yvar," ~ c*", xvar,"
    
    # Computed Parameters
    ", paste(
      sapply(1:length(itemvars), function(x){
        paste0("prop", x, " := b", x, " / a", x)
      }),
      collapse = "\n"),"
    ", paste(
      sapply(1:length(itemvars), function(x){
        paste0("a", x, " == b", x, " / ", M.prop$est)
      }),
      collapse = "\n"),"
  ")
  const.fit <- lavaan::sem(model = item.model.const, data = data, estimator = estimator,
                           missing = missing, fixed.x = fixed.x, se = se,
                           bootstrap = bootstrap)
  
  LRT <- lavaan::lavTestLRT(free.fit, const.fit)
  MIs <- lavaan::modindices(const.fit, sort = TRUE, minimum.value = 5)
  
  return(list(med.fit = M.fit,
              free.fit = free.fit,
              constrain.fit = const.fit,
              LRT = LRT,
              MIs = MIs))
}
#tests <- propTest(data = medDat, idvar = "id", xvar = "X", yvar = "Y", itemvars = names(medDat)[4:13], compvar = "M")



