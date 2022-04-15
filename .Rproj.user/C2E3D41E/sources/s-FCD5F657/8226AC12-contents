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
#medDat <- read.csv("~/Dropbox/Professional/projects/donders/sem-interrupted/2.scripts/SEM-interrupted/data/example02-10items.csv", header = TRUE)
#medDat$M <- colMeans(medDat[,4:13], na.rm = TRUE)

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
                    
                    # Computed Parameters
                    indirect := a*b
                    prop := b / a
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

itemCOVMediation <- function(data = NULL, idvar = NULL, 
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
                    ", yvar," ~~ b*", mvar,"
                    
                    # Direct Effect
                    ", yvar," ~ ", xvar,"
                    
                    # Computed Parameters
                    prop := b / a
                    ")
    lavaan::sem(model = model, data = data, estimator = estimator,
                missing = missing, fixed.x = fixed.x, se = se,
                bootstrap = bootstrap)
  })
  
  names(fits) <- c(itemvars, compvar)
  
  return(fits)
}


tableParameters <- function(fits = NULL){
  `%>%` <- magrittr::`%>%`
  
  paramTabs <- lapply(fits, function(x){
    lavaan::standardizedSolution(x) %>%
    #lavaan::parameterEstimates(x) %>%
      dplyr::filter(label != "")
  })
  
  return(paramTabs)
}
#paramTabs <- tableParameters(fits = fits)

plotItemMediation <- function(paramTabs = NULL, compvar = NULL){
  
  `%>%` <- magrittr::`%>%`
  effects <- c("a", "b", "c", "indirect", "prop", "direct")

  gList <- lapply(effects, function(eff){
    effs <- data.frame(
      y = sapply(paramTabs, '[[', "est.std")[which(effects == eff),],
      #y = sapply(paramTabs, '[[', "est")[which(effects == eff),],
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
    
    g <- ggplot2::ggplot(effs) +
      ggplot2::geom_pointrange(
        mapping=ggplot2::aes(x=stats::reorder(var, x1), 
                             y=y, 
                             ymin=ymin,
                             ymax=ymax, 
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
    q <- ggplot2::ggplot(effs) +
      ggplot2::geom_pointrange(
        mapping=ggplot2::aes(x=stats::reorder(var, x2), 
                             y=y, 
                             ymin=ymin,
                             ymax=ymax, 
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
    
    return(list(g = g, q = q))
  })
  
  library(patchwork)
  gq = ((gList[[1]]$g + ggplot2::labs(title="Items by Effect Discrepancy") +
           ggplot2::theme(plot.title=ggplot2::element_text(size=14))) | 
          gList[[2]]$g | 
          gList[[4]]$g |
          gList[[5]]$g) / 
    ((gList[[1]]$q + 
        ggplot2::ggtitle("Items by Name") + 
        ggplot2::labs(title="Items by Name") +
        ggplot2::theme(plot.title=ggplot2::element_text(size=14))) | 
       gList[[2]]$q | 
       gList[[4]]$q |
       gList[[5]]$q)
  
  gq = gq +
    plot_annotation(title = "Distribution of Mediation Effects",
                    theme = ggplot2::theme(
                      plot.title = ggplot2::element_text(
                        size = 18,
                        face = "bold"))) & 
    ggplot2::theme(plot.background = ggplot2::element_rect(
      fill = "white", color = "gray95"))
  
  return(gq)
  
}
#testplot <- plotItemMediation(paramTabs = paramTabs, compvar = "M")
