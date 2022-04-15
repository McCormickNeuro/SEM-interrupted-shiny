tableMIs <- function(fitObj = NULL,
                     standardize=TRUE,
                     std.nox=FALSE,
                     pretty = TRUE){
  
  table <- lavaan::modindices(object = fitObj, sort = TRUE) %>% 
    dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, 3))
  
  if (pretty){
    table <- table %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(term = paste(c(lhs, op, rhs), collapse=" ")) %>%
      dplyr::select(-lhs, -op, -rhs) %>%
      dplyr::relocate(term)
    
    colnames(table) <- c("Parameter","MI","EPC","EPC Std. LV",
                         "EPC Std. All","EPC Std. NoX")
    table <- table[,c(rep(TRUE,3), standardize, standardize, std.nox)]
  } else {
    table <- table[,c(rep(TRUE,5), standardize, standardize, std.nox)]
  }
  
  return(table)
}

plotTidyMIs <- function(fitObj = NULL,
                        xName = NULL, 
                        yName = NULL,  
                        mName = NULL,
                        itemNames = NULL,
                        nItems = NULL,
                        filter = NULL){
  library(dplyr)
  
  layout <- matrix(NA, 
                   nrow = 5, 
                   ncol = (ceiling(nItems) - ceiling(nItems) %% 2) + 1)
  
  layout[5, 1] <- paste0(xName)
  layout[5, ncol(layout)] <- paste0(yName)
  layout[3, median(1:ncol(layout))] <- paste0("lv", mName)
  itemseq <- seq(1, ncol(layout), by = 1)
  if ((nItems %% 2) == 0) itemseq <- itemseq[-median(1:ncol(layout))]
  for (x in 1:length(itemseq)){layout[1, itemseq[x]] <- itemNames[x]}
  
  tidyPlot <- tidySEM::prepare_graph(fitObj,
                                     layout = layout,
                                     rect_width = 1,
                                     rect_height = 1,
                                     ellipses_width = 1.25,
                                     ellipses_height = 1.25,
                                     variance_diameter = 0.5,
                                     angle = 180)
  
  tidySEM::nodes(tidyPlot) %>%
    dplyr::mutate(colour = "black",
                  fill = "grey95",
                  label_fill = NA) -> tidySEM::nodes(tidyPlot)
  
  tidySEM::edges(tidyPlot) %>%
    dplyr::mutate(colour = "black",
                  label_colour = "grey90",
                  label_fill = "white",
                  linetype = 1,
                  size = 0.5,
                  alpha = ifelse(est == "0.00", 0, 0.1),
                  label = replace(label, label == "0.00", ""),
                  label_location = 0.5
    ) -> tidySEM::edges(tidyPlot)
  
  miTable <- tableMIs(fitObj = fitObj,
                      standardize = TRUE,
                      std.nox = FALSE,
                      pretty = FALSE)
  
  miTable <- miTable[miTable$mi > 5.00,]
  
  if (nrow(miTable) > 0){
    x <- miTable$mi
    miTable$normMI <- (2 - 0.1)*((x - min(x)) / (max(x) - min(x))) + .5
    
    if (filter == "itemMed"){
      miTable <- miTable[
        (miTable$rhs == xName |
           miTable$lhs == yName) & 
          miTable$op == "~",
      ]
    }
    
    tidySEM::edges(tidyPlot) %>%
      add_row(
        from = miTable$rhs, rhs = miTable$rhs,
        to = miTable$lhs, lhs = miTable$lhs, op = miTable$op,
        arrow = ifelse(miTable$op == "~~", "both", "last"),
        label = paste0(miTable$mi),
        connect_from = ifelse(miTable$rhs %in% itemNames, "bottom", "top"),
        connect_to = ifelse(miTable$lhs %in% itemNames, "bottom", "top"),
        est = paste0(miTable$mi), se = "0.00", pval = NA, 
        confint = "[0.00, 0.00]", est_sig = paste0(miTable$mi), 
        est_std = paste0(miTable$sepc.all), se_std = "0.00", pval_std = NA, 
        confint_std = "[0, 0]", est_sig_std = paste0(miTable$mi),
        label_results = "TEST", lavaan_label = "", show = TRUE,
        label_colour = "black", label_fill = "white", 
        label_location = runif(length(miTable$mi), (1/3), (2/3)),
        curvature = 75,
        colour = "red",
        linetype = 1,
        size = miTable$normMI,
        alpha = 1
      ) -> tidySEM::edges(tidyPlot)
  }
  
  return(tidyPlot)
}





#xName = "Cause"; yName = "Outcome"; mName = "LatentVar"; itemNames = names(sem.suppress)[1:10]; nItems = 10; fitObj = LVmediator.fit; filter = "itemMed"
# temp <- tidySEM::edges(tidyPlot)
#plot(tidyPlot)












