generateSyntax <- function(xvar = NULL, yvar = NULL, itemvars = NULL, 
                           compvar = NULL, compType = NULL, type = "composite",
                           estimator = "ML", fixed.x = TRUE, se = "standard",
                           bootstrap = 1000){
  
  mplus_Itemabbr <- abbreviate(itemvars, minlength = 4)
  mplus_compabbr <- abbreviate(compvar, minlength = 4)
  
  invvar <- ""
  lvcompvar <- paste0("lv", compvar)
  
  ## Latent Variable Syntax
  if (compType == "Sum Score"){
    
    lavLVsyntax <- paste0(
      lvcompvar, 
      ifelse(type == "composite", " =~ 1*", " =~ NA*"), 
      paste(itemvars, collapse = ifelse(
        type == "composite", " + 1*", " + ")),
      "\n", lvcompvar, " ~ 0*1",
      ifelse(type == "composite",
             paste0("\n", lvcompvar, " ~~ ", lvcompvar),
             paste0("\n", lvcompvar, " ~~ 1*", lvcompvar))
      )
    
    mplusLVsyntax <- paste0(
      mplus_compabbr, " BY ",
      paste(mplus_Itemabbr, collapse = ifelse(
        type == "composite", "@1 ", "* ")
      ), ifelse(type == "composite", "@1; \n", "*; \n"),
      "\t[", mplus_compabbr, "@0]; ",
      ifelse(type == "composite",
             paste0(mplus_compabbr, ";"),
             paste0(mplus_compabbr, "@1;"))
    )
    
    mplusDefinesyntax <- ""
    
  } else if (compType == "Mean Score"){
    
    nItems <- length(itemvars)
    
    lavLVsyntax <- paste0(
      lvcompvar, 
      ifelse(type == "composite", paste0(" =~ (1/", nItems,")*"), " =~ NA*"), 
      paste(itemvars, collapse = ifelse(
        type == "composite", paste0(" + (1/", nItems,")*"), " + ")),
      "\n", lvcompvar, " ~ 0*1",
      ifelse(type == "composite",
             paste0("\n", lvcompvar, " ~~ ", lvcompvar),
             paste0("\n", lvcompvar, " ~~ 1*", lvcompvar)))
    
    mplusLVsyntax <- paste0(
      mplus_compabbr, " BY ",
      paste(mplus_Itemabbr, collapse = ifelse(
        type == "composite", paste0("@", round(1/nItems, 6) , " "), "* ")
      ), ifelse(type == "composite", 
                paste0("@", round(1/nItems, 6), "; \n"),
                "*; \n"),
      "\t[", mplus_compabbr, "@0]; ",
      ifelse(type == "composite",
             paste0(mplus_compabbr, ";"),
             paste0(mplus_compabbr, "@1;"))
    )
    
    mplusDefinesyntax <- ""
    
  } else if (compType == "Difference Score"){
    
    lavLVsyntax <- paste0(
      lvcompvar, " =~ 1*", itemvars[1],
      "\n", itemvars[1], " ~ 1*", itemvars[2]
    )
    
    mplusLVsyntax <- paste0(
      mplus_compabbr, " BY ", mplus_Itemabbr[1], "@1;\n",
      "\t", mplus_Itemabbr[1], " ON ", mplus_Itemabbr[2], "@1;"
    )
    
    mplusDefinesyntax <- ""
    
  } else if (compType == "Product Score"){
    
    lavLVsyntax <- ""
    
    mplusLVsyntax <- paste0(
      mplus_compabbr, " | ", mplus_Itemabbr[1], 
      " ON ", mplus_Itemabbr[2], ";"
    )
    
    mplusDefinesyntax <- ""
    
  } else if (compType == "Ratio Score"){
    
    lavLVsyntax <- ""
    
    invvar <- paste0("i.", mplus_Itemabbr[2])
    mplusLVsyntax <- paste0(
      mplus_compabbr, " | ", mplus_Itemabbr[1], 
      " ON ", invvar, ";"
    )
    
    mplusDefinesyntax <- paste0(
      "DEFINE:\n",
      "\t", invvar, " = (1 / ", mplus_Itemabbr[2], ");\n\n"
    )
    
  }
  
  ## Item Syntax
  if (compType == "Sum Score" | compType == "Mean Score"){
    
    lavItemsyntax <- paste(
      sapply(itemvars, function(x){
        paste0(x, ifelse(type == "composite",
                         " ~~ theta*",
                         " ~~ "), x)
      }),
      collapse = "\n")
    
    mplusItemsyntax <- paste(
      sapply(mplus_Itemabbr, function(x){
        paste0("\t", x, ifelse(type == "composite",
                         " (theta);",
                         ";"))
      }),
      collapse = "\n")
    
  } else if (compType == "Difference Score"){
    
    lavItemsyntax <- paste(
      sapply(itemvars, function(x){
        paste0(x, " ~~ ", x)
      }),
      collapse = "\n")
    
    mplusItemsyntax <- paste(
      sapply(mplus_Itemabbr, function(x){
        paste0("\t", x, ";")
      }),
      collapse = "\n")
    
  } else if (compType == "Product Score"){
    
    lavItemsyntax <- ""
    
    mplusItemsyntax <- paste(
      sapply(mplus_Itemabbr, function(x){
        paste0("\t", x, ifelse(type == "composite",
                               " (theta);",
                               ";"))
      }),
      collapse = "\n")
    
  } else if (compType == "Ratio Score"){
    
    lavItemsyntax <- ""
    
    mplusItemsyntax <- paste(
      sapply(c(mplus_Itemabbr[1], invvar), function(x){
        paste0("\t", x, ifelse(type == "composite",
                               " (theta);",
                               ";"))
      }),
      collapse = "\n")
    
  }
  
  lavItemMedsyntax <- paste(
    sapply(itemvars, function(x){
      paste0(x, " ~ 0*", xvar, "; ", yvar, " ~ 0*", x)
    }),
    collapse = "\n")
  
  ## Combine Final Syntax
  if (compType != "Product Score" & compType != "Ratio Score"){
    
    lavaan_syntax <- paste0(
      "# Latent Variable\n", 
      lavLVsyntax,
      
      "\n\n# Item Variables\n", 
      lavItemsyntax,
      
      "\n\n# Constrain Item Mediations\n", 
      lavItemMedsyntax,
      
      "\n\n# A Path\n", 
      lvcompvar," ~ a*", xvar,
      
      "\n\n# B Path\n", 
      yvar," ~ b*", lvcompvar,
      
      "\n\n# Direct Effect\n", 
      yvar," ~ c*", xvar,
      
      "\n\n# Computed Parameters\n",
      "indirect := a*b\n",
      "total := c + (a*b)"
    )
    
  } else {
    lavaan_syntax <- paste0("Warning: Lavaan does not currently support the ability to model ", 
                            tolower(compType), 
                            "s.\nPlease see the Mplus syntax to run these models.")
  }
  
  mplus_syntax <- list()
  temp <- paste0(
    "TITLE: Latent Variable Mediation: ", mplus_compabbr," (", compType, ")\n\n",
    "DATA: FILE = ./path/to/file/mediation.dat;\n\n",
    "VARIABLE: \n", 
    "\tNAMES = ", paste(c("id", xvar, yvar, mplus_Itemabbr, 
                          paste0("ov", mplus_compabbr)), 
                        collapse = " "), ";\n",
    "\tUSEVARIABLES = ", 
    ifelse(compType == "Ratio Score",
      paste(c(xvar, yvar, mplus_Itemabbr[1], invvar), 
                               collapse = " "),
      paste(c(xvar, yvar, mplus_Itemabbr), 
            collapse = " ")), ";\n",
    "\tMISSING = .;\n\n",
    ifelse(compType == "Product Score" | compType == "Ratio Score",
           mplusDefinesyntax, ""),
    "ANALYSIS:\n",
    "\tESTIMATOR = ", estimator, ";",
    ifelse(se == "bootstrap", paste0("\n\tBOOTSTRAP = ", boostrap, ";\n"), ""),
    ifelse(compType != "Product Score" | compType != "Ratio Score",
           "\n\tTYPE = RANDOM;\n\n", "\n\n"),
    "MODEL:\n",
    "\t", mplusLVsyntax, "\n\n",
    mplusItemsyntax, "\n\n",
    "\t", mplus_compabbr, " ON ", xvar, " (a);\n",
    "\t", yvar, " ON ", mplus_compabbr, " (b);\n",
    "\t", yvar, " ON ", xvar, " (c);\n",
    ifelse(fixed.x, paste0("\t", xvar,";\n\n"), "\n")
  )
  
  mplus_syntax$modifications <- paste0(
    temp,
    "OUTPUT:\n",
    "\tSTDYX;\n", "\tMODINDICES (ALL);"
  )
  
  mplus_syntax$indirect <- paste0(
    temp,
    "MODEL CONSTRAINT:\n",
    "\tnew(indirect total);\n",
    "\tindirect = a * b;\n",
    "\ttotal = c + (a * b);\n\n",
    "OUTPUT:\n",
    "\tSTDYX;"
  )
  
  return(list(lavaan_syntax = lavaan_syntax, 
              mplus_syntax = mplus_syntax))
}

lvMediation <- function(model = NULL, data = NULL, 
                        idvar = NULL, xvar = NULL, yvar = NULL,
                        itemvars = NULL, compvar = NULL,
                        estimator = "ML", missing = "FIML",
                        fixed.x = TRUE, se = "standard",
                        bootstrap = 1000){
  
  fit <- lavaan::sem(model = model, data = data, estimator = estimator,
                     missing = missing, fixed.x = fixed.x, se = se,
                     bootstrap = bootstrap)
  
  return(fit)
}









