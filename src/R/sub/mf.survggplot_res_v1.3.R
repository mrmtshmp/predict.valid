## proj: 18_BrS_GWAS
## crea: 180406
## disc: 
##
## 
## 


mf.survggplot <- function(
  data,
  SNPs = c("rs11708996", "rs10428132"),
  gt = list(c("CC","GC","GG"), c("GG", "GT", "TT")),
  gt_levels = list(c("CC/GC","CC/GC","GG"), c("GG/GT", "GG/GT", "TT")),
  surv= c("time","status"),
  cov = c("gender","Family_history"),
  conf.int=TRUE,
  x.label = "Years since 20th birthday",
  y.label = "Survival probability (%)",
  break.x.by = 20,
  fun="pct",
  plot.theme = theme_bw()
  ){
  
  
  cols <- brewer.pal(6, "Paired")
  
  
  data$DUMMY <- 1
  
  if(is.null(cov)){
    cov <-  "DUMMY"
    }
  if(is.null(SNPs)){
    SNPs <- "DUMMY"
  }
  
  data_01 <- inner_join(
    
    data[,c("ID",surv)], 
    data[,c("ID",cov)]) %>%
    inner_join(
      data[,c("ID",SNPs)]
      )

  
  print(colnames(data_01))
  
  options(scipen=FALSE)
  
  # handling missingness -----------------------------------------------------------------
  
  data_01 <- data_01[complete.cases(data_01),]

  # Categorizing genotypes -----------------------------------------------------------------
  
  data_surv <- data_01

  for (i in 1:length(SNPs)){
    
    data_surv <- data_surv %>%
      
      mutate(
        targ = eval(
          parse(text=SNPs[i])
          )
        )
    
      data_surv$new  <- gt_levels[[i]][
        match(
          data_surv$targ,
          gt[[i]]
          )
        ]
      
      data_surv[,SNPs[i]] <- data_surv$new
      
      data_surv[,paste(SNPs[i],"_ind",sep="")] <- as.numeric(
        factor(
          data_surv$new,
          levels = unique(gt_levels[[i]]),
          labels = 1 : length(unique(gt_levels[[i]]))
          )
        )
      
      data_surv  <- data_surv %>%
        dplyr::select(-new, -targ)
  }
  

  # adding surv variable  -----------------------------------------------------------------
  
  data_surv$SurvObj <- with(
    data_surv, 
    Surv(time,status)
    )
  
  # formula -----------------------------------------------------------------

  var_x      <- c()
  var_x_plot <- c()
  
  for(i in 1:length(SNPs)){
    
    
    data_surv[, "ind_var"] <- data_surv[,paste(SNPs[i],"_ind",sep="")]
    
    if(
      length(
        unique(
          data_surv$ind_var
          )
        )  == 1
      ){
      var_x = "1"} else
      { 
        var_x[i]      = paste(SNPs[i],"_ind",sep="")
        var_x_plot[i] = SNPs[i]
      }
  }
  
  data_surv  <- data_surv %>%
    dplyr::select(-ind_var)
  
  var_x      <- append(var_x, cov)
  var_x_plot <- append(var_x_plot, cov)
  
  tmp_formula      <- as.formula(
    sprintf(
      "%s ~ %s",
      "SurvObj",
      paste(var_x,  collapse = " + ")
      )
    )
  
  tmp_formula_plot <- try(
    as.formula(
      sprintf(
        "%s ~ %s",
        "SurvObj",
        paste(var_x_plot,  collapse = " + ")
        )
      ), silent  =TRUE
    )
  
  print(tmp_formula)
  print(tmp_formula_plot)
  
  
  # Design matrix of analysis data ---------------------------------------------------------------------
  
  dd <- try(
    datadist(
      data_surv %>% 
        dplyr::select(
          -SurvObj
          ),
      adjto.cat="mode"
      )
    )
  
  options(datadist="dd")
  
  print(dd)
  
  
  # fit (log-rank test) ---------------------------------------------------------------------
  
  # a log rank test is equivalent to score statistics p-value from CoxPH with the factor as covariate.
  # c.f. FH test for trend : https://cran.r-project.org/web/packages/FHtest/FHtest.pdf
  #      
  coxph.fit <- coxph(
    tmp_formula,
    data= data_surv,
    model = TRUE
    ) %>%
    try()
  
  print(confint(coxph.fit))
  
  logrank_p_from_coxph <- 1- pchisq(
    coxph.fit$score, 1
    ) %>%
    round(8) %>%
    try()
  
  # fit (Kaplan-Meier plot ) ---------------------------------------------------------------------
  
  km.fit <- surv_fit( # survminer::surv_fit() https://github.com/kassambara/survminer/issues/283
    tmp_formula_plot, 
    data= data_surv
    )
  
  quantile(km.fit,0.5)
  
  # plotting ----------------------------------------------------------------
  
  # survminer::ggsurvplot()
  #
  # Github: https://github.com/kassambara
  #
  
  surv_ggplot <-  ggsurvplot(
    km.fit,
    conf.int = conf.int,
    data = data_surv,
    palette = c('darkgreen','darkblue','red'), #cols[c(1,3,5,2,4,6)],  #1:length(
#      unique( data_surv$gt)
#      ),
#    pval        = round(logrank_p_from_coxph,8) ,
    pval.method = FALSE,
    risk.table  = TRUE, 
    conf.int.style = "ribbon",
    ncensor.plot = FALSE, 
    size = 0.5, 
    linetype =  1, # c(1,2,3,4,5,6),
             #Kaplan-Meier PLOT部分の体裁を調整
    title    = paste(SNPs,collapse="_"),
    subtitle = "",
    caption = "",
    legend.title = "genotype:",
#    font.title = c(30, "plain", "black"),
    #    font.subtitle = c(13, "plain", "#df6d68"),
    #    font.caption = c(10, "italic", "#f6adad"),

    xlab = x.label,
    ylab = y.label,
    fun=fun,

    xlim = c(0, 85),
    break.x.by = break.x.by,

#    font.x = c(30, "plain", "black"),
#    font.y = c(30, "plain", "black"),

#    font.tickslab = c(30, "plain", "black"),
    legend = c(0.9, 0.5),
#    font.legend = c(30, "plain", "black"),
    pval.size = 7,
     #"1","n","sqrtN","S1","S2","FH"が選択可能
    #  log.rank.weights = "FH",
    #  test.for.trend= TRUE,      
    # I cant read the souce code
    # (https://github.com/kassambara/survminer/blob/master/R/surv_pvalue.R)

    #risk table
    risk.table.title = "Number at risk",
    risk.table.y.text.col = TRUE,
    risk.table.y.text = FALSE,
#    fontsize = 30
  )
  return(
    list(
      surv_ggplot, 
      tmp_formula,
      coxph.fit,
      confint(coxph.fit)[1,1],
      confint(coxph.fit)[1,2],
      confint(coxph.fit),
      "Adjust_to"=as.matrix(dd$limits)[2,SNPs]
      )
    )
}


# mf.Simple_input ---------------------------------------------------------


mf.Simple_input <- function(
  data_input,
  SNPs = c("rs11708996","rs12539264"),
  GT   = "CC_or_GC__GG",
  gt = list(c("CC","GC","GG"),c("AA","AG","GG")),
  gt_levels = list(c("CC/GC","CC/GC","GG"), c("AA","AG","GG")),
  cov = c(),
  first  = 1,
  ...
  ){

  
  mf.survggplot_res <- mf.survggplot(
    data = data_input,
    SNPs = SNPs,
    gt = gt,
    gt_levels = gt_levels,
    cov = cov,
    ...
    )
  
  
  mf.survggplot_res[[2]]
  mf.survggplot_res[[3]]
  
  pval <- as.matrix(
    summary(
      mf.survggplot_res[[3]]
    )$coefficients
  ) [, "Pr(>|z|)"]
  
  df <- data.frame(
    "Model"       = paste(mf.survggplot_res[[2]][-1], collapse = "~"),
    c(SNPs,cov),
    "Effect(exp)" = mapply(exp,mf.survggplot_res[[3]][1]),
    "CI_L"          = exp(mf.survggplot_res[[4]]),
    "CI_U"          = exp(mf.survggplot_res[[5]]),
    "Adjusted"    = mf.survggplot_res[[7]],
    "p-value"     = format.pval(pval,digits = 4)
  )
  
  print(df)

  data_result_try <- data.frame()
  
  if(first){
    data_result <- df
  }else{
    
    data_result_try <- try(rbind(data_result[[1]], df))
    
    if (
      class(data_result_try)=="try-error"
      ){
      data_result <- data_result[[1]]
      }else 
        data_result <- data_result_try  %>%
          filter(!is.na(coefficients))
      }
  
  return(
    list(
      data_result ,
      mf.survggplot_res[[1]]
      )
    )
}

