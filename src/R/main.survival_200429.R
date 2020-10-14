#' Make KM plot.. 
#' Shimpei Morimoto
#' 2020/04/30
#' 
#' reference: https://daviddalpiaz.github.io/r4sl/elastic-net.html
#' 

source(file = "./sub/settings_200429.R")

load(
  file = sprintf(
    "%s/%s", dir.Output, fn.RData
    )
  )

SurvObj    <- Surv(time = ADS$time, event = ADS$d30_surv)
var_x_plot <- c("mPBS_0_4_8")

# fit (Kaplan-Meier plot ) ---------------------------------------------------------------------

tmp_formula_plot <-
  as.formula(
    sprintf(
      "%s ~ %s",
      "SurvObj",
      paste(var_x_plot,  collapse = " + ")
    )
  )


km.fit <- surv_fit( # survminer::surv_fit() https://github.com/kassambara/survminer/issues/283
  tmp_formula_plot, 
  data= ADS
  )

quantile(km.fit,0.5)

quartz(
  file = sprintf(
    "%s/%s.pdf",dir.Output,fn.KMcurve
    ),
  type  = "pdf",
  width  = 7,
  height = 7
  )
ggsurvplot(
  fit = km.fit,
  data = ADS, 
  risk.table = TRUE, 
  conf.int = TRUE,
  conf.int.style = "ribbon",
  break.x.by = 5,
  fun="pct",
  xlim = c(0, 35),
  xlab = "Days from onset of candidemia",
  ylab = "Survival probability (%)",
  legend.title = "mPBS",
  legend.labs=c(
    "0-3" ,"4-7", "≥8"
    )
  )
dev.off()