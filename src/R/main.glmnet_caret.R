#' Selecting predictors via L1.5 penalized logistic regression model using caret package. 
#' Shimpei Morimoto
#' 2020/04/30
#' 
#' reference: https://daviddalpiaz.github.io/r4sl/elastic-net.html
#' 

dir.sub <- "./sub"
Bibtex <- TRUE

vec.list.files <- 
  list.files(
   dir.sub,
   pattern = "^mf\\.|.+glmnet_caret.+"
   )

for(i in 1:length(vec.list.files)) 
  source(
    file = sprintf(
      "./sub/%s",vec.list.files[i]
      )
    )

sink("session_info.main.glmnet_caret.R.txt")
sessionInfo()
sink()

load(
  file = sprintf(
    "%s/%s", dir.ADS, fn.ADS
    )
  )


#' reference: https://daviddalpiaz.github.io/r4sl/elastic-net.html

set.seed(429)

#' The initial model
#' 

var_y <- 'd30_surv'
var_x <- unlist(
  df.col_info[
    !is.na(
      df.col_info$pred_cand==1
      ),
    "col_names"
    ]
  )

df.ADS_comp <- 
  df.ADS[
    complete.cases(df.ADS[,c(var_x, var_y)]),
    ] %>%
  data.frame()

df.ADS_comp[,var_y] <-
  factor(df.ADS_comp[,var_y],levels=c(0,1),labels=c("censor","event"))

fml <- sprintf(
  '%s ~ %s',
  var_y,
  paste(var_x,collapse = '+')
  )


mat.model <- 
  model.matrix(
    data = df.ADS_comp,
    as.formula(fml)
    )

elnet_bootTrain = caret::train(
  as.formula(fml),
  data = df.ADS_comp,
  method = "glmnet",
  trControl = bootTrain
  )

adaBootTrain.control <-
  trainControl(
    method = "adaptive_boot",
    summaryFunction = twoClassSummary,
    classProbs = T,
    savePredictions = "final"
  )

df.ADS_comp <- 
  mf.create_weight(
    ADS = df.ADS_comp,
    var.y = "d30_surv",
    outname = "weights",
    y.class = c(1,2)
    )

elnet_adabootTrain = caret::train(
  as.formula(fml),
  data = df.ADS_comp,
  method = "glmnet",
  weights = df.ADS_comp$weights,
  trControl = adaBootTrain.control
  )


en.model <- glmnet(
  x = mat.model,y=df.ADS_comp[,var_y], family = 'binomial',
  lambda = c(elnet_adabootTrain$bestTune$lambda),
  alpha =  c(elnet_adabootTrain$bestTune$alpha),
  weights = df.ADS_comp$weights
  )

# en.model <- glmnet(
#   x = mat.model,y=df.ADS_comp[,var_y], family = 'binomial'
#   )
# 
# plot(en.model,label=TRUE)

sink(
  sprintf(
    "%s/%s",dir.output,"res.glmnet.main.glmnet_caret.txt"
    )
  )
df.col_info[,c("col_names","orig_name")] %>%
  mutate(col_names=sprintf("%s1", col_names)) %>%
  right_join(
    data.frame(
      colnames =
        unlist(
          en.model$beta@Dimnames[[1]][
            en.model$beta@i
            ]
          ),
      beta = en.model$beta@x
    ),
    by=c("col_names"="colnames")
  )
sink()

df.ADS_comp$pred <- 
  predict(
    en.model,mat.model,type="response"
    )

table(
  pred = predict(en.model,mat.model,type="class"),
  act  = df.ADS_comp$d30_surv
  )

ggdata <- data.frame(
  pred = predict(en.model,mat.model),
  oc = df.ADS_comp$d30_surv
  ) %>%
  ggplot(
    aes(x=oc, y=s0)
    )

ggdata.predictiveScores <-
  ggplot(
    data =
      df.ADS_comp,
    aes(
      x = pred
    )
  )

res.roc <- 
  pROC::roc_(
    data=df.ADS_comp,
    predictor="pred", 
    response=var_y
    )
res.ci.auc <-
  ci.auc(res.roc,method = "boot")

quartz(
  type = 'pdf',
  file = sprintf(
    '%s/%s.pdf', dir.output, 
    'roc.predicted_by_ElNet.pdf'
    ),
  pointsize = 20,
  family = "Arial"
)

# Total subjects

pROC::plot.roc(
  main=sprintf(
    "%s
        (survivor=%s; non-survivor=%s)",
    "Total subjects", 
    length(res.roc$cases),
    length(res.roc$controls)
    ),
  res.roc,
  # ci=TRUE,
  # ci.type='bars',
  # ci.col= "black",
  thre.col="blue",
  print.thres.best.method="youden",
  print.thres=TRUE,
  print.thres.col="royalblue",
  print.thres.cex=0.7,
  print.thres.adj=c(-0.4,1),
  segments.lwd= 0.5,
  lwd=4,
  identity.col="black",
  cex.axix = 2.5,
  cex.lab = 1.5, cex.main=0.7
  )

legend(
  x = 0.6, y=0.5,cex = 0.7,
  legend = c(
    sprintf( 
      "AUC = %s 
      0.95 CI: %s - %s",
      round(auc(res.roc),4),
      round(res.ci.auc[1],4),
      round(res.ci.auc[2],4)
      )
  ),
  bty = "n"
)

plot(
  ggdata.predictiveScores + 
    stat_ecdf(
      aes(
        color = 
          as.factor(eval(parse(text=var_y)))
      ),
      # bw="SJ",
      # #      binwidth = FD,
      # alpha=0.5,
      position="identity"
    ) +
    geom_point(
      aes(
        y=as.numeric(eval(parse(text=var_y))) - 1,
        x=pred,
        color=as.factor(eval(parse(text=var_y)))
      ),
      size=0.5
    ) +
    theme_bw()
)
dev.off()

