#' Make data from dataset received 2020/09/29
#' 
#' proj. predictCandida
#' PI: Taiga Miyazaki
#' author: Shimpei Morimoto
#' 




dir.sub <- "./sub"

for(i in 1:length(list.files(dir.sub))) 
  source(
    file = sprintf(
      "./sub/%s",
      list.files(dir.sub)[i]
      )
    )

df.col_info <-
  readxl::read_excel(
    path = path.orig_data,
    sheet=2
    )

df.ADS <-
  readxl::read_excel(
    path = path.orig_data,
    sheet=1,
    skip=3,
    col_names = df.col_info$col_names,
    col_types = df.col_info$col_type
    ) %>%
  mutate(
    AKI= 
      ifelse(is.na(AKI), 0, AKI), 
    CVC_rem_24hrs= 
      ifelse(is.na(CVC_rem_24hrs), 1, CVC_rem_24hrs)
    )

save(
  df.ADS,df.col_info,
  file = sprintf(
    "%s/%s", dir.ADS, fn.ADS
    )
  )