library(data.table)
load("/data/sPlot/releases/sPlot2.1/sPlot_header_20161124.RData")
load("/data/sPlot/releases/sPlot2.1/DT2_20161025.RData")



###run Header fix
affiliations <- fread("../_sPlot_Management/Affiliations_20180704.csv")
databases <- fread("../_sPlot_Management/Databases.out_20180704.csv")
sel <- fread("_data/Resampled1.csv")$x

header.ress <- header.fix %>%
  filter(PlotID %in% sel)


res.source <- header.fix %>%
  dplyr::select(PlotID, Dataset, `GIVD ID`) %>%
  filter(PlotID %in% sel) %>%
  group_by(`GIVD ID`) %>% 
  summarise(num.plots.res=n()) %>%
  distinct() %>%
  left_join(databases %>%
              filter(label!="BIOTA_South_Africa_2") %>% ### two dataset labels for the same GIVD
              dplyr::select(`GIVD ID`, Custodian, `Deputy custodian`), by="GIVD ID") %>%
  left_join(header.fix %>% 
              group_by(`GIVD ID`) %>%
              summarise(tot.plots=n()), by="GIVD ID") %>%
  left_join(affiliations %>% 
              filter(Sequence_affiliations==1) %>%
              dplyr::select(Name, `E-Mail`) %>%
              rename(Custodian=Name), by="Custodian") %>%
  rename(`Custodian E-Mail`=`E-Mail`) %>%
  left_join(affiliations %>% 
              filter(Sequence_affiliations==1) %>%
              dplyr::select(Name, `E-Mail`) %>%
              rename(`Deputy custodian`=Name), by="Deputy custodian") %>%
  rename(`Deputy custodian E-Mail`=`E-Mail`) %>%
  left_join(header.fix %>%
              dplyr::select(`GIVD ID`, Dataset) %>%
              filter(Dataset!="BIOTA_South_Africa_2") %>% ### two dataset labels for the same GIVD
              distinct(), by="GIVD ID") %>%
  mutate(perc.plots=round(num.plots.res/tot.plots*100,1)) %>%
  dplyr::select(`GIVD ID`,Dataset, num.plots.res, tot.plots, perc.plots, Custodian, `Deputy custodian`,
                `Custodian E-Mail`, `Deputy custodian E-Mail`)# %>%
  arrange(desc(perc.plots))
  

  

fwrite(res.source[,1:6], "_output/resampling_source_custodian_perc.csv")
  

#### create figure to compare plot density before and after resampling in environmental space



  


