library(tidyverse)
##### code from HELGE BRUELHEIDE

load("/data/sPlot2.0/plot_data.RData") # server directory
# selected pool of plots by Jonathan
str(plot_data)
dim(plot_data) #799400     54

load("c:\\Daten\\iDiv2\\sPlot2\\JonathanLenoir\\plotToRemove.RData")

str(output)
output[[1]]
plotToRemove <- output[[2]]
rm(output)
class(plotToRemove) 
length(plotToRemove[[1]]) # First iteration containing 700037 IDs
for (i in 1:100) {
  plotToRemove[[i]] <- na.omit(plotToRemove[[i]])
}
str(plotToRemove)
# List of 100
str(plotToRemove[[1]]) #atomic [1:700036] 

posit <- match(plotToRemove[[1]], plot_data$PlotID)
# this gives the index for the first set, which we have chosen for paper #2.

plot_data_sel1 <- plot_data[-posit, ]
dim(plot_data_sel1)[1] # 99364 selected relev?s
# and this gives the list of plots selected

#If you repeat this three times
posit <- match(plotToRemove[[2]], plot_data$PlotID)
plot_data_sel2 <- plot_data[-posit, ]
posit <- match(plotToRemove[[3]], plot_data$PlotID)
plot_data_sel3 <- plot_data[-posit, ]
str(plot_data_sel1)
PlotID_run_1_2_3 <- 
  sort(unique(c(plot_data_sel1$PlotObservationID,plot_data_sel2$PlotObservationID, 
                plot_data_sel3$PlotObservationID)))
str(PlotID_run_1_2_3)
# int [1:150116] 16 17 18 20 22 23 26 30 31 41 ...

##### FMS TAKES OVER FROM HERE ##### 
## reimport selected plots
PlotID123 <- read_csv("_data/PlotID_run_1_2_3.csv") %>% 
  rename(PlotObservationID=x)
#load header and fix
load("/data/sPlot/releases/sPlot2.1/sPlot_header_20161124.RData")
source("/data/sPlot/users/Francesco/_sPlot_Management/Fix.header.R")
header <- fix.header(header, exclude.sophy = F)
databases <- read_csv("/data/sPlot/users/Francesco/_sPlot_Management/Consortium/Databases.out.csv")


header.sel <- header %>% 
  filter(PlotObservationID %in% PlotID123$PlotObservationID)



### Data from Stephan - Check for ID matches
header.TV <- read_delim(file = "_data/sPlot-2.1_header.csv", delim = "\t", 
                        col_types = cols(PlotObservationID = col_double(),
                                        PlotID = col_double(),
                                        `TV2 relevé number` = col_double(),
                                        `Original nr in database` = col_character(),
                                        ORIGDB_NR = col_character(),
                                        ORIGDB_NR_1 = col_character(),
                                        ORIG_REL_N = col_character(),
                                        Longitude = col_double(),
                                        Latitude = col_double(),
                                        `Location uncertainty (m)` = col_double(),
                                        Dataset = col_character()))

## import answers from custodians
answers <- read_csv(file="_management/resampling_answers.csv")

header.sel.TV <- header.sel %>% 
  left_join(header.TV %>% 
              dplyr::select(PlotObservationID, `TV2 relevé number`:ORIG_REL_N),
            by="PlotObservationID") %>% 
  dplyr::select(PlotObservationID,  Dataset, `GIVD ID`, Longitude:Latitude, Country, `Location uncertainty (m)`, `TV2 relevé number`, `Original nr in database`, ORIG_REL_N) %>% 
  left_join(databases %>% 
              dplyr::select(`GIVD ID`, Custodian, `Deputy custodian`, `Still in sPlot`))# %>% 
## join answers from google spreadsheet
  #left_join(answers) %>% 
  #filter(`Yes/Conditional/No`!="Yes")

write_csv(header.sel.TV, path="_output/header.sel123_tocheck_withIDs.csv")

### ADD NA-US-002
header.sel.TV.add.NAUS002 <- header.sel.TV %>% 
  filter(`GIVD ID`=="NA-US-002")
write_csv(header.sel.TV.add.NAUS002, path="_output/header.sel123_addNAUS002.csv")

### ADD SOPHY
header.sel.TV.Sophy <- header.sel.TV %>% 
  filter(`GIVD ID`=="EU-FR-003") %>% 
  dplyr::select(-`Still in sPlot`) %>% 
  mutate(`Usable in sPlot Project 02`="")
write_csv(header.sel.TV.Sophy, path="_output/header.sel123_Sophy.csv")

### EXTRACT RELEVE numbers for EU-CZ-001
header.TV.joined <- header.TV %>% 
  left_join(header %>% 
              dplyr::select(-Longitude, -Latitude, -`Location uncertainty (m)`, -Dataset), 
            by="PlotObservationID") %>% 
  dplyr::select(PlotObservationID,  Dataset, `GIVD ID`, Longitude:Latitude, Country, `Location uncertainty (m)`, `TV2 relevé number`, `Original nr in database`, ORIG_REL_N) %>% 
  left_join(databases %>% 
              dplyr::select(`GIVD ID`, Custodian, `Deputy custodian`, `Still in sPlot`))
header.sel.TV.EUCZ001 <- header.TV.joined %>% 
    filter(`GIVD ID`=="EU-CZ-001")
write_csv(header.sel.TV.EUCZ001, path="_output/header.sel123_EUCZ001.csv")

## extract RAINFOR Data + DAta From PERU
header.sel.TV.0000001 <-   header.TV.joined %>% 
  filter(`GIVD ID`=="00-00-001")
write_csv(header.sel.TV.0000001, path="_output/header.sel123_0000001.csv")

header.sel.TV.Peru <- header.TV.joined %>% 
  filter(Country=="Peru")
write_csv(header.sel.TV.0000001, path="_output/header.Peru.csv")

### EXTRACT RELEVE NUMBERS FOR AS-KG-001
header.sel.TV.add.ASKG001 <- header.sel.TV %>% 
  filter(`GIVD ID`=="AS-KG-001")
write_csv(header.sel.TV.add.ASKG001, path="_output/header.sel123_ASKG001.csv")

### EXTRACT RELEVE NUMBERS FOR SALVIAS 00-00-002
header.sel.TV.add.SALVIAS <- header.sel.TV %>% 
  filter(`GIVD ID`=="00-00-003")
write_csv(header.sel.TV.add.SALVIAS, path="_output/header.sel123_SALVIAS.csv")


##summarize
header.sel.TV.summary <- header.sel %>% 
  group_by(`GIVD ID`) %>% 
  summarize(n.sel.plot=n()) %>% 
  left_join(header %>% 
              group_by(`GIVD ID`) %>% 
              summarize(n.tot.plot=n()),
            by="GIVD ID") %>% 
  mutate(share.perc=n.sel.plot/n.tot.plot*100)



## Prepare demonstration subset for Miguel Alvarez, custodian of SWEA
load("/data/sPlot/releases/sPlot2.1/DT2_20161025.RData")
swea.header <- header.sel %>% 
  filter(`GIVD ID`=="AF-00-006") 

swea.IDs <- header.sel.TV  %>% 
  filter(`GIVD ID`=="AF-00-006") %>% 
  dplyr::select(-Custodian, -`Deputy custodian`, -`Still in sPlot`)

swea.dt <- DT2 %>% 
  filter(PlotObservationID %in% (swea.header %>% 
                                   pull(PlotObservationID))) %>% 
  dplyr::select(-Taxon.group, -Layer)

write_csv(swea.header, path="_output/header.SWEA.csv")
write_csv(swea.IDs, path="_output/IDs.SWEA.csv")
write_csv(swea.dt, path="_output/DT2.SWEA.csv")



####################################################################################################################
######### Reimport answers from dataset custodians and build redundant selection of plots that can be used #########
####################################################################################################################
rm(list=ls())
## reimport selected plots
PlotID123 <- read_csv("_data/PlotID_run_1_2_3.csv") %>% 
  rename(PlotObservationID=x)
#load header and fix
load("/data/sPlot/releases/sPlot2.1/sPlot_header_20161124.RData")

## GIT checkout to last stable version for sPlot 2.1 in GIT for consortium management data
source("/data/sPlot/users/Francesco/_sPlot_Management/Fix.header.R")
header <- fix.header(header, exclude.sophy = F)
# GIT checkout baco to master! - i.e., switch back to most up-to-date version in GIT consortium
databases <- read_csv("/data/sPlot/users/Francesco/_sPlot_Management/Consortium/Databases.out.csv")

header.sel <- header %>% 
  filter(PlotObservationID %in% PlotID123$PlotObservationID)

### Data from Stephan - Check for ID matches
header.TV <- read_delim(file = "_data/sPlot-2.1_header.csv", delim = "\t", 
                        col_types = cols(PlotObservationID = col_double(),
                                         PlotID = col_double(),
                                         `TV2 relevé number` = col_double(),
                                         `Original nr in database` = col_character(),
                                         ORIGDB_NR = col_character(),
                                         ORIGDB_NR_1 = col_character(),
                                         ORIG_REL_N = col_character(),
                                         Longitude = col_double(),
                                         Latitude = col_double(),
                                         `Location uncertainty (m)` = col_double(),
                                         Dataset = col_character()))

## import answers from custodians GIVD level
library(openxlsx)
#SWEA CONFIRMED ! - reimport answers

#answers <- read_csv(file="_management/resampling_answers.csv")
answers <- openxlsx::read.xlsx("_management/resampling_answers.xlsx", sheet = 2)
answers <- answers %>% 
  mutate(`Yes/Conditional/No`=fct_recode(`Yes/Conditional/No`, No="NO", Yes="yes")) %>% 
  # Manually set some dataset to yes
  # Rasmus Revermann and Donald Walker's acceptance is conditional, 
  # but depends on conditions others than the selection of plot
  # Brian Enquist confirmed for SALVIAS [29.04.2020]
  mutate(`Yes/Conditional/No`=replace(`Yes/Conditional/No`, 
                                    list=GIVD.ID %in% c("NA-US-014","AF-00-009", 
                                                        "AF-00-006", "00-00-003"),  
                                    values="Yes"))

# join header with Turboveg information by PlotObservationID, and attach dataset-level answers from custodians
header.sel.TV <- header.sel %>% 
  left_join(header.TV %>% 
              dplyr::select(PlotObservationID, `TV2 relevé number`:ORIG_REL_N),
            by="PlotObservationID") %>% 
  dplyr::select(PlotObservationID,  Dataset, `GIVD ID`, Longitude:Latitude, Country, `Location uncertainty (m)`, `TV2 relevé number`, `Original nr in database`, ORIG_REL_N) %>% 
  left_join(databases %>% 
              dplyr::select(`GIVD ID`, Custodian, `Deputy custodian`, `Still in sPlot`)) %>% 
  left_join(answers %>% 
              rename(`GIVD ID`=GIVD.ID)) 

## import google spreadsheet with plots marked individually
plots.checked <- openxlsx::read.xlsx("_management/header.sel123_checked_20200319.xlsx", sheet = 1)
plots.checked <- plots.checked %>% 
  as.tbl() %>% 
  mutate(Usable.in.Paper.02=ifelse(GIVD.ID=="00-RU-002", NA, Usable.in.Paper.02)) %>% 
  dplyr::select(PlotObservationID, Usable.in.Paper.02)

  


## DB whose plots are marked in other sources:
# CZ 
# Siberia - 00-RU-002 [Milan]
# Germany 
# NA-US-002

## import answers from custodians GIVD level
## Czechia - EU-CZ-001
CZ.id <- read_csv("_management/Header_EUCZ001_20191029_checked.csv")
CZ.id <- CZ.id %>% 
  mutate(Canbeused=ifelse(Canbeused=="Y", "Yes", "No")) %>% 
  dplyr::select(PlotObservationID, Usable.in.Paper.02=Canbeused)
  
## GVRD
GVRD <- openxlsx::read.xlsx("_management/GVRD_check_openaccess_short.xlsx", sheet = 1, rowNames=F)
GVRD <- header.sel.TV %>% 
  filter(`GIVD ID`=="EU-DE-014") %>% 
  dplyr::select(PlotObservationID, `GIVD ID`, `TV2 relevé number`) %>% 
  left_join(GVRD %>% 
              as.tbl() %>% 
              mutate(Usable.in.Paper.02=ifelse(Usable.in.Paper.02=="YES", "Yes", "No")) %>% 
              dplyr::select(`GIVD ID`=GIVD.ID, `TV2 relevé number`=TV2.relevé.number, Usable.in.Paper.02), 
            by=c("GIVD ID", "TV2 relevé number")) %>% 
  dplyr::select(-`TV2 relevé number`, -`GIVD ID`)

## Siberia - This part of code ONLY selects the usable plots. Header and DT will need to be replaced with the new version afterwards
### Import updated data from 00-RU-002
Siberia.public <- openxlsx::read.xlsx("_management/Siberia_update/00-RU-002-potentially-public-plots.xlsx", sheet=1)
Siberia.new <- read_delim("_management/Siberia_update/Siberia_Chytry_header.csv", delim="\t", 
                          col_types = cols(PlotObservationID = col_double(),
                            PlotID = col_double(),
                            `TV2 relevé number` = col_double(),
                            Country = col_character(),
                            `Cover abundance scale` = col_character(),
                            `Date of recording` = col_character(),
                            `Relevé area (m²)` = col_double(),
                            `Altitude (m)` = col_double(),
                            `Aspect (°)` = col_logical(),
                            `Slope (°)` = col_logical(),
                            `Cover total (%)` = col_logical(),
                            `Cover tree layer (%)` = col_double(),
                            `Cover shrub layer (%)` = col_double(),
                            `Cover herb layer (%)` = col_double(),
                            `Cover moss layer (%)` = col_double(),
                            `Cover lichen layer (%)` = col_logical(),
                            `Cover algae layer (%)` = col_logical(),
                            `Cover litter layer (%)` = col_double(),
                            `Cover open water (%)` = col_double(),
                            `Cover bare rock (%)` = col_double(),
                            `Height (highest) trees (m)` = col_double(),
                            `Height lowest trees (m)` = col_double(),
                            `Height (highest) shrubs (m)` = col_double(),
                            `Height lowest shrubs (m)` = col_double(),
                            `Aver. height (high) herbs (cm)` = col_double(),
                            `Aver. height lowest herbs (cm)` = col_double(),
                            `Maximum height herbs (cm)` = col_double(),
                            `Maximum height cryptogams (mm)` = col_double(),
                            `Mosses identified (y/n)` = col_character(),
                            `Lichens identified (y/n)` = col_character(),
                            COMMUNITY = col_character(),
                            SUBSTRATE = col_character(),
                            Locality = col_character(),
                            ORIG_NUM = col_character(),
                            ALLIAN_REV = col_character(),
                            REV_AUTHOR = col_character(),
                            Forest = col_logical(),
                            Grassland = col_logical(),
                            Wetland = col_logical(),
                            `Sparse vegetation` = col_logical(),
                            Shrubland = col_logical(),
                            `Plants recorded` = col_character(),
                            `Herbs identified (y/n)` = col_character(),
                            Naturalness = col_integer(),
                            EUNIS = col_character(),
                            FIELD_NO = col_character(),
                            Longitude = col_double(),
                            Latitude = col_double(),
                            `Location uncertainty (m)` = col_double(),
                            Dataset = col_character(),
                            `Access regime` = col_character())) %>% 
  mutate(Usable.in.Paper.02=ifelse(FIELD_NO %in% Siberia.public$FIELD_NO, "Yes", "No")) %>% 
  dplyr::select(-PlotObservationID) %>% 
  # attach PlotObservationID from header
  left_join(header.sel %>% 
              filter(Dataset=="Siberia_Chytry") %>% 
              dplyr::select(PlotObservationID) %>% 
              left_join(header.TV %>% 
                  dplyr::select(PlotObservationID, `TV2 relevé number`),
                by="PlotObservationID"), 
            by=c("TV2 relevé number")) %>% 
  dplyr::select(PlotObservationID, Usable.in.Paper.02) %>% 
  # exclude new plots not in sPlot 2.1
  filter(!is.na(PlotObservationID))

### NA-US-002 - USA_vegbank
vegbank0 <- openxlsx::read.xlsx("_management/sPlot-Open-NA-US-002.xlsx", sheet=1)
vegbank <- vegbank0 %>% 
  mutate(Bob.answer=replace(Bob.answer, 
                            list=Bob.answer %in% c("No ", "no"), 
                            values="No")) %>% 
  mutate(Bob.answer=replace(Bob.answer, 
                            list=Bob.answer=="yes", 
                            values="Yes")) %>% 
  mutate(`Usable.in.Paper.02`=Bob.answer) %>% 
  dplyr::select(PlotObservationID, Usable.in.Paper.02) %>% 
  filter(!is.na(PlotObservationID))
  
## SOPHY
sophy <- read_csv("_management/RLV_espaces_naturels-buffer_10m.csv") %>% 
  dplyr::select(PlotObservationID=PlotObserv) %>% 
  mutate(Usable.in.Paper.02="Yes")


### the turboveg codes of the new and old database match, but there are many new plots in Siberia.new. 
#  check <- header.sel %>% 
#    filter(Dataset=="Siberia_Chytry") %>% 
#    left_join(header.TV %>% 
#                dplyr::select(PlotObservationID, `TV2 relevé number`:ORIG_REL_N),
#              by="PlotObservationID") %>% 
#    dplyr::select(Dataset, PlotObservationID, `Original nr in database`, `GIVD ID`,  Country:`Slope (°)`, Longitude:Latitude, `Location uncertainty (m)`, `TV2 relevé number`) %>% 
#    left_join(Siberia.new %>% 
#                dplyr::select(Dataset, PlotID:`Slope (°)`, Latitude, Longitude),
#              by=c("Dataset", "TV2 relevé number"))






### Coalesce files
header.sel.final <- header.sel.TV %>% 
  dplyr::select(-Longitude, -Latitude, -`Location uncertainty (m)`, -ORIG_REL_N, -`TV2 relevé number`, -`Original nr in database`) %>% 
  left_join(plots.checked, by="PlotObservationID") %>% 
  left_join(CZ.id, by="PlotObservationID") %>% 
  left_join(GVRD, by="PlotObservationID") %>% 
  left_join(Siberia.new, by="PlotObservationID") %>% 
  left_join(vegbank, by="PlotObservationID") %>% 
  mutate(Usable=ifelse(`Yes/Conditional/No`=="Yes", "Yes", NA)) %>% 
  mutate(Usable=ifelse(`Yes/Conditional/No`=="No", "No", Usable)) %>%
  mutate(Usable=coalesce(Usable, Usable.in.Paper.02.x, Usable.in.Paper.02.y, Usable.in.Paper.02.x.x, Usable.in.Paper.02.y.y, Usable.in.Paper.02)) %>% 
  dplyr::select(-Usable.in.Paper.02.x, -Usable.in.Paper.02.y, -Usable.in.Paper.02.x.x, -Usable.in.Paper.02.y.y, -Usable.in.Paper.02) %>% 
  dplyr::select(-`Still in sPlot`, -`Yes/Conditional/No`) %>% 
  ##join France data from SOPHY
  left_join(sophy, by="PlotObservationID") %>% 
  mutate(Usable=coalesce(Usable, Usable.in.Paper.02)) %>% 
  mutate(Usable=ifelse(`GIVD ID`=="EU-FR-003" & is.na(Usable), 
                       "No", Usable)) %>% 
  dplyr::select(-Usable.in.Paper.02) %>% 
  mutate(Usable=ifelse(is.na(Usable), "Unknown", Usable))

  
summary.sel.final <- header.sel.final %>% 
  group_by(`GIVD ID`, Dataset, Custodian, `Deputy custodian`) %>% 
  summarize(usable=sum(Usable=="Yes"), 
            not.usable=sum(Usable=="No"), 
            unknown=sum(Usable=="Unknown")) %>% 
  arrange(desc(unknown), desc(not.usable), desc(usable), desc(`GIVD ID`)) 
  
print(summary.sel.final, n=20)
#AS-KG-001# still waiting for plot selection
#00-RU-001# need to be reimported from turboveg (asked Stephan Hennekens)

write_csv(summary.sel.final, "_output/summary.sel.final.csv")
write_csv(header.sel.final, "_output/header.sel.final.csv")


### Export plot ID of Angola data for Rasmus Revermann. Angola data will be updated.

angola.sel <- header.sel.TV %>% 
  filter(PlotObservationID %in% header.sel.final$PlotObservationID) %>% 
  filter(Dataset=="Angola") %>% 
  select(1:6, 8) 
write_csv(angola.sel, "_output/Angola_sel.csv")
