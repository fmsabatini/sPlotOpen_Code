library(tidyverse)

## Load sPlotOpen data
load("~/share/groups/sPlot/releases/_sPlotOpenDB/sPlotOpen.RData")

# Fix header.oa based on Issue #6
# https://github.com/fmsabatini/sPlotOpen_Code/issues/6
header.oa <- header.oa %>%    
  mutate_at(vars(starts_with('Cover')),   
            .funs=~(ifelse(. > 100, 100, .)))



# Fix DT2 and CWM based on Issue #7
# https://github.com/fmsabatini/sPlotOpen_Code/issues/7

## Identify SALVIAS plots in sPlotOpen to be replaced
tofix <- header.oa %>% 
  filter(Dataset=="Salvias") %>% 
  dplyr::select(PlotObservationID:Location_uncertainty) %>% 
  left_join(metadata.oa %>% 
              dplyr::select(PlotObservationID, Original_nr_in_database), 
            by="PlotObservationID")

## Import SALVIAS corrected data 
header.salvias <- read_delim("_data/Update_SALVIAS/SALVIAS_correct_header.csv") %>% 
  dplyr::select(New_PlotObservationID = PlotObservationID, 
                Original_nr_in_database = `Original nr in database`)

## attach New Plot ID to tofix object
tofix <- tofix %>% 
  dplyr::select(PlotObservationID, Original_nr_in_database) %>% 
  left_join(header.salvias, by="Original_nr_in_database")

## Import correct salvias DT table, and match new IDs with old IDs
DT2.salvias0 <- read_delim("_data/Update_SALVIAS/SALVIAS_correct_species.csv") %>% 
  rename(New_PlotObservationID = PlotObservationID) %>%  
  left_join(tofix, by="New_PlotObservationID") %>% 
  filter(!is.na(PlotObservationID))

## double check species richness (they should be almost identical)

checking <- DT2.oa %>% 
  filter(PlotObservationID %in% tofix$PlotObservationID) %>% 
  count(PlotObservationID) %>% 
  left_join(DT2.salvias0 %>% 
              count(PlotObservationID) %>% 
              rename(n_new=n), 
            by="PlotObservationID")
plot(checking[,2:3])
## There are minor mismatches, probably due to taxonomic standardization
## The most series seems plots 56497    NADUGANI - Unclear why



### Taxonomic standardization
# ## First Try - backengineer Backbone from sPlotOpen itself
# bb.sPlotOpen <- DT2.oa %>% 
#   filter(PlotObservationID %in% tofix$PlotObservationID) %>% 
#  distinct(Species, Original_species) 
# # Check wheter all taxa from DT2.salvias can be resolved this way
# DT2.salvias0 %>%
#  distinct(`Species name`) %>% 
#  left_join(bb.sPlotOpen, 
#            by=c("Species name" = "Original_species")) %>% 
#  filter(is.na(Species))
# ## Too many unmatched entries --> 22,765

## Second Try - Load Backbone from sPlot 3
#load("~/share/groups/sPlot/releases/sPlot2.1/backbone.splot2.1.try3.is.vascular.Rdata")
load("~/share/groups/sPlot/releases/sPlot3.0/Backbone3.0.RData")


DT2.salvias <- DT2.salvias0 %>% 
  # some string cleaning
  mutate(Original_species=str_remove(`Species name`, pattern = " species$")) %>% 
  mutate(Original_species=str_remove(Original_species, " \\[CM$| \\[Derris$| subf\\.$| RS$| SP\\:$| M$| NPZ$| LC$")) %>% 
  left_join(Backbone %>% 
              dplyr::select(Name_sPlot_TRY, Name_short) %>% 
              distinct(), 
            by=c("Original_species" = "Name_sPlot_TRY")) %>% 
  left_join(Backbone %>% 
              dplyr::select(Name_submitted, Name_short2 = Name_short) %>% 
              distinct(), 
            by=c("Original_species" = "Name_submitted")) %>% 
  mutate(Name_short=coalesce(Name_short, Name_short2)) %>% 
  mutate(Name_short = replace(Name_short, 
                              list=str_detect(Name_short, "suitable"), 
                              values=NA)) %>% 
  #distinct(Original_species, Name_short) %>% 
  #filter(is.na(Name_short)) %>% 
  #View()
  mutate(Relative_cover = NA) %>% 
  dplyr::select(PlotObservationID, 
                Species = Name_short, 
                Original_species = `Species name`, 
                #Layer, 
                ## The stem count and the TV2 species number seem reversed!
                Original_abundance = `TV2 species nr`, 
                Abundance_scale = `Cover code`, 
                Relative_cover, 
                New_PlotObservationID) 
dim(DT2.salvias)

DT2.salvias %>% 
  distinct(Species, Original_species) %>% 
  filter(is.na(Species)) %>% 
  nrow()
## Using the backbone from sPlot3.0 only 31 taxa remain unmatched for a total of 931 species x plot obs

### Check for non-vascular plants
#Complement taxon group using info from sPlot 3.0 
## Assign genera to taxon group
taxon.groups <- DT2.salvias %>% 
  as.tbl() %>% 
  distinct(Species) %>% 
  mutate(species2=Species) %>% 
  separate(species2, into=c("Genus"), sep=" ") %>% 
  distinct(Species, Genus) %>% 
  left_join(Backbone %>% 
              distinct(Name_short, `Taxon group`) %>% 
              rename(species=Name_short, 
                     Taxon.group=`Taxon group`) %>% 
              separate(species, into=c("Genus"), sep=" ") %>% 
              mutate(Taxon.group=as.character(Taxon.group)) %>% 
              mutate(Taxon.group=ifelse(Taxon.group=="Unknown", NA, Taxon.group)) %>% 
              distinct(Genus, .keep_all=T), 
            by="Genus") %>% 
  distinct(Species, .keep_all = T) %>% 
  dplyr::select(-Genus)

DT2.salvias <- DT2.salvias %>% 
  # attach taxon group info from Backbone 3.0
  left_join(taxon.groups, by="Species") %>% 
  filter(is.na(Taxon.group) | 
           !Taxon.group %in% c("Alga", "Lichen", "Moss")) %>% 
  # calculate relative cover
  left_join({.} %>%
              group_by(PlotObservationID) %>% 
              summarize(tot.cover=sum(Original_abundance), .groups = 'drop'), 
            by=c("PlotObservationID")) %>% 
  mutate(Relative_cover=Original_abundance/tot.cover) %>% 
  dplyr::select(-tot.cover, -Taxon.group)
## Only one moss taxon, with three entries. Excluded
dim(DT2.salvias)



### Delete wrong entries from DT2.oa and replace with correct salvias
DT2.oa.out <- DT2.oa %>% 
  filter(!PlotObservationID %in% tofix$PlotObservationID) %>% 
  bind_rows(DT2.salvias %>% 
              dplyr::select(all_of(colnames(DT2.oa)))) %>% 
  arrange(PlotObservationID, Species)

dim(DT2.oa)
dim(DT2.oa.out)


### Check what the 2 thousand rows were before
#species in old version (with wrong SALVIA plots) but not in the corrected version
setdiff(DT2.oa %>% distinct(Species) %>% arrange(Species) %>% pull(),
        DT2.oa.out %>% distinct(Species) %>% arrange(Species) %>% pull())
#504 species were excluded

#species in the corrected version that were not present in the old version
setdiff(DT2.oa.out %>% distinct(Species) %>% arrange(Species) %>% pull(),
        DT2.oa %>% distinct(Species) %>% arrange(Species) %>% pull())
#352 new species were included


#selecting random tropical species for closer inspection
header.oa %>% filter(PlotObservationID %in%
                       c(DT2.oa %>% filter(Species == 'Bauhinia divaricata') %>% pull(PlotObservationID))) %>% 
  View()


header.oa %>% filter(PlotObservationID %in%
                       c(DT2.oa.out %>% filter(Species == 'Tabebuia rosea') %>% pull(PlotObservationID))) %>% 
  View()


### Compute CWM\CWV for DT2.salvias
#Load species level gap-filled trait data
load("~/share/groups/sPlot/releases/sPlot2.0/TRY.all.mean.sd.3.by.genus.species.Rdata")
TRY <- TRY.all.mean.sd.3.by.genus.species

#Merge species data table with traits, and calculate species coverage for each plot, both based on relative cover, and number of species having trait info.
CWM_CWV.oa0.salvias <- DT2.oa.out %>%
  as.tbl() %>%
  dplyr::select(PlotObservationID, Species, Relative_cover) %>%
  left_join(TRY %>%
              dplyr::rename(Species=StandSpeciesName) %>%
              dplyr::select(Species, LeafArea.mean:Wood.vessel.length.mean), 
            by="Species") %>% 
  rename_at(.vars=vars(ends_with(".mean")), 
            .funs=~gsub(pattern=".mean", replacement="", x=.))

# number of species with trait information.
CWM_CWV.oa0.salvias %>% 
  distinct(Species, .keep_all = T) %>% 
  filter(!is.na(SLA)) %>% 
  nrow()

# Calculate coverage for each trait in each plot
CWM_CWV.oa2.salvias <- CWM_CWV.oa0.salvias %>%
  mutate_at(.vars = vars(LeafArea), 
            .funs = list(~if_else(is.na(.),0,1) * Relative_cover)) %>%
  group_by(PlotObservationID) %>%
  summarize(TraitCoverage_cover=sum(LeafArea, na.rm=T),
            Species_richness=n(),
            TraitCoverage_pa=mean(LeafArea>0), 
            .groups = 'drop')


#Calculate CWM and CWV for each trait in each plot
# Ancillary function to calculate CWV
variance2.fun <- function(trait, abu){
  res <- as.double(NA)
  #nam <- nam[!is.na(trait)]
  abu <- abu[!is.na(trait)]
  trait <- trait[!is.na(trait)]
  abu <- abu/sum(abu)
  if (length(trait)>1){
    # you need more than 1 observation to calculate
    # skewness and kurtosis
    # for calculation see 
    # http://r.789695.n4.nabble.com/Weighted-skewness-and-curtosis-td4709956.html
    m.trait <- weighted.mean(trait,abu)
    res <- sum(abu*(trait-m.trait)^2)
  }
  res
}

CWM_CWV.oa1.salvias <- CWM_CWV.oa0.salvias %>%
  group_by(PlotObservationID) %>%
  summarize_at(.vars= vars(LeafArea:Wood.vessel.length),
               .funs = list(CWM=~weighted.mean(., Relative_cover, na.rm=T), 
                            CWV=~variance2.fun(., Relative_cover)))

#Assemble output
CWM_CWV.oa.salvias <- header.oa %>% 
  dplyr::select(PlotObservationID) %>% 
  left_join(CWM_CWV.oa2.salvias, by="PlotObservationID") %>% 
  left_join(CWM_CWV.oa1.salvias, by="PlotObservationID") %>% 
#Rename fields to follow convention 
  rename_all(.funs=~gsub('\\.', '_', x = .))

dim(CWM_CWV.oa.salvias)

## Check CWM correlations

plot(CWM_CWV.oa$LeafArea_CWM, 
     CWM_CWV.oa.salvias$LeafArea_CWM, 
     col = (CWM_CWV.oa %>% 
                    pull(PlotObservationID) %in% tofix$PlotObservationID + 1))


### Export
DT2.oa <- DT2.oa.out
CWM_CWV.oa <- CWM_CWV.oa.salvias

path <- "_sPlotOpenDBv2"
save(DT2.oa, header.oa, metadata.oa, CWM_CWV.oa, reference.oa, sPlotOpen_citation, file = file.path(path, "sPlotOpen.RData"))

## Export to csv files
write_delim(DT2.oa, file = file.path(path, "sPlotOpen_DT.txt"), delim="\t")
write_delim(CWM_CWV.oa, file = file.path(path, "sPlotOpen_CWM_CWV.txt"), delim="\t")

    
       
       




