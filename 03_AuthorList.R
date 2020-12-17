### Code to administer the author list and their affiliations
### of sPlotOpen_Manuscript
### This code also formats authors affiliations to Manubot's standards (yaml)
library(tidyverse)
library(stringr)
filter <- dplyr::filter

#### 0. Ancillary functions ####
# Function 1 - Extract name initials
# credits: https://codereview.stackexchange.com/questions/150624/extracting-initials-with-r
initials <- function(full.name) {
  # Returns initials of a full name
  # Input will contain only letters (uppercase and/or lowercase) plus 
  # single spaces between words. Folks like Joseph Gordon-Levitt, 
  # Conan O’Brien, and David J. Malan won’t be using your program. (If only!)
  if (nchar(full.name) == 0) {
    stop ("Valid name please")
  }
  isspace  <- integer(0)
  fn.split <- unlist(strsplit(full.name, fixed = TRUE, split = ""))
  isspace  <- which(fn.split == " ")
  init     <- toupper(fn.split[c(1, (isspace+1))])
  paste(init, collapse = "")
}

#### Function 2 - format name, orcid, email and affiliation info into the metadata.yaml standard for manubot
create.yaml <- function(x, file.output){
  tmp <- affiliations %>%
    #    mutate(github="") %>% 
    filter(name==x) %>% 
    pivot_longer(!Sequence_affiliations, names_to = "tag") %>%
    arrange(Sequence_affiliations) %>% 
    dplyr::select(-Sequence_affiliations) %>% 
    mutate(tag=factor(tag, levels=c("github", "name","initials", 
                                    "orcid", "twitter", "email", 
                                    "affiliations","correspondence", "symbol_str"))) %>% 
    distinct() %>% 
    bind_rows(data.frame(tag="affiliations", 
                         value=paste0("\n      - ", 
                                      paste({.} %>% 
                                              filter(tag=="affiliations") %>% 
                                              pull(value),
                                            collapse="\n      - ")))) %>% 
    group_by(tag) %>% 
    slice(n()) %>% 
    ungroup() %>% 
    mutate(tag=as.character(tag)) %>% 
    mutate(tag=ifelse(tag=="github", "  - github", paste0("    ", tag))) %>% 
    filter(!is.na(value)) %>%
    unite(tag:value,
          sep = ": ",
          col = "newtag",
          remove = T) %>% 
    mutate(newtag=str_remove(newtag, pattern = '"')) %>% 
    mutate(newtag=str_remove(newtag, pattern = '"')) %>% 
    mutate(newtag=gsub(pattern="†", replacement = '"†"', x=newtag)) %>% 
    mutate(newtag=str_replace_all(string = newtag, pattern=", , ", replacement = ", ")) %>% 
    mutate(newtag=str_replace_all(string = newtag, pattern=", , ", replacement = ", ")) %>% 
    mutate(newtag=str_replace_all(string = newtag, pattern=" NA,", replacement = "")) %>% 
    mutate(newtag=gsub(pattern=", $", replacement = "", x=newtag))
  
  write_lines(tmp[[1]], file = file.output, append=T) 
}


## function 3 - ### get affiliation data from google sheet
get.affiliation.gs <- function(name.to.match, aff.gs){
  n.tmp <- which(paste(aff.gs$`First Name`, aff.gs$`Last Name`, sep=" ") == name.to.match)
  aff.gs0 <- aff.gs %>% mutate_at(.vars=vars(starts_with("Postal code")), .funs=funs(as.character))
  if(length(n.tmp)==0){stop("No matched name")}
  out <-  aff.gs0[n.tmp,] %>%
    mutate(Name=paste(`First Name`, `Last Name`, sep=" ")) %>%
    mutate(Sequence_affiliations=1) %>%
    dplyr::select(Surname=`Last Name`, Name, Sequence_affiliations, `Preferred email`, `Alternative email`, `ORCID`,
           `Department/Institute/Faculty`, `University/Institution`, Street, `Postal code`, Town, Country) %>%
    rename(`E-Mail`=`Preferred email`) %>%
    rename(`Second E-Mail`=`Alternative email`)
  if(is.na(aff.gs0[n.tmp,]$"Would you like to add another affiliation")){return(out); stop()} 
  if(aff.gs0[n.tmp,]$"Would you like to add another affiliation"=="Yes"){
    out <- out %>%
      bind_rows(aff.gs0[n.tmp,] %>%
                  mutate(Name=paste(`First Name`, `Last Name`, sep=" ")) %>%
                  mutate(Sequence_affiliations=2) %>%
                  dplyr::select(Surname=`Last Name`, Name, Sequence_affiliations, `Preferred email`, `Alternative email`, `ORCID`,
                         `Department/Institute/Faculty_1`, `University/Institution_1`, Street_1, `Postal code_1`, Town_1, Country_1) %>%
                  rename_at(.vars = vars(ends_with("_1")),
                            .funs = funs(sub("_1", "", .))) %>%
                  rename(`E-Mail`=`Preferred email`) %>%
                  rename(`Second E-Mail`=`Alternative email`))}
  if(is.na(aff.gs0[n.tmp,]$"Would you like to add another affiliation_1")){return(out); stop()} 
  if(aff.gs0[n.tmp,]$"Would you like to add another affiliation_1"=="Yes"){
    out <- out %>%
      bind_rows(aff.gs0[n.tmp,] %>%
                  mutate(Name=paste(`First Name`, `Last Name`, sep=" ")) %>%
                  mutate(Sequence_affiliations=3) %>%
                  dplyr::select(Surname=`Last Name`,Name, Sequence_affiliations, `Preferred email`, `Alternative email`, `ORCID`,
                         `Department/Institute/Faculty_2`, `University/Institution_2`, Street_2, `Postal code_2`, Town_2, Country_2) %>%
                  rename_at(.vars = vars(ends_with("_2")),
                            .funs = funs(sub("_2", "", .))) %>%
                  rename(`E-Mail`=`Preferred email`) %>%
                  rename(`Second E-Mail`=`Alternative email`))}
  return(out)
}


#### 1. Import data ####
##import sPlotOpen data
path <- "_sPlotOpenDB"
load(file = file.path(path, "sPlotOpen.RData"))
table1 <- read_csv("_output/Table1_Databases.csv")

## Import affiliation information of sPlot members
allroles <- read_csv("/data/sPlot/users/Francesco/_sPlot_Management/Consortium/roles.csv")
allaffiliations <- read_csv("/data/sPlot/users/Francesco/_sPlot_Management/Consortium/Affiliations.csv") %>% 
  left_join(allroles %>% 
              dplyr::select(Name,Surname), 
            by="Name")

#### 2. Start coauthor list ####
#### 2.1. First authors, core sPlot + last author ####
first <- allaffiliations %>% 
  filter(Name %in% c("Francesco Maria Sabatini", "Jonathan Lenoir")) %>% 
  arrange(Name)
##third author Tarek Habab
third <- tibble(Name="Tarek Hattab",
                `E-Mail`="Tarek.Hattab@ifremer.fr",
                ORCID="0000-0002-1420-5758",
                `Department/Institute/Faculty`="CNRS, IFREMER and IRD",
                `University/Institution`="MARBEC, University of Montpellier",
                Town="Sète",
                Country="France", 
                Surname="Hattab")

custodians <- unique(table1$Custodian)
# Splot core team
core <- allaffiliations %>% 
  filter(Name %in% (allroles %>%
                      filter(`Core team` == T) %>%
                      filter(!Surname %in% c("Bruelheide", "Sabatini", "Lenoir")) %>%
                      pull(Name))) %>% 
  arrange(Surname)
#last author
last <- allaffiliations %>% 
  filter(Name=="Helge Bruelheide")

#### 2.2. Additional opt-ins ####
#Import affiliation info for additioal opt-in coauthors
#(Update 09 November 2020)
optins <- read_csv("_management/Opt-in - Project #02 (Responses) - Form responses 1.csv") %>% 
  filter(X3=="I read and understand sPlot rules and would like to opt-in to this project") %>% 
  filter(!Name %in% c("Alicia Acosta", "Bruno Herault")) %>%  #already among custodians, but with name spelled differently
  dplyr::select(Surname, Name, `Email address`, Affiliation, Address) %>% 
  #join affiliations from our records
  left_join(allaffiliations, by="Name") %>% 
  #replace affiliations with opt-in input if record is not in archive
  mutate(`University/Institution`=ifelse(is.na(`E-Mail`), Affiliation, `University/Institution`)) %>% 
  mutate(Street=ifelse(is.na(`E-Mail`), Address, Street)) %>% 
  mutate(Surname=coalesce(Surname.y, Surname.x)) %>% 
  dplyr::select(-Surname.x, -Surname.y) %>% 
  mutate(`E-Mail`=ifelse(is.na(`E-Mail`), `Email address`, `E-Mail`)) %>% 
  mutate(Sequence_affiliations=ifelse(is.na(Sequence_affiliations), 1, Sequence_affiliations)) %>% 
  dplyr::select(Name, Surname, Sequence_affiliations:Country) %>% 
  mutate_if(.predicate = ~is.character(.), 
            .funs = list(~gsub(pattern = ",$", replacement = "", x = {.}))) %>% 
  filter(!Name %in% c(first, third, custodians, core)) %>% 
  mutate(ORCID=replace(ORCID,
                       list=Name=="Inger Greve Alsos", 
                       values="0000-0002-8610-1085")) %>% 
  mutate(Town=replace(Town,
                      list=Name=="John-Arvid Grytnes",
                      values="Bergen")) %>% 
  mutate(Country=replace(Country,
                      list=Name=="John-Arvid Grytnes",
                      values="Norway"))

### second batch of opt-ins (mostly from TRY)
### 14/12/2020
optin2 <- openxlsx::read.xlsx("_management/UpdateAffiliations - sPlot (Responses).xlsx", sheet=1)
colnames(optin2)[16:22] <-  paste0(colnames(optin2)[16:22], "_1")
colnames(optin2)[23:28] <-  paste0(colnames(optin2)[23:28], "_2")
optin2 <- optin2[,-c(33,34,35)]
optin2 <- optin2 %>% 
  rename_all(.funs=~gsub(pattern=".", replacement=" ", x=., fixed=T))

names_to_import <- c('Meelis Pärtel', 'Sophie Gachet', 'Josep Penuela', 'Dirk Nikolaus Karger', 'Gregory Richard Guerin', 'Attila Lengyel', #20.11.2020
                     'Frederic Lens', 'Débora Vanessa Lingner', "Arindam Banerjee", "Farideh Fazayeli", 
                     "Hanhuai Shan") # update 14.12.2020


optin2.aff <- NULL
for(n in names_to_import) {
  optin2.aff <- optin2.aff %>% 
    bind_rows(get.affiliation.gs(n, aff.gs = optin2))
}

optin2.aff <- optin2.aff %>% 
  mutate(Name=replace(Name, 
                      list=Name=="Josep Penuela", 
                      values="Josep Peñuelas")) %>% 
  mutate(Surname=replace(Surname, 
                      list=Surname=="Penuela", 
                      values="Peñuelas"))


#### 2.3. Merge lists ####
# first + core sPlot [alphabetical] + (custodians + opt-ins + TRY) [alphabetical] + last author
affiliations <- first %>% 
  bind_rows(third) %>% 
  bind_rows(core) %>% 
  bind_rows(allaffiliations %>% 
              filter(Name %in% custodians) %>%
              filter(!Name %in% core$Name) %>%
              filter(!Name %in% first$Name) %>%
              filter(!Name %in% last$Name) %>% 
              ## add additional coauthors from SOPHY
              bind_rows(
                tibble(
                  Name = "Guillermo Hinojos Mendoza",
                  `E-Mail` = "ghinojos@asessc.net",
                  `Department/Institute/Faculty` = "Pépinière d’Entreprises l’Espélidou, Parc d’Activités du Vinobre",
                  `University/Institution` = "ASES Ecological and Sustainable Services",
                  Street = "555 Chemin des Traverses, Lachapelle-sous-Aubenas",
                  `Postal code` = "07200",
                  Town = "Aubenas",
                  Country = "France",
                  Surname = "Hinojos Mendoza"
                )
              ) %>%
              ## add additional coauthors from AF-CD-001
              bind_rows(allaffiliations %>%
                          filter(Name == "Elizabeth Kearsley") %>%
                          mutate(
                            `Department/Institute/Faculty` =
                              replace(
                                `Department/Institute/Faculty`,
                                list = Name == "Elizabeth Kearsley",
                                values =
                                  "Department Environment, Computational and Applied Vegetation Ecology (UGent-CAVELab)"
                              )) %>% 
                          mutate(Surname = "Kearsley")) %>%
              bind_rows(
                tibble(
                  Name = "Wannes Hubau",
                  Sequence_affiliations = c(1, 2),
                  `E-Mail` = "wannes.hubau@ugent.be",
                  `Department/Institute/Faculty` = c(
                    "Department Environment, Laboratory of Wood Biology (UGent-WoodLab)",
                    "Service of Wood Biology"
                  ),
                  `University/Institution` = c("Ghent University", "Royal Museum for Central Africa"),
                  Street = c("Coupure Links 653", "Leuvensesteenweg 13"),
                  `Postal code` = c("9000", "3080"),
                  Town = c("Ghent", "Tervuren"),
                  Country = "Belgium",
                  Surname = "Hubau"
                )
              ) %>%
              bind_rows(
                tibble(
                  Name = "Marijn Bauters",
                  Sequence_affiliations = c(1, 2),
                  `E-Mail` = "marijn.bauters@ugent.be",
                  `Department/Institute/Faculty` =
                    c(
                      "Department Green chemistry and technology, Isotope Bioscience laboratory (UGent-ISOFYS)",
                      "Department Environment, Computational and Applied Vegetation Ecology (UGent-CAVELab)"
                    ),
                  `University/Institution` = "Ghent University",
                  Street = "Coupure Links 653",
                  `Postal code` = "9000",
                  Town = "Ghent",
                  Country = "Belgium",
                  Surname = "Bauters"
                )
              ) %>% 
              
              ### Authors from RAINFOR - as recommended by Oliver Phillips
              bind_rows(
                tibble(
                  Name = c("Abel Monteagudo Mendoza", "Rodolfo Vásquez Martínez"),
                  Sequence_affiliations = 1,
                  `E-Mail` = c("amonteagudomendoza@gmail.com", "neotaxon@yahoo.com"),
                  `Department/Institute/Faculty` =
                    "",
                  `University/Institution` = "Jardín Botánico de Missouri Oxapampa",
                  Street = "Bolognesi Mz-E-6",
                  `Postal code` = NA,
                  Town = "Oxapampa, Pasco",
                  Country = "Peru",
                  Surname = c("Monteagudo Mendoza", "Vásquez Martínez")
                )
              ) %>%
              bind_rows(
                tibble(
                  Name = "Luzmila Arroyo",
                  Sequence_affiliations = 1,
                  `E-Mail` = "luzmilaarroyo@hotmail.com",
                  `Department/Institute/Faculty` = "Dirección de la Carrera de Biología",
                  `University/Institution` = "Universidad Autónoma Gabriel René Moreno",
                  Street = NA,
                  `Postal code` = NA,
                  Town = "Santa Cruz de la Sierra",
                  Country = "Bolivia",
                  Surname = "Arroyo"
                )
              ) %>%
              bind_rows(
                tibble(
                  Name = "Timothy Killeen",
                  Sequence_affiliations = 1,
                  `E-Mail` = "timothy.j.killeen@gmail.com",
                  `Department/Institute/Faculty` =
                    "Museo de Historia Natural Noel Kempff Mercado",
                  `University/Institution` = "Universidad Autonoma Gabriel Rene Moreno",
                  Street = NA,
                  `Postal code` = NA,
                  Town = "Santa Cruz de la Sierra",
                  Country = "Bolivia",
                  Surname = "Killeen"
                )
              ) %>%
              ## Add possible additional coauthors here
              bind_rows(allaffiliations %>% 
                          filter(Name%in% c(#"Anita Smyth",
                                            "Alireza Naqinezhad",
                                            "Sylvia Haider", 
                                            "Pavel Shirokikh", 
                                            "Alicia T.R. Acosta", 
                                            "Bruno Hérault", 
                                            'Petr Petřík', 
                                            "Donald Waller"))) %>% 
              ##
              bind_rows(optins) %>% 
              bind_rows(optin2.aff) %>% 
              ## Joop Schaminée is not custodian anymore. Exclude?
              ###
            arrange(Surname, Sequence_affiliations)) %>%  
  bind_rows(last) %>% 
  dplyr:::select(name=Name, email=`E-Mail`, orcid=ORCID, everything(), -Surname, -`Second E-Mail`) %>% 
  replace_na(list(`Department/Institute/Faculty`="", Street="", `Postal code`="", Town="",   Country="" )) %>% 
  unite(`University/Institution`, `Department/Institute/Faculty`, Street:Country, sep = ", ", col="affiliations", remove=T) %>% 
  mutate(affiliations=str_replace_all(string = affiliations, pattern=", , ", replacement = ", ")) %>% 
  ### Add Github accounts, if required
  mutate(github="") %>% 
  mutate(github=ifelse(name=="Miguel Alvarez", "kamapu", github)) %>% 
  mutate(github=ifelse(name=="Francesco Maria Sabatini", "fmsabatini", github)) %>% 
  mutate(github=ifelse(name=="Jonathan Lenoir", "lenjon", github)) %>% 
  mutate(github=ifelse(name=="Helge Bruelheide", "Bruelheide", github)) %>% 
  ### Add Twitter account, if required
  mutate(twitter=NA) %>% 
  mutate(twitter=ifelse(name=="Francesco Maria Sabatini", "sPlot_iDiv", twitter)) %>% 
  mutate(twitter=ifelse(name=="Jonathan Lenoir", "EkoLogIt", twitter)) %>% 
  mutate(twitter=ifelse(name=="Helge Bruelheide", "HelgeBruelheide", twitter)) %>% 
  ### Add correspondence
  mutate(correspondence=ifelse(name=="Francesco Maria Sabatini", "true", NA)) %>% 
  ### Add equal contribution
  mutate(symbol_str=ifelse(name %in% c("Francesco Maria Sabatini", "Jonathan Lenoir"), 
                           '"†"', NA)) %>% 
  ### Add initials
  rowwise() %>% 
  mutate(initials=initials(name)) %>% 
  ungroup() %>% 
  ## correct typo
  mutate(name=replace(name, list=name=="Andraž Carni", values="Andraž Čarni"))


#### 2.4 opt-out ####
## exclude authors who declined offer
affiliations <- affiliations %>% 
  filter(!name %in% c("Marten Winter", 
                      "Ching-Feng Li", 
                      "Kim Sarah Jacobsen",
                      "Desalegn Wana", 
                      "Milan Valachovič")) 


#### 3. Create metadata.yaml file ####
affi.out <- "../_manuscript/content/metadata.yaml" #create empty affiliation file
##populate yaml file with affiliation info
write_lines(c(
  "---",
  'title: "sPlotOpen – An environmentally-balanced, open-access, global dataset of vegetation plots"',
  'keywords:',
  '  - vegetation',
  '  - database',
  '  - plants',
  '  - biodiversity',
  '  - functional traits',
  '  - big-data',
  '  - manubot',
  'lang: en-US', 
  'authors:'), file = affi.out)
lapply(affiliations %>% 
         dplyr::select(name) %>% 
         distinct() %>% 
         pull(name), create.yaml, affi.out)




#### 4. Create list of email addresses ####
email <- affiliations %>% 
  dplyr::select(name, email) %>% 
  distinct() %>% 
  mutate(export = paste0(name, " <", email, ">")) %>% 
  dplyr::select(export)
write_delim(email, file = "_output/Author_email.txt", delim="/t")

## Create email list for Opt-ins
#### this list was used to invite additional coauthors, besides the custodians
roles <- read_csv("/data/sPlot/users/Francesco/_sPlot_Management/Consortium/roles.csv")
email.optins <- roles %>% 
  filter(`Still in sPlot`) %>% 
  arrange(Surname) %>% 
  left_join(allaffiliations, by="Name") %>% 
  dplyr::select(Name, email=`E-Mail`) %>% 
  distinct() %>% 
  filter(!Name %in% affiliations$name) %>% 
  mutate(export = paste0(Name, " <", email, ">")) %>% 
  dplyr::select(export)
write_delim(email.optins, file = "_output/Optins_email", delim="/t" )




#### 5. Create checklist to approve submission ####
approve.checklist <- affiliations %>% 
  distinct(name) %>% 
  arrange(name) %>% 
  mutate(name=paste0(" - [ ] ", name))
write_delim(approve.checklist, file = "_output/Author_checklist.txt", delim="/t" )
## second batch
approve.checklist <- optin2.aff %>% 
  #filter(Name %in% affiliations$name) %>% 
  distinct(Name) %>% 
  arrange(Name) %>% 
  mutate(Name=paste0(" - [ ] ", Name))
write_delim(approve.checklist, file = "_output/Author_checklist_batch2.txt", delim="/t" )





