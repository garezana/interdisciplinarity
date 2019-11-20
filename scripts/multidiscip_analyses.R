library(tidyverse)

#Take all the output data on pubs in each journal (the csv files) and read them in as an R dataframe
# with the name of each file as a column

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

ALLDATA <-
  list.files(path = "./scripts/data/outputdata/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))


names(ALLDATA)

# rename and reorder the columns
ALLDATA <- ALLDATA %>% rename("focal_jrnl"="filename","cited_jrnl"="citedjournal") %>% select(uid,article,focal_jrnl,cited_jrnl)

# Simplify the journal names
ALLDATA$focal_jrnl<-gsub("./scripts/data/outputdata//","",ALLDATA$focal_jrnl)
ALLDATA$focal_jrnl<-gsub(".csv","",ALLDATA$focal_jrnl)
ALLDATA$focal_jrnl<-gsub(".2018","",ALLDATA$focal_jrnl)
ALLDATA$focal_jrnl<-gsub("ENVIRON","ENV",ALLDATA$focal_jrnl)
ALLDATA$focal_jrnl<-gsub("CONSERV","CON",ALLDATA$focal_jrnl)
ALLDATA$focal_jrnl<-gsub("POLICY","POL",ALLDATA$focal_jrnl)
ALLDATA$focal_jrnl<-gsub("POPUL","POP",ALLDATA$focal_jrnl)


head(ALLDATA,20)

# Save Journal Names as Factors
ALLDATA$focal_jrnl<-as.factor(ALLDATA$focal_jrnl)
ALLDATA$cited_jrnl<-as.factor(ALLDATA$cited_jrnl)
levels(ALLDATA$focal_jrnl)
levels(ALLDATA$cited_jrnl)

# TODO: the "weird" citations with numbers at the beginning are conference proceedings, white papers, etc. 
# Need to fix these later, if we can. 

ALLDATA
# Articles per Journal
ALLDATA %>% group_by(focal_jrnl) %>% summarize(n())

levels(ALLDATA$cited_jrnl)<-c(levels(ALLDATA$cited_jrnl),"missing_jrnl_name")
# replace NA with "missing_jrnl_name"
ALLDATA$cited_jrnl<-ALLDATA$cited_jrnl %>% replace_na("missing_jrnl_name")
ALLDATA %>% group_by(cited_jrnl) %>% summarize(n())


# CIitations 

journalsByArticles = ALLDATA %>% group_by(focal_jrnl,uid,cited_jrnl) %>% add_count(cited_jrnl) %>% distinct(.keep_all = FALSE) 


# Read in the file with the WOS categories
WOS_cats<-read.csv("./data/WOS_Core_Category_Sept_2019.csv")
names(WOS_cats)
head(WOS_cats,10)
WOS_cats<-WOS_cats %>% rename("wos_cat"="WoS.Category","cited_jrnl"="J.20","jrnl_title_full"="Journal.Title")

# select only the two columns of interest and attach them to the file ALLDATA
# this will tell you the category of journal for each cited journal
WOS_cats_reduced<-WOS_cats %>% select(cited_jrnl,wos_cat)
ALLDATA<-left_join(ALLDATA,WOS_cats_reduced)
ALLDATA$wos_cat<-tolower(ALLDATA$wos_cat)
head(ALLDATA,20)

coarse_cats<-read.csv("./data/coarse_to_medium.csv")
coarse_cats$wos_cat<-tolower(coarse_cats$wos_cat)
head(coarse_cats)

ALLDATA<-left_join(ALLDATA,coarse_cats)
missing_coarse_cat<-filter(ALLDATA,is.na(coarse_cat))
missing_coarse_cat$coarse_cat<-droplevels(missing_coarse_cat$coarse_cat)

head(missing_coarse_cat,20)
levels(as.factor(missing_coarse_cat$wos_cat))

# Need to define what couarse ategories these "fine categories" belong to
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"astronomy & astrophysics"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"development studies"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"film, radio, television"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"green & sustainable science & technology"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"medicine, research & experimental"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"meteorology & atmospheric sciences"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"quantum science & technology"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"regional & urban planning"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="ccccc"]<-"womens studies"
                 

ALLDATA %>% group_by(wos_cat) %>% summarize(count=n()) %>% arrange(desc(count))