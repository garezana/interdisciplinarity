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
# Add what "category" each of the focal journals is in
ALLDATA$focaljrnl_type<-NA
levels(ALLDATA$focal_jrnl)
ALLDATA$focaljrnl_type<-as.factor(ALLDATA$focaljrnl_type)
levels(ALLDATA$focaljrnl_type)<-(c("Envt Science","Envt Studies"))
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="ECOL.ECON"]<-"Envt Science"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="ENV.CON"]<-"Envt Science"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="ENV.RES.LETT"]<-"Envt Science"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="ENV.SCI.POL"]<-"Envt Science"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="J.ENV.SCI-CHINA"]<-"Envt Science"

ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="ENV.POL.GOV"]<-"Envt Studies"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="ENV.PLAN.C-POLIT"]<-"Envt Studies"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="ENV.IMPACT.ASSES"]<-"Envt Studies"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="J.ENV.DEV"]<-"Envt Studies"
ALLDATA$focaljrnl_type[ALLDATA$focal_jrnl=="POP.ENV"]<-"Envt Studies"


# Add a unique I number to each article

ALLDATA$article_id <- ALLDATA %>% group_indices(article) 

# Articles per Journal
ALLDATA %>% group_by(focal_jrnl) %>% summarize(n())

levels(ALLDATA$cited_jrnl)<-c(levels(ALLDATA$cited_jrnl),"missing_jrnl_name")
# replace NA with "missing_jrnl_name"
ALLDATA$cited_jrnl<-ALLDATA$cited_jrnl %>% replace_na("missing_jrnl_name")
# ALLDATA %>% group_by(cited_jrnl) %>% summarize(n())

# TODO: the "weird" citations with numbers at the beginning are conference proceedings, white papers, etc. 
# Need to fix these later, if we can. 
ALLDATA$cited_jrnl<-gsub('[[:digit:]]+', '', ALLDATA$cited_jrnl)
ALLDATA$cited_jrnl<-gsub("[[:punct:]]","",ALLDATA$cited_jrnl)
ALLDATA$cited_jrnl<-trimws(ALLDATA$cited_jrnl)

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
ALLDATA$cited_jrnl
coarse_cats<-read.csv("./data/coarse_to_medium.csv")
coarse_cats$wos_cat<-tolower(coarse_cats$wos_cat)
head(coarse_cats)

ALLDATA<-left_join(ALLDATA,coarse_cats)
missing_coarse_cat<-filter(ALLDATA,is.na(coarse_cat))
missing_coarse_cat$coarse_cat<-droplevels(missing_coarse_cat$coarse_cat)

head(missing_coarse_cat,20)
levels(as.factor(missing_coarse_cat$wos_cat))


levels(ALLDATA$coarse_cat)
# Need to define what couarse ategories these "fine categories" belong to
ALLDATA$coarse_cat[ALLDATA$wos_cat=="astronomy & astrophysics"]<-"Physical_Sciences"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="development studies"]<-"Social_Sciences"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="film, radio, television"]<-"Arts_Humanities"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="green & sustainable science & technology"]<-"Engineering_Technology"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="medicine, research & experimental"]<-"Clinical_PreClinical_Health"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="meteorology & atmospheric sciences"]<-"Physical_Sciences"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="quantum science & technology"]<-"Engineering_Technology"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="regional & urban planning"]<-"Social_Sciences"
ALLDATA$coarse_cat[ALLDATA$wos_cat=="womens studies"]<-"Social_Sciences"
                 
head(ALLDATA,50)


missing_coarse_cat<-filter(ALLDATA,is.na(coarse_cat))
missing_coarse_cat$coarse_cat<-droplevels(missing_coarse_cat$coarse_cat)
missing.cats<-levels(as.factor(missing_coarse_cat$cited_jrnl))
head(missing.cats,50)

# code any missing categories as missing instead of NA
levels(ALLDATA$coarse_cat)<-c(levels(ALLDATA$coarse_cat),"uncategorized")
ALLDATA$coarse_cat<-ALLDATA$coarse_cat %>% replace_na("uncategorized")


# analysis: how many coarse cats cited in each article 
coursecats_cited<-ALLDATA %>% group_by(focaljrnl_type,focal_jrnl,article_id) %>% summarize(cats_cited=n_distinct(coarse_cat))
coursecats_cited_table1<-coursecats_cited %>% group_by(focaljrnl_type) %>% summarize(avg=mean(cats_cited))
coursecats_cited_table2<-coursecats_cited %>% group_by(focaljrnl_type,focal_jrnl) %>% summarize(avg=mean(cats_cited))


# analysis: how many of each coarse cat cited in each article 
# use frequency because different articles have different numbers of citation 
counts_by_coarsecat<-ALLDATA %>% group_by(focaljrnl_type,focal_jrnl,article_id,coarse_cat) %>% tally() %>% mutate(pcnt = n / sum(n)*100) %>% arrange(focaljrnl_type,focal_jrnl,article_id,n)
counts_by_coarsecat_table1<-counts_by_coarsecat %>% group_by(focaljrnl_type,focal_jrnl,coarse_cat) %>% summarize(avg_pcnt=mean(pcnt))
counts_by_coarsecat_table2<-counts_by_coarsecat %>% group_by(focaljrnl_type,coarse_cat) %>% summarize(avg_pcnt=mean(pcnt)) %>% arrange(coarse_cat)



ggplot(data=counts_by_coarsecat_table2, aes(x=coarse_cat, y=avg_pcnt,fill=focaljrnl_type)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


ALLDATA %>% group_by(focaljrnl_type,coarse_cat) %>% summarize(count=n()) %>% arrange(desc(count))
