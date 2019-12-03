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
articles_per_journal<-ALLDATA %>% group_by(focal_jrnl,focaljrnl_type) %>%
  summarize(n_articles=n()) %>% arrange(focaljrnl_type,desc(n_articles)) 
write.table(articles_per_journal, file = "./output/articles_per_journal.txt", sep = ",", quote = FALSE, row.names = F)

levels(ALLDATA$cited_jrnl)<-c(levels(ALLDATA$cited_jrnl),"missing_jrnl_name")
# replace NA with "missing_jrnl_name"
ALLDATA$cited_jrnl<-ALLDATA$cited_jrnl %>% replace_na("missing_jrnl_name")
# ALLDATA %>% group_by(cited_jrnl) %>% summarize(n())

# TODO: the "weird" citations with numbers at the beginning are conference proceedings, white papers, etc. 
# Need to fix these later, if we can. 
ALLDATA$cited_jrnl<-gsub('[[:digit:]]+', '', ALLDATA$cited_jrnl)
ALLDATA$cited_jrnl<-gsub("[[:punct:]]","",ALLDATA$cited_jrnl)
ALLDATA$cited_jrnl<-trimws(ALLDATA$cited_jrnl)

# Number of Artciles and citations
nArticles = ALLDATA %>% summarize(n_distinct(article_id))
nCitations = nrow(ALLDATA)

# Citations 
journalsByArticles = ALLDATA %>% group_by(focal_jrnl,uid,cited_jrnl) %>% add_count(cited_jrnl) %>% distinct(.keep_all = FALSE) 

# No_of_citations_per_article
No_of_citations_per_article<-ALLDATA %>% group_by(article_id, focal_jrnl,focaljrnl_type) %>%
  summarize(n_citations=n()) %>% arrange(focal_jrnl,desc(n_citations)) 
hist_cit_count<-ggplot(No_of_citations_per_article, aes(x=n_citations)) + 
  geom_histogram(binwidth=25,color="darkblue", fill="white")+
  theme_classic()

# avg_citations_per_article
No_of_citations_per_article<-ALLDATA %>% group_by(article_id, focal_jrnl,focaljrnl_type) %>%
  summarize(n_citations=n()) %>% arrange(focal_jrnl,desc(n_citations)) %>% 
  group_by(focaljrnl_type,focal_jrnl) %>% summarize(avg=mean(n_citations),sd=sd(n_citations))
write.table(No_of_citations_per_article, file = "./output/No_of_citations_per_article.txt", sep = ",", quote = FALSE, row.names = F)

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
missing.cats<-as.data.frame(missing.cats)
head(missing.cats,50)

# code any missing categories as missing instead of NA
levels(ALLDATA$coarse_cat)<-c(levels(ALLDATA$coarse_cat),"uncategorized")
ALLDATA$coarse_cat<-ALLDATA$coarse_cat %>% replace_na("uncategorized")


mid_cat<-ALLDATA %>% distinct(coarse_cat,wos_cat) %>% arrange(coarse_cat,wos_cat)
write.csv(mid_cat,"./data/mid_cat.csv")






# analysis: how many fine cats cited in each article 
finecats_cited<-ALLDATA %>% group_by(focaljrnl_type,focal_jrnl,article_id) %>% summarize(cats_cited=n_distinct(wos_cat))
finecats_cited_table1<-finecats_cited %>% group_by(focaljrnl_type) %>% summarize(avg=mean(cats_cited),sd=sd(cats_cited))
finecats_cited_table2<-finecats_cited %>% group_by(focaljrnl_type,focal_jrnl) %>% summarize(avg=mean(cats_cited),sd=sd(cats_cited))
write.table(finecats_cited_table2, file = "./output/finecats_cited_per_article.txt", sep = ",", quote = FALSE, row.names = F)


# analysis: how many coarse cats cited in each article 
coursecats_cited<-ALLDATA %>% group_by(focaljrnl_type,focal_jrnl,article_id) %>% summarize(cats_cited=n_distinct(coarse_cat))
coursecats_cited_table1<-coursecats_cited %>% group_by(focaljrnl_type) %>% summarize(avg=mean(cats_cited),sd=sd(cats_cited))
coursecats_cited_table2<-coursecats_cited %>% group_by(focaljrnl_type,focal_jrnl) %>% summarize(avg=mean(cats_cited),sd=sd(cats_cited))
write.table(coursecats_cited_table2, file = "./output/coursecats_cited_per_articles.txt", sep = ",", quote = FALSE, row.names = F)


# analysis: how many of each coarse cat cited in each article 
# use frequency because different articles have different numbers of citation 
counts_by_coarsecat<-ALLDATA %>% group_by(focaljrnl_type,focal_jrnl,article_id,coarse_cat) %>% tally() %>% mutate(pcnt = n / sum(n)*100) %>% arrange(focaljrnl_type,focal_jrnl,article_id,n)
counts_by_coarsecat_table1<-counts_by_coarsecat %>% group_by(focaljrnl_type,focal_jrnl,coarse_cat) %>% summarize(avg_pcnt=mean(pcnt),sd=sd(pcnt))
counts_by_coarsecat_table2<-counts_by_coarsecat %>% group_by(focaljrnl_type,coarse_cat) %>% summarize(avg_pcnt=mean(pcnt),sd=sd(pcnt)) %>% arrange(coarse_cat)
write.table(counts_by_coarsecat_table2, file = "./output/pcnt_by_coarsecat.txt", sep = ",", quote = FALSE, row.names = F)



coarsecat_plot<-ggplot(data=counts_by_coarsecat_table2, aes(x=coarse_cat, y=avg_pcnt,fill=focaljrnl_type)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")
coarsecat_plot<-coarsecat_plot+theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-15, hjust=0.05),        #Sets title size, style, locatio
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        # legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        axis.text.x=element_text(angle = 45, hjust = 1))
coarsecat_plot<-coarsecat_plot+labs(fill = "Focal Journal Type",x="Web of Science Category", y= "Percent (avg.)")
coarsecat_plot


# analysis: how many of each fine cat cited in each article 
# use frequency because different articles have different numbers of citation 
counts_by_finecat<-ALLDATA %>% group_by(focaljrnl_type,focal_jrnl,article_id,wos_cat) %>% tally() %>% mutate(pcnt = n / sum(n)*100) %>% arrange(focaljrnl_type,focal_jrnl,article_id,n)
counts_by_finecat_table1<-counts_by_finecat %>% group_by(focaljrnl_type,focal_jrnl,wos_cat) %>% summarize(avg_pcnt=mean(pcnt),sd=sd(pcnt))
counts_by_finecat_table2<-counts_by_finecat %>% group_by(focaljrnl_type,wos_cat) %>% summarize(avg_pcnt=mean(pcnt),sd=sd(pcnt)) %>% arrange(wos_cat)
levels(counts_by_finecat_table2$wos_cat)
counts_by_finecat_table2$wos_cat<-droplevels(counts_by_finecat_table2$wos_cat)
counts_by_finecat_table2<-droplevels(counts_by_finecat_table2)

finecat_plot<-ggplot(data=counts_by_finecat_table2, aes(x=wos_cat, y=avg_pcnt,fill=focaljrnl_type)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
finecat_plot


ALLDATA %>% group_by(focaljrnl_type,coarse_cat) %>% summarize(count=n()) %>% arrange(desc(count))



##########################
##########################
##########################
# Geographic Diversity  
DivDataPooled<-ALLDATA %>% group_by(focaljrnl_type,focal_jrnl,article_id,coarse_cat) %>% tally()  
DivDataPooled<-spread(DivDataPooled, coarse_cat, n) 
column_names<-colnames(DivDataPooled)
DivDataPooled[is.na(DivDataPooled)] <- 0
DivDataPooled<-ungroup(DivDataPooled)
library(vegan)
# 4: Geo Diverisity using Inverse Simpson's Index (expressed as 1/D)
IsimpDivTable <- diversity((DivDataPooled %>% select(-focaljrnl_type,-focal_jrnl,-article_id)), index="invsimpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
IsimpDivTable <- data.frame(IsimpDivTable)
IsimpDivTable$focaljrnl_type <-DivDataPooled$focaljrnl_type #Add year as a column
IsimpDivTable$focal_jrnl <-DivDataPooled$focal_jrnl #Add year as a column
IsimpDivTable$article_id <-DivDataPooled$article_id #Add year as a column

IsimpDivTable<-rename(IsimpDivTable, InvSimpson=IsimpDivTable) #rename the columns
IsimpDivTable <- IsimpDivTable[c("focaljrnl_type","focal_jrnl","article_id","InvSimpson")] #reorder the columns
IsimpDivTable<-as_tibble(IsimpDivTable)

# THIS CALCLULATES THE SIMPSONS INDEX (expressed as 1-D)

simpDivTable <- diversity((DivDataPooled %>% select(-focaljrnl_type,-focal_jrnl,-article_id)), index="simpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
simpDivTable <- data.frame(simpDivTable)
simpDivTable$focaljrnl_type <-DivDataPooled$focaljrnl_type #Add year as a column
simpDivTable$focal_jrnl <-DivDataPooled$focal_jrnl #Add year as a column
simpDivTable$article_id <-DivDataPooled$article_id #Add year as a column

simpDivTable<-rename(simpDivTable, Simpson=simpDivTable) #rename the columns
simpDivTable <- simpDivTable[c("focaljrnl_type","focal_jrnl","article_id","Simpson")] #reorder the columns

simpDivTable<-as_tibble(simpDivTable)

IsimpDivTable<-left_join(IsimpDivTable,simpDivTable, by=c("focaljrnl_type","focal_jrnl","article_id"))
rm(simpDivTable)

avgs_diversity<-IsimpDivTable %>% group_by(focaljrnl_type,focal_jrnl) %>% summarize(avg=mean(InvSimpson),sd=sd(InvSimpson))
write.table(avgs_diversity, file = "./output/avgs_diversity.txt", sep = ",", quote = FALSE, row.names = F)


Plot2 <- ggplot(avgs_diversity, aes(focal_jrnl, avg), fill=focaljrnl_type) + 
  geom_col() +  
  geom_errorbar(aes(ymin = avg - sd, ymax = avg + sd), width=0.2)

Plot2 <- Plot2 + labs(y="avg. simpson's div. index", x = "journal") + theme_classic()+theme(axis.text.x = element_text(angle = 45,hjust = 1))



# STATS
library(lme4)



mod1<-glmer(n ~ (1|focal_jrnl), data = counts_by_coarsecat ,family=poisson, na.action = "na.fail")
mod2<-glmer(n ~ focaljrnl_type + (1|focal_jrnl), data = counts_by_coarsecat ,family=poisson, na.action = "na.fail")
mod3<-glmer(n ~ focaljrnl_type + coarse_cat +(1|focal_jrnl), data = counts_by_coarsecat ,family=poisson, na.action = "na.fail")
mod4<-glmer(n ~ coarse_cat +(1|focal_jrnl), data = counts_by_coarsecat ,family=poisson, na.action = "na.fail")

AIC(mod1,mod2,mod3,mod4)
anova(mod3,mod4, test = "Chisq")

summary(mod3)


# UNCATEGORIZED

head(ALLDATA)
levels(as.factor(ALLDATA$wos_cat))
levels(as.factor(ALLDATA$coarse_cat))


uncat<-filter(ALLDATA,coarse_cat=="uncategorized") %>% 
  distinct(cited_jrnl,coarse_cat,.keep_all = TRUE) %>% 
  arrange(cited_jrnl)

foo<-unlist(strsplit(uncat$cited_jrnl , " "))
foo<-as.data.frame(foo)
head(foo,10)
count_snippets<-foo %>% group_by(foo) %>% summarize(count=n()) %>% arrange(desc(count))
# ACTA, JOURNAL, J, ANN: journals  (note ANN could be "Annual")
# FAO: UN
# PROCEEDINGS, CONFERENCES
# UNEP, UN, UNFCCC, UNDP
# WORKING

