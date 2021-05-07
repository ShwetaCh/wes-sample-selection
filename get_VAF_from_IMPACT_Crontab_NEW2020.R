#!/opt/common/CentOS_6-dev/bin/current/R
library(data.table)
library(dplyr)
library(plyr)
library(stringr)
library(gsheet)
#library(googlesheets)
library(googlesheets4)
library(googledrive)

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
"%ni%"<-Negate("%in%")

############################################################################################################################################################################
#Pathology data aquisition
############################################################################################################################################################################

impact_qc_metrics<-read.table("/home/chavans/WES_sample_selection/MASTER_SAMPLE_LIST_deid.txt", fill = T, header = T, stringsAsFactors = F, sep = "\t")
hemepact_qc_metrics<-read.table("/home/chavans/WES_sample_selection/Deidentified_HEME_Sample_List.txt", fill = T, header = T, stringsAsFactors = F, sep = "\t")
pathology_qc_metrics<-rbind(impact_qc_metrics, hemepact_qc_metrics, fill = TRUE)
#pathology_qc_metrics = impact_qc_metrics
head(pathology_qc_metrics); dim(pathology_qc_metrics)
res <- pathology_qc_metrics %>% 
              dplyr::mutate(`AF_>=0.15_Exonic`=ifelse(Median_Exonic_Mutation_VAF>=0.15,1,0), 
                             `AF_>=0.15_Silent`=ifelse(Median_Silent_Mutation_VAF>=0.15,1,0)) %>%
              dplyr::mutate(.,Recommended_Coverage=ifelse(`AF_>=0.15_Exonic`==1,"Standard_Coverage","Higher_Coverage")) %>%
              select(DMP.Sample.ID = DMP_ASSAY_ID, Median_Exonic_Mutation_VAF, Median_Silent_Mutation_VAF, `AF_>=0.15_Exonic`, `AF_>=0.15_Silent`, Recommended_Coverage)
#head(res)
dim(res)
x = filter(res,Median_Exonic_Mutation_VAF>=0.15); table(x$Recommended_Coverage)
#head(x)
y = filter(res,Median_Exonic_Mutation_VAF<0.15); table(y$Recommended_Coverage)
#head(y)
write.table(res, '/home/chavans/WES_sample_selection/Deidentified_IMPACT_HEME_Sample_List.CCS.txt', row.names = F, quote = F, append = F, sep = "\t")
#filter(res,DMP.Sample.ID=="P-0037714-T01-IM6")
############################################################################################################################################################################
##Query google sheet and keep a back-up file
############################################################################################################################################################################

#url <- 'docs.google.com/spreadsheets/d/1K_CNufFracneU9xHub2KnwhTP0LMKf-N-tS62bjVbLc' #Original sheet
#url <- 'docs.google.com/spreadsheets/d/1KKu8BDt6lzYXcXOydoZFjFC3CJoM8PGbOWfIv6y1LZI' #New sheet

#query <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)

url <- 'docs.google.com/spreadsheets/d/1KKu8BDt6lzYXcXOydoZFjFC3CJoM8PGbOWfIv6y1LZI' #New sheet
query <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)
#newquery  = read_sheet(newurl)
#print(query$DMP.Sample.ID)
write.table(query,'/home/chavans/WES_sample_selection/DMPids_Results_GoogleSheet_Since030119.txt.bkp', row.names = F, quote = F, append = T)

############################################################################################################################################################################
## Actual processing, get VAF or claim missing data
############################################################################################################################################################################
missing_in_path<-setdiff(unique(query$DMP.Sample.ID),unique(res$DMP.Sample.ID))
print(length(missing_in_path))

if(length(missing_in_path) >=1){
  print(paste("Missing",missing_in_path))
  #write.table(paste("Samples missing in the metrics file:", missing_in_path), '/ifs/res/taylorlab/chavans/home/log/daily_wes_sample_selection.log', row.names = F, quote = F, append = T)
}
query = query %>% select(DMP.Sample.ID, Request.Date.to.CCS, Tracking.ID, Principal.Investigator, Data.Analyst, Study.Name, Tumor.Type)
subset_res = left_join(query,res,by=c("DMP.Sample.ID")) 
dim(subset_res)
subset_res = subset_res %>% mutate(.,`AF_>=0.15_Exonic`=ifelse(query$DMP.Sample.ID %in% missing_in_path,"Missing_Data",`AF_>=0.15_Exonic`), 
                                   Recommended_Coverage=ifelse(query$DMP.Sample.ID %in% missing_in_path,"Missing_Data",Recommended_Coverage),
                                     `AF_>=0.15_Silent`=ifelse(query$DMP.Sample.ID %in% missing_in_path,"Missing_Data",`AF_>=0.15_Silent`)) %>% distinct(.)
dim(subset_res)
#print(subset_res)
#table(subset_res$Recommended_Coverage)

x = filter(subset_res,Median_Exonic_Mutation_VAF>=0.15); table(x$Recommended_Coverage)
y = filter(subset_res,Median_Exonic_Mutation_VAF<0.15); table(y$Recommended_Coverage)

subset_res_na = filter(subset_res, Recommended_Coverage %ni% c("Higher_Coverage","Standard_Coverage","Missing_Data")) %>% select(DMP.Sample.ID)
subset_res_na   
subset_res = subset_res %>% mutate(.,Recommended_Coverage=ifelse(DMP.Sample.ID %in% subset_res_na$DMP.Sample.ID,"Higher_Coverage",Recommended_Coverage))
#tail(subset_res)
results = '/home/chavans/WES_sample_selection/DMPids_Results_GoogleSheet_Since030119.txt'
write.table(subset_res, results, row.names = F, quote = F, append = F, sep = "\t")

#gs_auth(new_user = TRUE) 
#gs4_auth
#drive_auth("~/service-token.json")
#drive_auth_config(active = FALSE)
#options(httr_oob_default=TRUE) 
#options(gargle_oob_default = TRUE)
#options(gargle_oauth_email = "jenny@example.org")

#drive_auth(
#  email = gargle::gargle_oauth_email(),
#  path = '/home/chavans/ccs-wes-284218-7a4138dd57c9.json',
#  scopes = "https://www.googleapis.com/auth/drive",
#  cache = FALSE,
#  use_oob = gargle::gargle_oob_default(),
#  token = NULL)

#vaf_review <- gs_title("VAF Review")
#gs4_auth_configure(api_key = "AIzaSyA3h1pJ8zGTGq6M-eGUmDUT5wjoiXxqBG0")
#gs4_auth(path="/home/chavans/ccs-wes-284218-7a4138dd57c9.json")
options(gargle_oauth_cache = ".secrets")

##Reading
print("Reading now...-->\n")
read_sheet("docs.google.com/spreadsheets/d/1KKu8BDt6lzYXcXOydoZFjFC3CJoM8PGbOWfIv6y1LZI")
##Writing
sheet_write(subset_res, 'docs.google.com/spreadsheets/d/1KKu8BDt6lzYXcXOydoZFjFC3CJoM8PGbOWfIv6y1LZI', "VAF Review")
#gs_upload(results, sheet_title = "VAF Review", verbose = TRUE, overwrite = TRUE)
#gs_add_row(vaf_review,"VAF Review","test\ttest\ttest",verbose = TRUE)	
############################################################################################################################################################################



