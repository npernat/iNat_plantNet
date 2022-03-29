# Dataset preparations ####

# 0. Packages ####
install.packages("rinat")
install.package("zoo")
install.packages("httr")
install.packages("tidyverse")
install.packages("cli")
devtools::install_github(repo = 'BiologicalRecordsCentre/plantnet')
install.packages("stringr")
install.packages("taxize")
library(rinat)
library(zoo)
library(httr)
library(tidyverse)
library(cli)
library(plantnet)
library(stringr)
library(taxize)

# 1. retrieving images from iNaturalist ####

inat<-get_inat_obs(taxon_name="Isodontia mexicana", maxresults = 10000) 

# get iNaturalist observation of I. mexicana, but it only works for one year at a time
# adapt function to work for more than one year

years<-data.frame(year=c(2007:2021)) # define the years observations should be downloaded
inat_fun<-function(x){get_inat_obs_nostop(taxon_name = "Isodontia mexicana", meta=F,
                                          maxresults = 10000, year=x, quality="research")}
# *I adapted the function so that it won't stop when a result is zero for a year

Isodontia<-apply(years, 1, inat_fun) # loop over years
names(Isodontia)<-as.character(c(2007:2021)) # list of observations, years are list elements
Isodontia_df<-bind_rows(Isodontia) # create a dataframe

# check dataframe
dim(Isodontia_df)
head(Isodontia_df$taxon_id)
names(Isodontia_df)
iso_df<-Isodontia_df[,c(1,5,6,9,10,19,22,37)] # subset of interesting variables to keep
iso_df$SampleID<-c(1:nrow(iso_df)) # create ID number for merging datasets later
img_iso<-data.frame(imageURL=iso_df$image_url) # create a dataframe with all image URLs
# iso_df was exported for the Expert identification


# 2. Feed iNaturalist image URLs in PlantNet App and prepare dataset for merging with expert data ####

# create function for plantNet App to run over more than one URL
# get the key at https://my.plantnet.org/usage
fun_plantnet_all_simp<-function(x){identify(key, imageURL=x, no_reject = 'true',
                                            simplify="TRUE")}
iso_all<-apply(data.frame(img_iso[,1]), 1, fun_plantnet_all_simp) # loop functions over all image URLs

# save results
iso_all_id<-mapply(cbind, iso_all, "SampleID"=1:length(iso_all), SIMPLIFY=F)
notfound<-as.vector(which(sapply(iso_all_id, function(x) is.element("Species Not Found", x))))
iso_all_id_cc<-iso_all_id[-c(notfound)]
iso_all_df<-data.frame(do.call(rbind, iso_all_id_cc))

score_iso<-data.frame(score=unlist(isop_all_df))
latin_name_iso<-data.frame(latin_name=unlist(isop_all_df[,2]))
common_name_iso<-data.frame(common_name=unlist(isop_all_df[,3]))
sampleID_iso<-data.frame(sampleID=unlist(isop_all_df[,4]))
iso_save<-data.frame(score=score_isop, latin_name=latin_name_isop, 
                      common_name= common_name_isop, sampleID=sampleID_isop)
str(iso_save)
# write.csv2(iso_save, "iso_save.xls")
# further work with imported dataframe iso_save

# pick out the first 250 samples
isomex_250_test<-iso_save[c(1:10840),] # strangest thing that I could not filter
# by the sampleID but needed to look up the last row for sampleID 250 in the dataset

# get genus name from package stringr

library(stringr)
isomex_250_test$genus<-word(isomex_250_test$latin_name, 1) # gets the first word aka. the genus 
table(isomex_250_test$genus)

# get family name from package taxize 
# test with plantminer
plantminer<-plantminer(isomex_250_test$latin_name, from="tpl")
head(plantminer)
str(plantminer)
isomex_250_test$family<-plantminer$family
str(isomex_250_test)
isomex_250_test$score<-as.numeric(isomex_250_test$score)


# 3. Merge expert and plantNet dataset ####

# isomex_expert was imported after expert verification of URLs
# not run: readxls...
isomex_experts_250<-isomex_experts %>% 
  filter(SampleID<=250) %>% 
  rename(sampleID=SampleID)
isomex_250<-left_join(isomex_250_test, isomex_experts_250, by="sampleID")


# check important variables of proper format and correctness (e.g. spelling mistakes, dislocations)
isomex_250$score<-as.numeric(isomex_250$score)
table(isomex_250$species_exp)
table(isomex_250$family_exp) # some wrong spellings
isomex_250$family_exp<-gsub("Apiaceaea","Apiaceae",isomex_250$family_exp)
isomex_250$family_exp<-gsub("unindetifiable","unidentifiable",isomex_250$family_exp)
table(isomex_250$genus_exp) # some wrong spellings
table(isomex_250$genus)
isomex_250$genus_exp<-gsub("unindetifiable","unidentifiable",isomex_250$genus_exp)
isomex_250$genus_exp<-gsub("Euonimus","Euonymus",isomex_250$genus_exp)
which(isomex_250$species_exp=="noplant")
table(isomex_250$species_exp)
isomex_250$species_exp<-gsub("noplant","noflower",isomex_250$species_exp)
which(isomex_250$species_exp=="Cirsium arvensae")
isomex_250$species_exp<-gsub("Cirsium arvensae","Cirsium arvense",isomex_250$species_exp)
which(isomex_250$species_exp=="green - white")
head(isomex_250[718,]$genus_exp)
isomex_250[c(718, 719, 720),]$species_exp="unidentifiable"

# subsets to work with
isomex_first<-isomex_250 %>% 
  group_by(sampleID) %>% 
  slice_head()
isomex_first<-as.data.frame(isomex_first)

isomex_250_scores<-
  isomex_250%>% 
  filter(!is.na(score)) %>% 
  arrange(desc(score)) %>% 
  select(score,latin_name, species_exp, genus, 
         genus_exp, family, family_exp, image_url)
