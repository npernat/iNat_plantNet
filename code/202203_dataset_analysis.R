# 
# 0. Packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("viridis")

# 1. characteristics of App and Expert data ####
# App scores and candidate lists
library(tidyverse)

isomex_250 %>% 
  count(sampleID) %>% 
  summarize(mean=mean(n), max=max(n), min=min(n), median=quantile(n,0.5))

isomex_250 %>% 
  select(score) %>% 
  drop_na() %>% 
  summarize(mean=mean(score), max=max(score), min=min(score), median=quantile(score,0.5))

isomex_250 %>% 
  select(sampleID, score) %>% 
  drop_na() %>% 
  group_by(sampleID) %>% 
  summarize(mean=sum(score)/length(sampleID)) %>% 
  summarize(mean_all=sum(mean)/length(mean))

# Experts identification frequencies
# some characteristics of expert and app identification
# proportion of identifiable/unidentifiable
isomex_first %>% 
  select(species_exp) %>% 
  mutate(spec_exp=case_when(
    species_exp!="unidentifiable"
    & species_exp!="noflower"
    & species_exp!="need_id"~"identified",
    TRUE ~ as.character(species_exp)
  )) %>% 
  group_by(spec_exp) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  write.csv2(.,"output/species_expert_identification.xls")

# 2. Matching species, genera and family ####

# 2.1 including all candidates
# species 
isomex_250_scores %>% 
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
isomex_250_scores%>%  
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
isomex_250_scores%>% 
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# 2.2. Including only the first candidate (scores) of the App

# Agreement with cutting scores - no threshold 

# species 
isomex_first %>% 
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
isomex_first %>%  
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
isomex_first%>% 
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# Agreements with cutting scores >0.8%
# species 
isomex_first %>% 
  filter(score>=0.80) %>%
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  
# this shows 4 + 1; however, the total number of observations of scores over 0.8 is n=6
# as "Mentha spicata" was identified right by the experts and the App in two observations
# therefore, the right result is 5 (83.3%).

# genus
isomex_first %>%  
  filter(score>=0.80) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
isomex_first %>% 
  filter(score>=0.80) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  


# Agreements with cutting scores >0.5

# species 
isomex_first %>% 
  filter(score>=0.50) %>%
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
isomex_first %>%  
  filter(score>=0.50) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
isomex_first %>% 
  filter(score>=0.50) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n)) 

# Agreements with cutting scores >0.3

# species 
isomex_first %>% 
  filter(score>=0.30) %>%
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
isomex_first %>%  
  filter(score>=0.30) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
isomex_first %>% 
  filter(score>=0.30) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n)) 

# # Agreements with cutting scores <0.3

# species 
isomex_first %>% 
  filter(score<0.30) %>%
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# Agreements with cutting scores - genus
isomex_first %>%  
  filter(score<0.30) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# Agreements with cutting scores - family
isomex_first %>% 
  filter(score<0.30) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n)) 

# 3. Figures - matching species, genera and family ####
library(ggplot2)
library(viridis)

# Figure matching species, threshold >0.5
# create subset of expert data
species_exp_first_05<-isomex_first %>% 
  filter(score>=0.5) %>% 
  select(species_exp) %>% 
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  group_by(species_exp) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(id_by="Expert")
colnames(species_exp_first_05)<-c("species", "n","id_by")

# create subset of app data
species_app_first_05<-isomex_first %>% 
  filter(score>=0.5) %>% 
  select(latin_name) %>% 
  group_by(latin_name) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(id_by="App")
colnames(species_app_first_05)<-c("species", "n","id_by")

# bind subsets
species_df_first_05<-rbind(species_exp_first_05, species_app_first_05)
head(species_df_first_05)

# breaks and labels for figure
brks_50_spec = seq(-3, 3, 1)
lbls_50_spec = paste0(as.character(c(seq(3, 0, -1), seq(1, 3,1))))

# figure
library(ggplot2)
library(viridis)
tiff("./output/species_props_first_05.tiff", units="px",width = 3000,height = 2000,res = 360)
plot_spec<-species_df_first_05 %>% 
  mutate(counts=ifelse(id_by=="Expert", n, -n)) %>%  
  ggplot(., aes(x=reorder(species, counts), y=counts, fill=id_by))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(breaks = brks_50_spec,
                     limits=c(-3,3),
                     labels = lbls_50_spec)+
  coord_flip() +  # flip axes
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size=10, face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank()) +
  labs(y="Counts", fill="Identified by", 
       title="Plant species by method \n(scores>0.5, first score per URL)")+
  scale_fill_grey(start=0.1, end=0.75, labels=c("App","Experts"))        
dev.off()


# Figure matching genera, threshold >0.5

# create subset of expert data
genus_exp_first<-isomex_first %>% 
  filter(score>=0.5) %>% 
  select(genus_exp) %>% 
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  group_by(genus_exp) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(id_by="Expert")
colnames(genus_exp_first)<-c("genus", "n","id_by")

# create subset of app data
genus_app_first<-isomex_first %>% 
  filter(score>=0.5) %>% 
  select(genus) %>% 
  filter(!is.na(genus))%>% 
  group_by(genus) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(id_by="App")
colnames(genus_app_first)<-c("genus", "n","id_by")

# bind subsets
genus_df_first<-rbind(genus_exp_first, genus_app_first)
summary(genus_df_first)



# breaks and labels for figure
brks_50_genus = seq(-6, 6, 1)
lbls_50_genus = paste0(as.character(c(seq(6, 0, -1), seq(1, 6,1))))

# figure
tiff("./output/genus_props_first.tiff", units="px",width = 3000,height = 2000,res = 360)
plot_gen<-genus_df_first %>% 
  #filter(n>3) %>% 
  mutate(counts=ifelse(id_by=="Expert", n, -n)) %>%  
  ggplot(., aes(x=reorder(genus, counts), y=counts, fill=id_by))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(breaks = brks_50_genus,
                     #limits=c(-10,10),
                     labels = lbls_50_genus)+
  coord_flip() +  # flip axes
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size=10, face="italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank()) +
  labs(y="Counts", fill="Identified by", 
       title="Plant genus by method \n(scores>0.5, first score per URL)")+
  scale_fill_grey(start=0.1, end=0.75, labels=c("App","Experts"))        
dev.off()

# Figure matching families, threshold >0.5
family_exp_first<-isomex_first %>% 
  filter(score>=0.5) %>% 
  select(family_exp) %>% 
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(family_exp=case_when(
    family_exp=="Apiaceaea"~"Apiaceae",
    TRUE ~ as.character(family_exp)))%>% 
  group_by(family_exp) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(id_by="Expert")
colnames(family_exp_first)<-c("family", "n","id_by")

family_app_first<-isomex_first %>% 
  filter(score>=0.5) %>% 
  select(family) %>% 
  group_by(family) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(id_by="App")
colnames(family_app_first)<-c("family", "n","id_by")

family_df_first<-rbind(family_exp_first, family_app_first)
head(family_df_first)

# breaks and labels for figure
brks_50_family = seq(-10, 10, 1)
lbls_50_family = paste0(as.character(c(seq(10, 0, -1), seq(1, 10,1))))

# figure
tiff("./output/family_props_first.tiff", units="px",width = 3000,height = 2000,res = 360)
plot_fam<-family_df_first %>% 
  mutate(counts=ifelse(id_by=="Expert", n, -n)) %>%  
  ggplot(., aes(x=reorder(family, counts), y=counts, fill=id_by))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(breaks = brks_50_family,
                     limits=c(-10,10),
                     labels=lbls_50_family)+
  coord_flip() +  # flip axes
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size=10, face="italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank()) +
  labs(y="Counts", fill="Identified by", 
       title="Plant family by method \n(scores>0.5, first score per URL)")+
  scale_fill_grey(start=0.1, end=0.75, labels=c("App","Experts"))        
dev.off()

# figure as panel
library(cowplot)
tiff("./output/comparison_panel.jpeg", units="px",width = 2500,height = 3000,res = 300)
plot_grid(plot_spec, plot_gen, plot_fam, nrow=3, labels="auto")
dev.off()

# 4. Species list ####

# from expert data
isomex_first %>% 
  select(species_exp) %>% 
  count(species_exp) %>% 
  arrange(desc(n)) %>% 
  write.csv2("output/specieslist_experts.xls")

# from app data
isomex_first %>% 
  filter(score >=0.5) %>% 
  select(latin_name) %>% 
  count(latin_name) %>% 
  arrange(desc(n)) %>% 
  write.csv2("output/specieslist_app.xls")

# from Romania
isomex_rom <-read_delim("data/Isomex_rom.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
isomex_rom<-isomex_rom[,c(1:4)]
colnames(isomex_rom)<-c("id", "source", "species","interaction")
str(isomex_rom)

# some investigation
specmatch_rom_app<-unique(isomex_250[which(isomex_250$latin_name == isomex_rom$species),]$latin_name)
specmatch_rom_experts<-unique(isomex_250[which(isomex_250$species_exp == isomex_rom$species),]$species_exp)
unique(isomex_rom$species)

# Romanian species and species identification by the App were added manually to the
# expert species list

# 5. Color and interaction figures ####
isomex_first$colour_simp<-
  case_when(isomex_first$colour=="green - white" ~ "white/green",
            isomex_first$colour=="noflower"
            |isomex_first$colour=="unidentifiable"
            |isomex_first$colour=="need_id"~"NA",
            TRUE~as.character(isomex_first$colour))

tiff("./output/colour.tiff", units="px",width = 1000,height = 1500,res = 300)
plot1<-isomex_first %>% distinct(image_url, .keep_all=TRUE) %>% 
  select(colour_simp) %>% 
  filter(!is.na(colour_simp)) %>%
  filter(colour_simp!="NA") %>% 
  group_by(colour_simp) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(reorder(colour_simp, -n),n))+
  geom_bar(stat="identity", width=0.5,fill=c("white","yellow","purple","blue","pink",
                                             "light green", "light yellow", "orange", "dark green", 
                                             "darkolivegreen1","lightpink","magenta"))+
  labs(y="No. of identifications by experts")+
  theme_gray() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10),
        axis.title.x = element_blank(),
        panel.grid = element_blank())
dev.off()
# interactions

tiff("./output/interaction.tiff", units="px",width = 1000,height = 1500,res = 300)
plot2<-isomex_first %>% distinct(image_url, .keep_all=TRUE) %>% 
  select(interaction) %>% 
  filter(!is.na(interaction)) %>%
  filter(interaction!="NA") %>% 
  mutate(interaction=case_when(
    interaction=="nest_preparing"~"nesting",
    TRUE~as.character(interaction)
  )) %>% 
  group_by(interaction) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(reorder(interaction, -n),n))+
  geom_bar(stat="identity", width=0.5, fill=gray.colors(7, start=0, end=0.6))+
  labs(y="No. of identifications by experts")+
  theme_gray() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10),
        axis.title.x = element_blank(),
        panel.grid = element_blank()) 
dev.off()

# figure as panel
library(cowplot)
tiff("./output/interaction_color.jpeg", units="px",width = 1800,height = 1200,res = 300)
plot_grid(plot1, plot2, labels="auto")
dev.off()
