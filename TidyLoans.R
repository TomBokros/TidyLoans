.libPaths("D:/R/Library")
library(tidyverse)
options(scipen = 999)

loans <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

loans$agency_name <- gsub("[*,.]","",loans$agency_name)
loans$agency_name <- gsub("Inc","",loans$agency_name)
loans$agency_name <- gsub("LP","",loans$agency_name)
loans$agency_name <- gsub(" ","",loans$agency_name)
loans$agency_name <- gsub("LLC","",loans$agency_name)
loans$agency_name <- gsub("([a-z])([A-Z])", "\\1 \\2", loans$agency_name)
loans$agency_name <- gsub("ACT","Account Control Technology",loans$agency_name)
loans$agency_name <- gsub("FMS","FMS Investment Corp",loans$agency_name)
loans$agency_name <- gsub("FMS Investment CorpInvestment Corp","FMS Investment Corp",loans$agency_name)
loans$agency_name <- gsub("Pioneer","Pioneer Credit Recovery",loans$agency_name)
loans$agency_name <- gsub("Pioneer Credit Recovery Credit Recovery","Pioneer Credit Recovery",loans$agency_name)
loans$agency_name <- gsub("Windham","Windham Professionals",loans$agency_name)
loans$agency_name <- gsub("Windham Professionals Professionals","Windham Professionals",loans$agency_name)
loans$agency_name <- gsub("Collection Technologyorporated", "Collection Technology", loans$agency_name)
loans$agency_name <- gsub("tof","t of",loans$agency_name)
loans$agency_name <- gsub("sof","s of",loans$agency_name)
loans$agency_name <- gsub("uof","u of",loans$agency_name)
loans$agency_name <- gsub("CBEGroup","CBE Group",loans$agency_name)
loans$agency_name <- gsub("FHCannand","FH Cannand",loans$agency_name)
loans$agency_name <- gsub("GCServices","GC Services",loans$agency_name)

characters <- loans %>% 
  rename(just_year=year) %>% 
  mutate(year=paste0(just_year, "-",quarter)) %>%
  select(-just_year,-quarter) %>% 
  group_by(agency_name) %>% 
  summarise(total=mean(total,na.rm=T)) %>%  
  mutate(count=nchar(agency_name)) 
  
ggplot(characters, aes(x=reorder(agency_name,-total),y=total))+
  geom_col(fill="brown")+
  geom_point(data=characters,aes(x=agency_name, y=count*500000), colour="black", size=2)+
  geom_abline(intercept = 8000000, slope = 180000, colour="black", size=1)+
  coord_flip()+
  theme_minimal()+
  labs(y=element_blank(),x=element_blank())+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), panel.grid=element_blank())
