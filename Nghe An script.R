require(dplyr)
require(readxl)
require(AMR)
require(stringr)
library(arsenal)
rm(list=ls()) 

## -----------Uong Bi Hospital---------
NA_2021<-read_excel("C:/Users/halt.OUCRU/PH Epidemiology Dropbox/Le Thanh Ha/46HN vi sinh/46HN vi sinh/Nghe An/w21vnm.xlsx", guess_max = 99999)
NA_2021<-NA_2021 %>% rename(specimen=13, organism=16) %>% mutate(specimen=recode(specimen,"ur"="urin"))
table(NA_2021$specimen)


NA_2021<-cbind(NA_2021[, c(1:22)], NA_2021 %>% select(contains("_ND")|contains("_NM")|contains("_NE")))
unique(unlist(NA_2021[,c(23:79)]))  #having RSI values

NA_2021_num <- apply(NA_2021[,c(23:79)],MARGIN = 2, function(x){stringr::str_replace_all(x,"R|I|S|r|i|s","") }) %>% as.data.frame()
NA_2021_RSI <- apply(NA_2021[,c(23:79)],MARGIN = 2, function(x){stringr::str_replace_all(x,"[^RISris]","") }) %>% as.data.frame()
unique(unlist(NA_2021_RSI))


NA_2021_num<-as.data.frame(sapply(NA_2021_num, as.numeric))
NA_2021_num<-data.frame(sapply(NA_2021_num, function(x) as.character(gsub("<=","<",x))))
NA_2021_num<-data.frame(sapply(NA_2021_num, function(x) as.character(gsub(">=",">",x))))
unique(unlist(NA_2021_num))


NA_2021_num[, 1:50] <- lapply(NA_2021_num[, 1:50], as.disk)  #truncated 1 values "
NA_2021_num[,51:57]<- lapply(NA_2021_num[,51:57], as.mic)   

NA_2021_combine<-cbind(NA_2021[,c(13,16)], NA_2021_num)
unique(unlist(NA_2021_combine[,c(3:59)]))
NA_2021_combine[is.na(NA_2021_combine)]<-""   #truncated 50 "" (blank) values

NA_2021_combine<-as.rsi(NA_2021_combine, guideline = "CLSI", uti=NULL,
                       conserve_capped_values = TRUE,    
                       reference_data = AMR::rsi_translation)


NA_2021_interpret <- NA_2021_combine 
for(i in 3:ncol(NA_2021_combine)){
  NA_2021_combine[is.na(NA_2021_combine[,i]),i] <- ""
  NA_2021_RSI[is.na(NA_2021_RSI[,i-2]),i-2] <- ""
  NA_2021_interpret[,i] <- paste0(NA_2021_combine[,i],NA_2021_RSI[,i-2])
}

unique(unlist(NA_2021_interpret[,3:59]))   #NA, NAS, NAR, NAI
NA_2021_interpret[NA_2021_interpret=="NAI"]<-"I"
NA_2021_interpret[NA_2021_interpret=="NAS"]<-"S"
NA_2021_interpret[NA_2021_interpret=="NAR"]<-"R"
NA_2021_interpret[NA_2021_interpret=="NA"]<-""


#Comparing two datasets
NA_2021_interpret_final<-cbind(NA_2021[,c(1:22)], NA_2021_interpret[3:59])
summary(comparedf(x=NA_2021[23:24], y=NA_2021_interpret_final[23:24]))
summary(table(NA_2021[23:25]))

all_equal(NA_2021[c(23:79)], NA_2021_interpret_final[c(23:79)])



saveRDS(NA_2021_interpret_final, "NgheAn_Interpret_2021_final.Rds")


