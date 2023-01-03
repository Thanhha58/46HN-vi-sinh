rm(list=ls()) 
require(dplyr)
require(readxl)
require(AMR)
require(stringr)



## -----------Uong Bi Hospital---------
UB_2021<-read_excel("Uong Bi/KSD nam 2021-Uongbi hospital.xlsx", guess_max = 99999)
UB_2021<-UB_2021 %>% rename(specimen=19, organism=24) %>% mutate(specimen=recode(specimen,"cv"="urin"))
table(UB_2021$specimen)
colnames(UB_2021)
ab_name(colnames(UB_2021[,c(37:123)]))

#checking some particular organism names
unique(mo_name(UB_2021$organism))
unique(UB_2021$organism)
mo_shortname("ralman")
mo_fullname("ralman")
mo_shortname("entcpx")   #dont have
mo_shortname("cgx")    #unknown species
mo_shortname("ctl")    #unknown species
mo_shortname("san")    #unknown species
mo_shortname("xxx")      #automatically understand as NA
unknown <- filter(UB_2021, organism %in% c("entcpx", "cgx", "ctl"))

unique(unlist(UB_2021[,c(37:123)]))  #none RSI values
#which(UB_2021[,c(37:123)]=="6.4000000000000001E-2")
#UB_2021[UB_2021=="6.4000000000000001E-2"]<-""

UB_2021_num <- UB_2021[,c(37:123)] %>% select(contains("_ND")|contains("_NM")|contains("_NE"))

#UB_2021_num<-as.data.frame(sapply(UB_2021_num, as.numeric))
UB_2021_num<-data.frame(sapply(UB_2021_num, function(x) as.character(gsub("<=","<",x))))
UB_2021_num<-data.frame(sapply(UB_2021_num, function(x) as.character(gsub(">=",">",x))))
unique(unlist(UB_2021_num))

UB_2021_num[, 1:36] <- lapply(UB_2021_num[, 1:36], as.disk)  #truncated 20 values "
UB_2021_num[,37:87]<- lapply(UB_2021_num[,37:87], as.mic)   #truncated 17 values
UB_2021_combine<-cbind(UB_2021[,c(19,24)], UB_2021_num)


UB_2021_combine_convert<-as.rsi(UB_2021_combine, 
                        guideline = "CLSI", uti=NULL,
                         conserve_capped_values = TRUE,    
                         reference_data = AMR::rsi_translation)


unique(unlist(UB_2021_combine_convert[,3:89]))   #already had 3 levels (RSI)
name_disk<-paste(colnames(UB_2021_combine_convert)[3:38] %>% str_sub(.,1,3) %>% paste0(.,"_ND"))
names(UB_2021_combine_convert)[3:38]=name_disk
name_mic<-paste(colnames(UB_2021_combine_convert)[39:80] %>% str_sub(.,1,3) %>% paste0(.,"_NM"))
names(UB_2021_combine_convert)[39:80]=name_mic
name_etest<-paste(colnames(UB_2021_combine_convert)[81:89] %>% str_sub(.,1,3) %>% paste0(.,"_NE"))
names(UB_2021_combine_convert)[81:89]=name_etest


UB_2021_interpret_final<-cbind(UB_2021[,c(1:36)], UB_2021_combine_convert[3:89])

#checking result

table(UB_2021$IPM_NE)
table(UB_2021_interpret_final$IPM_NE)

table(UB_2021$CXM_ND30)
sum(table(UB_2021$CXM_ND30))
table(UB_2021_interpret_final$CXM_ND30)

saveRDS(UB_2021_interpret_final, "UongBi_Interpret_2021_final.Rds")


#testing githut commit


#testing second change
