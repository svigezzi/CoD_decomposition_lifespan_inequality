##################################################################
# Aim: Create multiple decrement lifetables from HMD and WHO data
# Date: 17/02/2023
# Notes: Requires WHO and HMD data, includes sound effects
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
##################################################################

# Load functions
source("Scripts/Functions.R")

# Load WHO data
load("Data/who.RData")

# Load and rename HMD data
load("Data/hmd.RData")

hmd <- hmd %>% 
  clean_names() %>% 
  filter(!(pop_name %in% c("DEUTE","DEUTW","FRACNP",
                           "NZL_MA","NZL_NM","GBRCENW",
                           "GBRTENW", "GBR_SCO", "GBR_NIR",
                           "TWN", "CHE", "DNK", 
                           "EST", "LVA","LTU","RUS","UKR"))) %>% # not included in WHO data
  filter(!(pop_name %in% c("CHE", "DNK"))) %>% # not included in WHO ICD-9 data
  filter(!(pop_name %in% c("BLR"))) %>% # causes in WHO have codes not included in ICD-10
  # with too many deaths to discount
  mutate(country = case_when(
    pop_name == "AUT" ~ 4010,
    pop_name == "BEL" ~ 4020,
    pop_name == "BGR" ~ 4030,
    pop_name == "ESP" ~ 4280,
    pop_name == "FRATNP" ~ 4080,
    pop_name == "HUN" ~ 4150,
    pop_name == "ITA" ~ 4180,
    pop_name == "NLD" ~ 4210,
    pop_name == "POL" ~ 4230,
    pop_name == "PRT" ~ 4240))



# Clean and merge HMD and WHO data ------------------------------------------------

miss_ctr <- c() # vector to store all countries with unknown causes
full_grouped_temp <- data.table() # data.table to store cleaned data

# How do you want to harmonise ages in HMD and WHO?
harmonise <-  1 # should all lifetables be single age and the last age group be 110?

# harmonise <- 2 # should tlifetables remain single age or abridged, but the open age hamornised?
# end_age <- 85 # if so, to which age should the lifetables be harmonised?

# harmonise <- 3 # should the WHO age formats be left as is and the HMD lifetables collapsed accordingly?


# Now come a lot of loops. It is not good R practice, but hey, it works.
# Start loop level 1
for(i in unique(hmd$country)){
  
  cat("Start", i, "\n")
  
  # Select country
  ctr_who_try <- who %>% 
    filter(country==i)
  
  # Group causes of death for each year (cause-of.death classification changes across years)
  ctr_who <- data.table() # data.table to store grouped 
  # Start loop level 2-A
  for(y in unique(ctr_who_try$year)){
    
    ctr_who_y <- ctr_who_try %>% 
      filter(year==y)
    
    # Create grouped CoD
    # ICD-9
    if(unique(ctr_who_y$list)%in%c("09A","09B","09N","09C")){
      ctr_who_y <- ctr_who_y %>% 
        select(!c(admin1,sub_div,list,im_frmat,
                  im_deaths1,im_deaths2,im_deaths3,im_deaths4)) %>% 
        filter(!substr(cause,1,3)%in%140:239, 
               !(substr(cause,1,3)=="B01"&cause!="B01"),
               !(substr(cause,1,3)=="B02"&cause!="B02"),
               !(substr(cause,1,3)=="B03"&cause!="B03"),
               !(substr(cause,1,3)=="B04"&cause!="B04"),
               !(substr(cause,1,3)=="B05"&cause!="B05"),
               !(substr(cause,1,3)=="B06"&cause!="B06"),
               !(substr(cause,1,3)=="B07"&cause!="B07"),
               !(substr(cause,1,3)=="B08"&cause!="B08"),
               !(substr(cause,1,3)=="B09"&cause!="B09"),
               !(substr(cause,1,3)=="B10"&cause!="B10"),
               !(substr(cause,1,3)=="B11"&cause!="B11"),
               !(substr(cause,1,3)=="B12"&cause!="B12"),
               !(substr(cause,1,3)=="B13"&cause!="B13"),
               !(substr(cause,1,3)=="B14"&cause!="B14"),
               !(substr(cause,1,3)=="B15"&cause!="B15"),
               !(substr(cause,1,3)=="B16"&cause!="B16"),
               !(substr(cause,1,3)=="B17"&cause!="B17"),
               !(substr(cause,1,3)=="B18"&cause!="B18"),
               !(substr(cause,1,3)=="B19"&cause!="B19"),
               !(substr(cause,1,3)=="B20"&cause!="B20"),
               !(substr(cause,1,3)=="B21"&cause!="B21"),
               !(substr(cause,1,3)=="B22"&cause!="B22"),
               !(substr(cause,1,3)=="B23"&cause!="B23"),
               !(substr(cause,1,3)=="B24"&cause!="B24"),
               !(substr(cause,1,3)=="B25"&cause!="B25"),
               !(substr(cause,1,3)=="B26"&cause!="B26"),
               !(substr(cause,1,3)=="B27"&cause!="B27"),
               !(substr(cause,1,3)=="B28"&cause!="B28"),
               !(substr(cause,1,3)=="B29"&cause!="B29"),
               !(substr(cause,1,3)=="B30"&cause!="B30"),
               !(substr(cause,1,3)=="B31"&cause!="B31"),
               !(substr(cause,1,3)=="B32"&cause!="B32"),
               !(substr(cause,1,3)=="B33"&cause!="B33"),
               !(substr(cause,1,3)=="B34"&cause!="B34"),
               !(substr(cause,1,3)=="B35"&cause!="B35"),
               !(substr(cause,1,3)=="B36"&cause!="B36"),
               !(substr(cause,1,3)=="B37"&cause!="B37"),
               !(substr(cause,1,3)=="B38"&cause!="B38"),
               !(substr(cause,1,3)=="B39"&cause!="B39"),
               !(substr(cause,1,3)=="B40"&cause!="B40"),
               !(substr(cause,1,3)=="B41"&cause!="B41"),
               !(substr(cause,1,3)=="B42"&cause!="B42"),
               !(substr(cause,1,3)=="B43"&cause!="B43"),
               !(substr(cause,1,3)=="B44"&cause!="B44"),
               !(substr(cause,1,3)=="B45"&cause!="B45"),
               !(substr(cause,1,3)=="B46"&cause!="B46"),
               !(substr(cause,1,3)=="B47"&cause!="B47"),
               !(substr(cause,1,3)=="B48"&cause!="B48"),
               !(substr(cause,1,3)=="B49"&cause!="B49"),
               !(substr(cause,1,3)=="B50"&cause!="B50"),
               !(substr(cause,1,3)=="B51"&cause!="B51"),
               !(substr(cause,1,3)=="B52"&cause!="B52"),
               !(substr(cause,1,3)=="B53"&cause!="B53"),
               !(substr(cause,1,3)=="B54"&cause!="B54"),
               !(substr(cause,1,3)=="B55"&cause!="B55"),
               !(substr(cause,1,3)=="B56"&cause!="B56"),
        ) %>% # some causes are coded twice
        mutate(cause_new = case_when(
          substr(cause,1,3)=="B00" | substr(cause,1,4)=="CH01" ~ 1, # all causes
          substr(cause,1,3)=="B08" | substr(cause,1,3)=="B09" | substr(cause,1,3)=="B10" | 
            substr(cause,1,3)=="B11" | substr(cause,1,3)=="B12" | substr(cause,1,3)=="B13" |
            substr(cause,1,3)=="B14" | substr(cause,1,3)=="B15" | substr(cause,1,3)=="B16" | 
            substr(cause,1,3)=="B17" | substr(cause,1,4)=="CH02" ~ 2, # neoplasms
          substr(cause,1,3)=="B26" | substr(cause,1,3)=="B27" | substr(cause,1,3)=="B28" | 
            substr(cause,1,3)=="B29" | substr(cause,1,3)=="B30" | substr(cause,1,4)=="CH07" ~ 3, # circulatory system
          substr(cause,1,3)=="B44" | substr(cause,1,3)=="B45" | substr(cause,1,4)=="CH14" | substr(cause,1,4)=="CH15" ~ 4, # perinatal and congenital
          substr(cause,1,3)=="B47" | substr(cause,1,3)=="B48" | substr(cause,1,3)=="B49" | substr(cause,1,3)=="B50" |
            substr(cause,1,3)=="B51" | substr(cause,1,3)=="B52" | substr(cause,1,3)=="B53" | substr(cause,1,3)=="B54" |
            substr(cause,1,3)=="B55" | substr(cause,1,3)=="B56" | substr(cause,1,4)=="CH17" ~ 5, # external
          substr(cause,1,3)=="B01" | substr(cause,1,3)=="B02" | substr(cause,1,3)=="B03" | substr(cause,1,3)=="B04" | 
            substr(cause,1,3)=="B05" | substr(cause,1,3)=="B06" | substr(cause,1,3)=="B07" | substr(cause,1,3)=="B18" |
            substr(cause,1,3)=="B19" | substr(cause,1,3)=="B20" | substr(cause,1,3)=="B21" | substr(cause,1,3)=="B22" |
            substr(cause,1,3)=="B23" | substr(cause,1,3)=="B24" | substr(cause,1,3)=="B25" | substr(cause,1,3)=="B31" | substr(cause,1,3)=="B32" |
            substr(cause,1,3)=="B33" | substr(cause,1,3)=="B34" | substr(cause,1,3)=="B35" | substr(cause,1,3)=="B36" |
            substr(cause,1,3)=="B37" | substr(cause,1,3)=="B38" | substr(cause,1,3)=="B39" | substr(cause,1,3)=="B40" |
            substr(cause,1,3)=="B41" | substr(cause,1,3)=="B42" | substr(cause,1,3)=="B43" | substr(cause,1,3)=="B46" |
            substr(cause,1,4)=="CH01" | substr(cause,1,4)=="CH04" | substr(cause,1,4)=="CH04" | substr(cause,1,4)=="CH05" |
            substr(cause,1,4)=="CH06" | substr(cause,1,4)=="CH08" | substr(cause,1,4)=="CH09" | substr(cause,1,4)=="CH10" | 
            substr(cause,1,4)=="CH11" | substr(cause,1,4)=="CH12" | substr(cause,1,4)=="CH14" | substr(cause,1,4)=="CH16" ~ 6, #other
        ))
      # Portugal --> in 2004 and 2005 Portuguese data is not divided by CoD
    } else  if(unique(ctr_who_y$list)=="UE1"){ 
      ctr_who_y <- ctr_who_y %>% 
        select(!c(admin1,sub_div,list,im_frmat,
                  im_deaths1,im_deaths2,im_deaths3,im_deaths4)) %>% 
        filter(!(year %in% c(2004,2005))) %>% # Portugal does not have age-specific deaths for these years
        mutate(cause_new = case_when(
          cause=="AAA" ~ 1, # all causes
          str_detect(cause, "C\\d\\d") | str_detect(cause, "D[0-48]") | str_detect(cause, "D[0-4][0-8].") ~ 2, # neoplasms --> should I differentiate malignant and non-malignant?
          str_detect(cause, "I\\d\\d") ~ 3, # circulatory system
          str_detect(cause, "[P]\\d\\d") | str_detect(cause, "[Q]\\d\\d") ~ 4, # perinatal and congenital
          cause == "U049" ~ 4, # perinatal --> Belgium has this code (possibly from GBD)
          str_detect(cause, "[V-Y]\\d\\d") ~ 5,  # external
          str_detect(cause, "A\\d\\d") | str_detect(cause, "B\\d\\d") |
            str_detect(cause, "D[5-9][0-9]") | str_detect(cause, "E\\d\\d") |
            str_detect(cause, "F\\d\\d") | str_detect(cause, "G\\d\\d") | 
            str_detect(cause, "H\\d\\d") | str_detect(cause, "J\\d\\d") |
            str_detect(cause, "K\\d\\d") | str_detect(cause, "[L-N]\\d\\d") |
            str_detect(cause, "R\\d\\d") | str_detect(cause, "O\\d\\d")  ~ 6 # other
        ))
      # ICD-10
    } else {
      ctr_who_y <- ctr_who_y %>% 
        select(!c(admin1,sub_div,list,im_frmat,
                  im_deaths1,im_deaths2,im_deaths3,im_deaths4)) %>% 
        mutate(cause_new = case_when(
          cause=="AAA" | as.numeric(cause)==1000 ~ 1, # all causes
          str_detect(cause, "C\\d\\d") | str_detect(cause, "D[0-48]") | str_detect(cause, "D[0-4][0-8].") | as.numeric(cause)%in%1026:1047 ~ 2, # neoplasms --> should I differentiate malignant and non-malignant?
          str_detect(cause, "I\\d\\d") |as.numeric(cause)%in%1064:1071 ~ 3, # circulatory system
          str_detect(cause, "[P]\\d\\d") | str_detect(cause, "[Q]\\d\\d") | as.numeric(cause)%in%1092:1093 ~ 4, # perinatal and congenital
          cause == "U049" ~ 4, # perinatal --> Belgium has this code (possibly from GBD)
          str_detect(cause, "[V-Y]\\d\\d") | as.numeric(cause)%in%1095:1103| substr(cause,1,4)=="U129" ~ 5,  # external
          str_detect(cause, "A\\d\\d") | str_detect(cause, "B\\d\\d") |
            str_detect(cause, "D[5-9][0-9]") | str_detect(cause, "E\\d\\d") |
            str_detect(cause, "F\\d\\d") | str_detect(cause, "G\\d\\d") | 
            str_detect(cause, "H\\d\\d") | str_detect(cause, "J\\d\\d") |
            str_detect(cause, "K\\d\\d") | str_detect(cause, "[L-N]\\d\\d") |
            str_detect(cause, "R\\d\\d") | str_detect(cause, "O\\d\\d") |
            as.numeric(cause)%in%c(1001:1025,1048:1063,1072:1091,1094) |
            substr(cause,1,4)=="U071" | substr(cause,1,4)=="U072" | substr(cause,1,4)=="U099" | substr(cause,1,4)=="U109" ~ 6 # unclassified
        ))
    }
    
    ctr_who <- rbind(ctr_who, ctr_who_y)
    
  }
  # End loop level 2-A
  
  # Check if detailed cause is unknown (i.e. not included in broader categorisation)
  if(anyNA(ctr_who$cause_new)){
    beepr::beep(9)
    stop(i, ": unclassified detailed cause")
    
  }
  
  # Pivot_longer to have ages in single column
  ctr_who <- ctr_who %>% 
    select(!c(cause,deaths1)) %>% 
    pivot_longer(deaths2:deaths26,names_to="age",names_prefix="deaths") %>% 
    mutate(age=case_when(age==2 ~ 0,
                         age==3 ~ 1,
                         age==4 ~ 2,
                         age==5 ~ 3,
                         age==6 ~ 4,
                         age==7 ~5,
                         age==8 ~ 10,
                         age==9 ~ 15,
                         age==10 ~ 20,
                         age==11 ~ 25,
                         age==12 ~ 30,
                         age==13 ~ 35,
                         age==14 ~ 40,
                         age==15 ~ 45,
                         age==16 ~ 50,
                         age==17 ~ 55,
                         age==18 ~ 60,
                         age==19 ~ 65,
                         age==20 ~ 70,
                         age==21 ~ 75,
                         age==22 ~ 80,
                         age==23 ~ 85,
                         age==24 ~ 90,
                         age==25 ~ 95,
                         age==26 ~ 999),
           age=ifelse(age==999, NA, age))
  
  # Check if there are too many deaths at unspecified ages
  sum <- sum(ctr_who$value[is.na(ctr_who$age)], na.rm=T)
  prop <- sum/sum(ctr_who$value[!is.na(ctr_who$age)], na.rm=T)
  
  # Are there any NA ages (i.e. not even labeled as unspecifed)?
  if(is.na(prop)){
    beepr::beep(9)
    stop(i)
  }
  
  # Are deaths at unspecified ages more than 1% of total deaths?
  if(prop<=0.01){
    # If not, we can eliminate the NA age category
    ctr_who <- ctr_who %>% 
      filter(!is.na(age))
  } else {
    beepr::beep(9)
    stop(i,": too many deaths at NA ages\n")
  }
  
  
  # Sum deaths over detailed causes into grouped causes
  # We don't use summarise() right away because that eliminates some ages (don't know why)
  ctr_who <- ctr_who  %>% 
    group_by(year, sex, age, cause_new) %>% 
    mutate(deaths = sum(value)) %>% 
    ungroup() %>% 
    select(-value) %>%  
    distinct()
  
  # Check third sex category
  sum <- sum(ctr_who$deaths[!(ctr_who$sex %in% c(1,2))], na.rm=T)
  prop <- sum/sum(ctr_who$deaths[ctr_who$sex %in% c(1,2)], na.rm=T)
  
  # Are deaths in third sex category more than 1% of total deaths?
  if(prop<=0.01){
    # If not, we can eliminate the third sex category
    ctr_who <- ctr_who %>% 
      filter(sex %in% c(1,2))
  } else {
    beepr::beep(9)
    stop(i,": too many deaths with undefined sex\n")
  }
  
  
  # Some causes are missing for males or females in random years
  # Let us re-create them with 0 deaths where they are missing
  
  m <- c()
  f <- c()
  ncause <- length(unique(ctr_who$cause_new))
  nyear <- length(unique(ctr_who$year))
  
  # Record which years and sexes have an unexpected number of rows
  # The commented commands allow you to follow R as it checks every year and sex
  # But it goes fast and slows down the whole loop
  # Start loop level 2-B
  for(y in unique(ctr_who$year)){
    nage <- length(unique(ctr_who$age[ctr_who$year==y]))
    if(length(ctr_who$cause_new[ctr_who$year==y])==(ncause*nage*2)){
      #cat(y, "ok \n")
    } else {
      if(length(ctr_who$cause_new[ctr_who$year==y & ctr_who$sex==1])==ncause*nage) {
        #cat(y, "males ok \n")
      } else {
        m[length(m)+1] <- y
        #cat(y, "males", length(unique(ctr_who$cause_new[ctr_who$year==y & ctr_who$sex==1])), "\n")
      }
      if(length(ctr_who$cause_new[ctr_who$year==y & ctr_who$sex==2])==ncause*nage){
        #cat(y, "females ok \n")
      } else {
        f[length(f)+1] <- y
        #cat(y, "females", length(unique(ctr_who$cause_new[ctr_who$year==y & ctr_who$sex==2])), "\n")
      }
    }
  }
  # End loop level 2-B
  
  # Add the missing causes to the necessary years
  causes <- 1:ncause
  
  # Women
  # Start loop level 2-C
  for(y in f){
    c_f <- unique(ctr_who$cause_new[ctr_who$year==y & ctr_who$sex==2]) # Causes already present
    diff_f <- setdiff(causes,c_f) # Differences with vector of expected causes
    # Add necessary causes
    # Start loop level 3-C
    for(c in unique(diff_f)){
      add <- data.frame(country=i,year=rep(y,length(unique(ctr_who$age))),
                        sex=rep(2,length(unique(ctr_who$age))),
                        frmat=rep(unique(ctr_who$frmat[ctr_who$year==y & ctr_who$sex==2]),
                                  length(unique(ctr_who$age))),
                        age=unique(ctr_who$age),cause_new=c,deaths=0)
      ctr_who <- rbind(ctr_who,add)
    }
    # End loop level 3-C
  }
  # End loop level 2-C
  
  # Men (see above for comments)
  # Start loop level 2-D
  for(y in m){
    c_m <- unique(ctr_who$cause_new[ctr_who$year==y & ctr_who$sex==1])
    diff_m <- setdiff(causes,c_m)
    # Start loop level 3-D
    for(c in unique(diff_m)){
      add <- data.frame(country=i,year=rep(y,length(unique(ctr_who$age))),
                        sex=rep(1,length(unique(ctr_who$age))),
                        frmat=rep(unique(ctr_who$frmat[ctr_who$year==y & ctr_who$sex==2]),
                                  length(unique(ctr_who$age))),
                        age=unique(ctr_who$age),cause_new=c,deaths=0)
      ctr_who <- rbind(ctr_who,add)
    }
    # End loop level 3-D
  }
  # End loop level 2-D
  
  # Check that dimensions are now coherent
  if((nrow(ctr_who) == nyear*nage*ncause*2)==FALSE){
    beepr::beep(9)
    stop(i,": wrong dimensions\n")
  }
  
  # Check that deaths over causes equal all-cause deaths
  # Or at least that they are close enough
  prop <- 
    # Difference between sum of cause-specific deaths and all-cause deaths
    (ctr_who %>% 
       filter(cause_new!=1) %>% 
       group_by(year,sex) %>% 
       summarise(sum = sum(deaths, na.rm=T)) %>% 
       ungroup() %>% 
       pull(sum) - 
       ctr_who %>% 
       filter(cause_new==1) %>% 
       group_by(year,sex) %>% 
       summarise(sum = sum(deaths, na.rm=T)) %>% 
       ungroup() %>% 
       pull(sum))/
    # As a proportion of all-cause deaths 
    ctr_who %>% 
    filter(cause_new==1) %>% 
    group_by(year,sex) %>% 
    summarise(sum = sum(deaths, na.rm=T)) %>% 
    ungroup() %>% 
    pull(sum)
  
  
  # Are there any year and sex where the difference is more than 10% of all-cause deaths?
  if(sum(abs(prop)>0.1, na.rm=T)>0){
    beepr::beep(9)
    stop(i,": too large differences between all-cause and sum of cause-specific deaths\n ")
  }
  
  
  # Check that age-specific deaths over causes equal age-specific all-cause deaths
  # Or at least that they are close enough
  prop <- 
    # Difference between sum of age- and cause-specific deaths and all-cause deaths
    (ctr_who %>% 
       filter(cause_new!=1) %>% 
       group_by(year,sex,age) %>%
       summarise(sum = sum(deaths, na.rm=T)) %>% 
       pull(sum) - 
       ctr_who %>% 
       filter(cause_new==1) %>% 
       group_by(year,sex,age) %>% 
       summarise(sum = sum(deaths, na.rm=T)) %>% 
       pull(sum))/
    # As a proportion of age-specific all-cause deaths 
    ctr_who %>% 
    filter(cause_new==1) %>% 
    group_by(year,sex,age) %>% 
    summarise(sum = sum(deaths, na.rm=T)) %>% 
    pull(sum)
  
  # Are there any year, sex and age where the difference is more than 1% of all-cause deaths?
  if(sum(abs(prop)>0.1, na.rm=T)>0){
    beepr::beep(9)
    stop(i,": too large differences between age-specific all-cause and cause-specific deaths. Please exclude\n ")
  }
  
  # Age-specific cause-specific and all-cause deaths are very close, but do not correspond exactly.
  # This is to be expected with not top-notch quality data.
  # However, future manipulations on the data (smoothing with the ungroup package and construction of multiple decrement lifetables)
  # will magnify these differences to yield unreasonable results.
  # So we calculate age-specific all-cause deaths from age- and cause-specific deaths.
  
  ctr_who <- ctr_who %>% 
    filter(cause_new!=1) %>% 
    group_by(year, sex, age) %>% 
    mutate(d_tot = sum(deaths)) %>% 
    ungroup() %>% 
    pivot_longer(cols=c("deaths","d_tot"), names_to = "type") %>% 
    mutate(cause_new = ifelse(type=="deaths", cause_new, 1)) %>% 
    distinct() %>% 
    select(!type) %>% 
    rename(deaths = value) %>% 
    arrange(country, year, sex, frmat, cause_new, age)
  
  # Let's check age-specific differences again
  diff <- (ctr_who %>% 
             filter(cause_new!=1) %>% 
             group_by(year,sex,age) %>%
             summarise(sum = sum(deaths, na.rm=T)) %>% 
             pull(sum) - 
             ctr_who %>% 
             filter(cause_new==1) %>% 
             group_by(year,sex,age) %>% 
             summarise(sum = sum(deaths, na.rm=T)) %>% 
             pull(sum))/
    ctr_who %>% 
    filter(cause_new==1) %>% 
    group_by(year,sex,age) %>% 
    summarise(sum = sum(deaths, na.rm=T)) %>% 
    pull(sum)
  
  if(sum(abs(diff)!=0, na.rm=T)>0){
    beepr::beep(9)
    stop(i,": too large differences between age-specific all-cause and cause-specific deaths. Please exclude\n ")
    break
  }
  
  # Save current dimensions of WHO data
  ncause <- length(unique(ctr_who$cause_new))
  nyear <- length(unique(ctr_who$year))
  nage <- length(unique(ctr_who$age))
  
  # Prepare HMD dataset to be merged with WHO
  
  # Select current country
  ctr_hmd <- hmd %>% 
    filter(country==i, sex %in% c("f","m"))
  
  # Only keep the years present in both WHO and HMD
  years_hmd <- unique(ctr_hmd$year)
  years_who <- unique(ctr_who$year)
  years <- Reduce(intersect, list(years_hmd,years_who))
  
  ctr_hmd <- ctr_hmd %>% 
    filter(year %in% years)
  ctr_who <- ctr_who %>% 
    filter(year %in% years)
  
  # Does the age-format for the WHO data of each country change from year to year?
  if(length(unique(ctr_who$frmat))!=1){
    cat(i,": different age formats\n",
        unique(ctr_who$frmat),"\n")
  }
  
  # Harmonise final age group of lifetables (or not)
  # You can choose whether to harmonise and how before the beginning of the loop (line 44)
  ctr_who_new <- data.table()
  ctr_hmd_new <- data.table()
  
  ctr_who <- as.data.table(ctr_who)
  # Ungroup WHO lifetables to single ages and up to age 110
  if(harmonise==1){
    # Ages 0 should not have 0 deaths, otherwise ungroup (used later) will have unreasonbale results
    # We interpolate such cases with the average of deaths at age 0 the year before and after
    ctr_who_try <- ctr_who %>%
      # NB: we do not filter out other ages, so we end up with a complete dataset
      group_by(sex, cause_new, age) %>%
      mutate(zero = ifelse(deaths==0, 1, 0),
             deaths_lag = lag(deaths),
             deaths_lead = lead(deaths)) %>% 
      rowwise() %>% 
      # Interpolate deaths using the average of the deaths in the year before and after.
      # If that mean rounds to 0, round up
      mutate(deaths_m = ifelse(round(mean(c(deaths_lag,deaths_lead)))==0,
                               ceiling(mean(c(deaths_lag,deaths_lead))),
                               round(mean(c(deaths_lag,deaths_lead))))) %>% 
      ungroup() %>% 
      group_by(sex, cause_new) %>%
      # Replace deaths at age 0 if there are none
      mutate(deaths = ifelse((deaths==0|is.na(deaths)) & age==0,
                             # If deaths are NA, rather than 0, substitute it with first valid deaths
                             ifelse(is.na(deaths_m),coalesce(deaths_lag, deaths_lead),deaths_m),
                             deaths)) %>%
      ungroup() %>%
      # We need to interpolate again to substitute years with no deaths 
      # that were sandwiched between other years with no deaths
      group_by(sex, cause_new, age) %>%
      mutate(deaths_lag = lag(deaths),
             deaths_lead = lead(deaths)) %>% 
      rowwise() %>% 
      mutate(deaths_m = ifelse(round(mean(c(deaths_lag,deaths_lead)))==0,
                               ceiling(mean(c(deaths_lag,deaths_lead))),
                               round(mean(c(deaths_lag,deaths_lead))))) %>% 
      ungroup() %>% 
      group_by(sex, cause_new) %>%
      mutate(deaths = ifelse((deaths==0|is.na(deaths)) & age==0,
                             ifelse(is.na(deaths_m),coalesce(deaths_lag, deaths_lead),deaths_m),
                             deaths)) %>%
      ungroup() %>% 
      # some causes might actually have 0 deaths at age 0
      # we identify them as ages with replaced values of 3 or less
      mutate(deaths = ifelse(zero==1 & deaths<=3 & age==0,1e-10,deaths)) %>% 
      select(!(zero:deaths_m)) %>% 
      as.data.table()
    
    # delete ages with NA and no deaths (they will be interpolate later anyways)
    ctr_who_try$deaths[ctr_who_try$deaths==0] <- NA
    
    ctr_who_try <- ctr_who_try %>%
      drop_na()
    
    # Ungroup and interpolate deaths at all non-0 ages
    ctr_who_new <- ctr_who_try[,list(age = 0:110,
                                     deaths_ungroup = pclm(x = unique(age),
                                                           y = deaths,
                                                           nlast = 111 - max(unique(age)),
                                                           control = list(lambda = 1000000),
                                                           offset = NULL)$fitted),
                               by = list(year,sex,cause_new)]
    
    # some countries have no perinatal and congenital deaths at older ages
    # PCLM puts NaNs for all ages in these categories and years, we replace that with the raw counts
    ctr_who_new <- ctr_who_new %>%
      left_join(ctr_who, by=c("year","sex","cause_new","age")) %>% 
      mutate(deaths_ungroup=ifelse(is.na(deaths_ungroup),
                                   ifelse(is.na(deaths),0,deaths), 
                                   deaths_ungroup)) %>% 
      select(year:deaths_ungroup)
    
    setorder(ctr_who_new, year, sex, cause_new, age)
    
    ctr_hmd_new <- ctr_hmd
    
    # Re-adjust proportions
    
    # Sum of cause-specific deaths for each age
    try_c <- ctr_who_new %>% 
      filter(cause_new!=1) %>% 
      group_by(year,sex,age) %>% 
      summarise(deaths_sum = sum(deaths_ungroup, na.rm=T)) %>% 
      ungroup()
    # Join with age-specific all-cause deaths
    try <- inner_join(try_c, 
                      ctr_who_new %>% 
                        filter(cause_new==1) %>% 
                        select(year,sex,age,deaths_ungroup) %>% 
                        rename(deaths_all = deaths_ungroup),
                      by=c("year", "sex", "age"))
    # Define new age- and cause-specific deaths to fit with age-specific all-cause deaths
    ctr_who_new <- inner_join(ctr_who_new,
                              try, by=c("year", "sex", "age")) %>% 
      mutate(prop = deaths_ungroup/deaths_sum,
             deaths_new = ifelse(cause_new==1,
                                 deaths_all,deaths_all*prop)) %>% 
      select(year,sex,cause_new,age,deaths_new)
  }
  # Harmonise lifetables to specific age
  if(harmonise==2){
    # collapse WHO deaths
    ctr_who_new <- ctr_who %>% 
      mutate(age = ifelse(age >= end_age, end_age, age)) %>% 
      group_by(sex, year, cause_new, age) %>% 
      summarise(country = country,
                deaths = sum(deaths, na.rm=T)) %>% 
      distinct()
    
    # collapse HMD lifetable
    for(y in unique(ctr_hmd$year)){
      for(s in c("f","m")){
        temp <- lt95(ctr_hmd[ctr_hmd$year==y & ctr_hmd$sex==s], age=end_age)
        ctr_hmd_new <- rbind(ctr_hmd_new,cbind(year=y, sex=s, temp))
      }
    }
  } 
  # Leave WHO lifetables as is and collapse HMD lifetables accordingly?
  if(harmonise==3){
    ctr_who_new <- ctr_who
    # Reduce ages of HMD to harmonise with WHO, according to country- and year-specific age-format
    # Start loop level 2-E
    for(y in unique(ctr_hmd$year)){
      # Go through WHO age format codes
      
      # Last age is 95
      if(unique(ctr_who_new$frmat[ctr_who_new$year==y])==0){
        # Start loop level 3-E_1
        for(s in c("f","m")){
          temp <- lt95(ctr_hmd[ctr_hmd$year==y & ctr_hmd$sex==s], age=95)
          ctr_hmd_new <- rbind(ctr_hmd_new,cbind(year=y, sex=s, temp))
        }
        # End loop level 3-E_1
        
        # Last age is 85
      } else if(unique(ctr_who_new$frmat[ctr_who_new$year==y]) %in% 1:2){
        temp <- ctr_who_new %>% 
          # In WHO data, all ages are present, even when they are NA
          filter(!(age %in% c("90","95")), year==y)
        ctr_who_new <- rbind(ctr_who_new[ctr_who_new$year<y,],temp,ctr_who_new[ctr_who_new$year>y,])
        # Start loop level 3-E_2
        for(s in c("f","m")){
          temp <- lt95(ctr_hmd[ctr_hmd$year==y & ctr_hmd$sex==s], age=85)
          ctr_hmd_new <- rbind(ctr_hmd_new,cbind(year=y, sex=s, temp))
        }
        # End loop level 3-E_2
        
        # Last age is 75
      } else if(unique(ctr_who_new$frmat[ctr_who_new$year==y]) %in% 3:4){
        temp <- ctr_who_new %>% 
          filter(!(age %in% c("80","85","90","95")), year==y)
        ctr_who_new <- rbind(ctr_who_new[ctr_who_new$year<y,],temp,ctr_who_new[ctr_who_new$year>y,])
        # Start loop level 3-E_3
        for(s in c("f","m")){
          temp <- lt95(ctr_hmd[ctr_hmd$year==y & ctr_hmd$sex==s], age=75)
          ctr_hmd_new <- rbind(ctr_hmd_new,cbind(year=y, sex=s, temp))
        }
        # End loop level 3-E_3
        
        # Last age is 70
      } else if(unique(ctr_who_new$frmat[ctr_who_new$year==y])==5){
        temp <- ctr_who_new %>% 
          filter(!(age %in% c("75","80","85","90","95")), year==y)
        ctr_who_new <- rbind(ctr_who_new[ctr_who_new$year<y,],temp,ctr_who_new[ctr_who_new$year>y,])
        # Start loop level 3-E_4
        for(s in c("f","m")){
          temp <- lt95(ctr_hmd[ctr_hmd$year==y & ctr_hmd$sex==s], age=70)
          ctr_hmd_new <- rbind(ctr_hmd_new,cbind(year=y, sex=s, temp))
        }
        # End loop level 3-E_4
        
        # Last age is 65
      } else if(unique(ctr_who_new$frmat[ctr_who_new$year==y])==6){
        temp <- ctr_who_new %>% 
          filter(!(age %in% c("70","75","80","85","90","95")), year==y)
        ctr_who_new <- rbind(ctr_who_new[ctr_who_new$year<y,],temp,ctr_who_new[ctr_who_new$year>y,])
        # Start loop level 3-E_5
        for(s in c("f","m")){
          temp <- lt95(ctr_hmd[ctr_hmd$year==y & ctr_hmd$sex==s], age=65)
          ctr_hmd_new <- rbind(ctr_hmd_new,cbind(year=y, sex=s, temp))
        }
        # End loop level 3-E_5
        
        # Unclassified codes
      } else if(unique(ctr_who_new$frmat[ctr_who_new$year==y])>=7){
        beepr::beep(9)
        stop(i,": age format beyond 6, not included in code\n
          skip country or add code")
      }
    }
  }
  
  # Do HMD and WHO data have different ages or different number of rows?
  age_who <- unique(ctr_who_new$age)
  age_hmd <- unique(ctr_hmd_new$age)
  
  # Are ages different, or do HMD rows not correspond to WHO rows divided by number of causes?
  if(length(setdiff(age_who, age_hmd))>0 | nrow(ctr_hmd_new)!=(nrow(ctr_who_new)/ncause)){
    beepr::beep(9)
    stop(i,": after harmonising, HMD and WHO ages don't correspond\n")
  }
  
  # Save current dimensions of WHO data
  ncause <- length(unique(ctr_who_new$cause_new))
  nyear <- length(unique(ctr_who_new$year))
  nage <- length(unique(ctr_who_new$age))
  
  # Let's rename WHO sex categories to harmonise with HMD
  ctr_who_new <- ctr_who_new %>% 
    mutate(sex=ifelse(sex==1,"m","f"))
  
  # Repeat HMD rows to accommodate causes
  # Females
  ctr_hmd_newf <- ctr_hmd_new %>% 
    filter(sex=="f")
  nrow <- nrow(ctr_hmd_newf)
  
  # Replicate number of rows by number of causes
  ctr_hmd_newf <- as.data.table(apply(ctr_hmd_newf,2,rep,times=rep(ncause,nrow)))
  
  # Add column to accommodate cause codes
  ctr_hmd_newf <- add_column(ctr_hmd_newf,
                             .before = 4,
                             cause = rep(1:ncause,nrow))
  
  # Check that numbers of rows correspond
  if(nrow(ctr_hmd_newf)!=nrow(ctr_who_new[ctr_who_new$sex=="f",])){
    beepr::beep(9)
    stop(i,": female causes in HMD do not correspond to expected")
  }
  
  # Males
  ctr_hmd_newm <- ctr_hmd_new %>% 
    filter(sex=="m")
  nrow <- nrow(ctr_hmd_newm)
  
  # Replicate number of rows by number of causes
  ctr_hmd_newm <- as.data.table(apply(ctr_hmd_newm,2,rep,times=rep(ncause,nrow)))
  
  # Add column to accommodate cause codes
  ctr_hmd_newm <- add_column(ctr_hmd_newm,
                             .before = 4,
                             cause = rep(1:ncause,nrow))
  
  # Check that numbers of rows correspond
  if(nrow(ctr_hmd_newm)!=nrow(ctr_who_new[ctr_who_new$sex=="m",])){
    beepr::beep(9)
    stop(i,": male causes in HMD do not correspond to expected")
  }
  
  # Re-unite HMD females and males
  ctr_hmd_new <- as.data.table(rbind(ctr_hmd_newf,ctr_hmd_newm))
  
  # Check that number of rows in HMD and WHO correspond
  if(nrow(ctr_hmd_new)!=nrow(ctr_who_new)){
    beepr::beep(9)
    stop(i,": HMD and WHO nrow do not correspond")
  }
  
  # Merge HMD and WHO data
  
  # Convert variables types and names as needed
  ctr_hmd_new$year <- as.integer(ctr_hmd_new$year)
  ctr_hmd_new$age <- as.integer(ctr_hmd_new$age)
  ctr_who_new <- ctr_who_new %>% 
    rename(cause = cause_new,
           deaths = deaths_new)
  
  # Merge
  merged <- inner_join(ctr_hmd_new, ctr_who_new, 
                       by = c("year", "sex", "age", "cause"))
  
  # Pivot_wider to get causes in different columns
  merged <- merged %>% 
    pivot_wider(names_from="cause", values_from="deaths", names_prefix="d_")
  
  # Add current country to overall dataset
  full_grouped_temp <- as.data.table(rbind(full_grouped_temp,merged))
  
} 
# End loop level 1

# Finished!
beepr::beep(8)


# Lifetable checks --------------------------------------------------------

# Do all-cause deaths sum to 100000?
(check1 <- full_grouped_temp %>% 
   group_by(country, year, sex) %>%
   summarise(sum = sum(as.numeric(dx))) %>% 
   ungroup %>% 
   pull(sum))

# Does the sum of cause-specific deaths equal all-cause deaths?
check_2 <- full_grouped_temp[, list(d_1_sum = sum(d_1), d_2_sum = sum(d_2),
                                    d_3_sum = sum(d_3), d_4_sum = sum(d_4),
                                    d_5_sum = sum(d_5), d_6_sum = sum(d_6)), by=.(year, pop_name, sex)]
check_2 <- check_2[, list(d_1_sum = d_1_sum, sum = sum(d_2_sum,d_3_sum,d_4_sum,d_5_sum,d_6_sum)), by=.(year, pop_name, sex)]

diff <- (check_2$sum - check_2$d_1_sum)/check_2$d_1_sum

if(sum(diff>0.0001)>0){
  beepr::beep(9)
  stop(i,": sum of cause-specific deaths does not equal all-cause deaths")
}

# Does the sum of age- and cause-specific deaths equal age-specific all-cause deaths?
check_3 <- full_grouped_temp[, list(d_1_sum = sum(d_1), d_2_sum = sum(d_2),
                                    d_3_sum = sum(d_3), d_4_sum = sum(d_4),
                                    d_5_sum = sum(d_5), d_6_sum = sum(d_6)), by=.(year, pop_name, sex, age)]
check_3 <- check_3[, list(d_1_sum = d_1_sum, sum = sum(d_2_sum,d_3_sum,d_4_sum,d_5_sum,d_6_sum)), by=.(year, pop_name, sex, age)]

diff <- (check_3$sum - check_3$d_1_sum)/check_3$d_1_sum

range(diff)

diff %>% 
  as.data.frame() %>%
  ggplot() +
  geom_histogram(aes(.))

if(sum(diff>0.0001)>0){
  beepr::beep(9)
  stop(i,": sum of age- and cause-specific deaths does not equal age-specific all-cause deaths")
}

# Estimate multiple decrement lifetables ----------------------------------

# Splitting the data frame into a list, according to ids recording sex, country and year
full_grouped_temp$id   <- paste(full_grouped_temp$sex, full_grouped_temp$country, full_grouped_temp$year, sep= "")
full_list_grouped <- split(full_grouped_temp, list(full_grouped_temp$id))

# Get all the lifetables for all countries over all the years
names_lt_grouped <- names(full_list_grouped)
MDLT_all_grouped <- lapply(names(full_list_grouped), MLT, data=full_list_grouped, causes=1:6)

# Rename the elements of the list
names(MDLT_all_grouped) <- names_lt_grouped

# Change from list to dataframe
full_grouped <- do.call(rbind.data.frame, MDLT_all_grouped)

# Add HMD country codes
for(i in unique(full_grouped$country)){
  full_grouped$pop_name[full_grouped$country==i] <- unique(hmd$pop_name[hmd$country==i]) 
}

# Add country names
full_grouped$pop_name_lab <- factor(full_grouped$pop_name,
                                    levels = c("HUN","POL","BGR",
                                               "BEL","AUT","FRATNP","NLD",
                                               "ESP","ITA","PRT"),
                                    labels = c("Hungary","Poland","Bulgaria",
                                               "Belgium","Austria","France","Netherlands",
                                               "Spain","Italy","Portugal"))

# Split countries in regions
West <- c("BEL","DEUTNP","AUT",
          "FRATNP","LUX","NLD",
          "GBR_NP","IRL")

East <- c("CZE","EST","HUN",
          "LTU","LTV","POL","SVN",
          "LVA","BGR","HRV",
          "RUS","SVK","UKR")

South <- c("ESP","GRC","ITA","PRT")

full_grouped$region[full_grouped$pop_name%in%West] <- "West"
full_grouped$region[full_grouped$pop_name%in%East] <- "East"
full_grouped$region[full_grouped$pop_name%in%South] <- "South"

# Save full dataset
save(full_grouped, file="Data/full_grouped.RData")

# Clean up
rm(list = ls()) 
