###################################################################
# Aim: Calculate decomposition results for the variance
# Date: 16/02/2023
# Notes: Requires running previous scripts
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
###################################################################

source("Scripts/Functions.R")

load("Data/full_grouped.RData")

# IPM level decomposition -------------------------------------------------------

components_grouped <- data.table()

# Did you miss the loops?
# Select country
for(i in unique(full_grouped$country)){
  data_ctr <- full_grouped %>% 
    filter(country==i)
  # Select year
  for(y in unique(data_ctr$year)){
    data_ctry <- data_ctr %>% 
      filter(year==y)
    # Select sex
    for(s in unique(data_ctry$sex)){
      data <- data_ctry %>% 
        filter(sex==s) %>% 
        select(country, year, sex, age, cause, ax, dx_i, lx_i, ex_i, pop_name)
      # P component
      prop <- data %>% 
        group_by(cause) %>% 
        summarise(P = sum(dx_i)/100000)
      
      # M component
      m <- data[,list(M=ex_i[1]), by=.(cause)]
      # I component (variance)
      v <- data[,list(I=sum(as.numeric(dx_i)/lx_i[1]*(as.numeric(age) + as.numeric(ax) - as.numeric(ex_i[1]))^2)), by=.(cause)]
      
      # Join them together
      all <- inner_join(prop, m, by="cause") %>% 
        inner_join(v, by="cause") %>% 
        as.data.table()
      
      # Whole cause-specific contribution
      ex <- data$ex_i[data$cause==1 & data$age==0]
      diff <- all[,list(diff=(M - ex)^2), by=.(cause)]
      cont <- inner_join(all, diff, by="cause")[,list(P=P, M=M, I=I, C=P*(diff+I)), by=.(cause)]
      
      final <- data.table(cbind(country=i,pop_name=unique(data$pop_name),
                                year=y,sex=s,cont))
      components_grouped <- data.table(rbind(components_grouped,final))
    }
  }
}

# Checking results

# All-cause contributions should be equal to all-cause variance
try <- components_grouped %>% 
  filter(cause==1, !is.na(country)) %>% 
  mutate(equal = (round(C,4)==I))

range(try[which(try$equal==F)]$I - try[which(try$equal==F)]$C)

(try[which(try$equal==F)]$I - try[which(try$equal==F)]$C) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# The sum of cause-specific contributions should be equal to all-cause contributions
range(components_grouped %>% 
        filter(cause!=1, !is.na(country)) %>% 
        group_by(country, year, sex) %>% 
        summarise(sum = sum(C)) %>% pull(sum) -
        components_grouped %>%
        filter(cause==1, !is.na(country)) %>% pull(C))

(components_grouped %>% 
    filter(cause!=1, !is.na(country)) %>% 
    group_by(country, year, sex) %>% 
    summarise(sum = sum(C)) %>% pull(sum) -
    components_grouped %>%
    filter(cause==1, !is.na(country)) %>% pull(C)) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# Get a list of countries where sum of cause-specific contributions is too different from all-cause variance 

# Make it into a list to go faster
components_grouped$id   <- paste(components_grouped$sex, components_grouped$country, components_grouped$year, sep= "")
components_list_grouped <- split(components_grouped, list(components_grouped$id))

# Difference as a proportion of all-cause variance 
check_var_per <- lapply(names(components_list_grouped), check.var.per, data=components_list_grouped)
range(unlist(check_var_per))
names(check_var_per) <- names(components_list_grouped)

unlist(check_var_per) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# Difference as a number
check_var_abs <- lapply(names(components_list_grouped), check.var.abs, data=components_list_grouped)
range(unlist(check_var_abs))
names(check_var_abs) <- names(components_list_grouped)

unlist(check_var_abs) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# Which are the countries where the proportional different is higher than x%?
x <- .01
not <- unique(substr(names(components_list_grouped)[lapply(check_var_per,abs)>x], start=2,stop=5))
not

save(components_grouped, file="Data/components_grouped.RData")

# IPM counterfactual difference decomposition ------------------------------------------------------

# Select the years
first <- components_grouped %>%
  filter(year==year_first) %>% 
  group_by(pop_name, year) %>% 
  mutate(cause = factor(cause,
                        levels=c("1","2","3","4",
                                 "5","6"),
                        labels=c("All","Neoplasms","Circ",
                                 "Per and cong", "Ext", "Other")),
         P=round(as.numeric(P),6),
         M=round(as.numeric(M),6),
         I=round(as.numeric(I),6),
         C=round(as.numeric(C),6)) %>% 
  ungroup()

middle <- components_grouped %>%
  filter(year==year_middle) %>% 
  group_by(pop_name, year) %>% 
  mutate(cause = factor(cause,
                        levels=c("1","2","3","4",
                                 "5","6"),
                        labels=c("All","Neoplasms","Circ",
                                 "Per and cong", "Ext", "Other")),
         P=round(as.numeric(P),6),
         M=round(as.numeric(M),6),
         I=round(as.numeric(I),6),
         C=round(as.numeric(C),6)) %>% 
  ungroup()


last <- components_grouped %>%
  filter(year==year_last) %>% 
  group_by(pop_name, year) %>% 
  mutate(cause = factor(cause,
                        levels=c("1","2","3","4",
                                 "5","6"),
                        labels=c("All","Neoplasms","Circ",
                                 "Per and cong", "Ext", "Other")),
         P=round(as.numeric(P),6),
         M=round(as.numeric(M),6),
         I=round(as.numeric(I),6),
         C=round(as.numeric(C),6)) %>% 
  ungroup()


# Decomposition between first and middle year
data_first <- rbind.data.frame(first[first$sex=="f",],middle[middle$sex=="f",],
                               first[first$sex=="m",],middle[middle$sex=="m",])

data_first$id <- str_sub(data_first$id,end=5)

data_first_list <- split(data_first, list(data_first$id))

# Actual difference decomposition
dec_first <- lapply(names(data_first_list),FUN=counterfactual,  
                    data=data_first_list,
                    causes=1:5,
                    all="All", measure="v")

names(dec_first) <- names(data_first_list)

# Turn into a dataframe and assign country names
dec_first <- do.call(rbind.data.frame, dec_first)
dec_first$pop_name_lab <- factor(dec_first$pop_name,
                                 levels = c("HUN","POL","BGR",
                                            "BEL","AUT","FRATNP","NLD",
                                            "ESP","ITA","PRT"),
                                 labels = c("Hungary","Poland","Bulgaria",
                                            "Belgium","Austria","France","Netherlands",
                                            "Spain","Italy","Portugal"))


# Decomposition between middle and last year
data_last <- rbind.data.frame(middle[middle$sex=="f",],last[last$sex=="f",],
                              middle[middle$sex=="m",],last[last$sex=="m",])

data_last$id <- str_sub(data_last$id,end=5)

data_last_list <- split(data_last, list(data_last$id))

# Actual difference decomposition
dec_last <- lapply(names(data_last_list),FUN=counterfactual,  
                   data=data_last_list,
                   causes=1:5, all="All", measure="v")

names(dec_last) <- names(data_last_list)

# Turn into a dataframe and assign country names
dec_last <- do.call(rbind.data.frame, dec_last)
dec_last$pop_name_lab <- factor(dec_last$pop_name,
                                levels = c("HUN","POL","BGR",
                                           "BEL","AUT","FRATNP","NLD",
                                           "ESP","ITA","PRT"),
                                labels = c("Hungary","Poland","Bulgaria",
                                           "Belgium","Austria","France","Netherlands",
                                           "Spain","Italy","Portugal"))



# Decomposition between first and last year
data_full <- rbind.data.frame(first[first$sex=="f",],last[last$sex=="f",],
                              first[first$sex=="m",],last[last$sex=="m",])
data_full$id <- str_sub(data_full$id,end=5)

data_full_list <- split(data_full, list(data_full$id))

# Actual difference decomposition
dec_full <- lapply(names(data_full_list),FUN=counterfactual,  
                   data=data_full_list,
                   causes=1:5, all="All", measure="v")

names(dec_full) <- names(data_full_list)

# Turn into a dataframe and assign country names
dec_full <- do.call(rbind.data.frame, dec_full)
dec_full$pop_name_lab <- factor(dec_full$pop_name,
                                levels = c("HUN","POL","BGR",
                                           "BEL","AUT","FRATNP","NLD",
                                           "ESP","ITA","PRT"),
                                labels = c("Hungary","Poland","Bulgaria",
                                           "Belgium","Austria","France","Netherlands",
                                           "Spain","Italy","Portugal")) 


# Assign regions
West <- c("BEL","DEUTNP","AUT",
          "FRATNP","LUX","NLD",
          "GBR_NP","IRL")

East <- c("CZE","EST","HUN",
          "LTU","LTV","POL","SVN",
          "LVA","BGR","HRV",
          "RUS","SVK","UKR")

South <- c("ESP","GRC","ITA","PRT")

dec_first$region[dec_first$pop_name%in%West] <- "West"
dec_first$region[dec_first$pop_name%in%East] <- "East"
dec_first$region[dec_first$pop_name%in%South] <- "South"

dec_last$region[dec_last$pop_name%in%West] <- "West"
dec_last$region[dec_last$pop_name%in%East] <- "East"
dec_last$region[dec_last$pop_name%in%South] <- "South"

dec_full$region[dec_full$pop_name%in%West] <- "West"
dec_full$region[dec_full$pop_name%in%East] <- "East"
dec_full$region[dec_full$pop_name%in%South] <- "South"
