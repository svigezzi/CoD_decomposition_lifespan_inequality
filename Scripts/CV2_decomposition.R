#################################################################################
# Aim: Calculate decomposition results for the coefficient of variation squared
# Date: 16/02/2023
# Notes: Requires running previous scripts
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
#################################################################################

components_grouped_cv <- data.table()

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
      # Here is the only difference with the variance decomposition
      cont <- inner_join(all, diff, by="cause")[,list(P=P, M=M, I=I, C=(P*(diff+I))/ex^2), by=.(cause)]
      
      final <- data.table(cbind(country=i,pop_name=unique(data$pop_name),
                                year=y,sex=s,cont))
      components_grouped_cv <- data.table(rbind(components_grouped_cv,final))
    }
  }
}

# Checking results

# All-cause contributions should be equal to all-cause variance
try <- components_grouped_cv %>% 
  filter(cause==1, !is.na(country)) %>% 
  mutate(equal = (round(C,4)==I))

range(try[which(try$equal==F)]$I - try[which(try$equal==F)]$C)

(try[which(try$equal==F)]$I - try[which(try$equal==F)]$C) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# The sum of cause-specific contributions should be equal to all-cause contributions
range(components_grouped_cv %>% 
        filter(cause!=1, !is.na(country)) %>% 
        group_by(country, year, sex) %>% 
        summarise(sum = sum(C)) %>% pull(sum) -
        components_grouped_cv %>%
        filter(cause==1, !is.na(country)) %>% pull(C))

(components_grouped_cv %>% 
    filter(cause!=1, !is.na(country)) %>% 
    group_by(country, year, sex) %>% 
    summarise(sum = sum(C)) %>% pull(sum) -
    components_grouped_cv %>%
    filter(cause==1, !is.na(country)) %>% pull(C)) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# Get a list of countries where sum of cause-specific contributions is too different from all-cause variance 

# Make it into a list to go faster
components_grouped_cv$id   <- paste(components_grouped_cv$sex, components_grouped_cv$country, components_grouped_cv$year, sep= "")
components_list_grouped_cv <- split(components_grouped_cv, list(components_grouped_cv$id))

# Difference as a proportion of all-cause variance 
check_var_per_cv <- lapply(names(components_list_grouped_cv), check.var.per, data=components_list_grouped_cv)
range(unlist(check_var_per_cv))
names(check_var_per_cv) <- names(components_list_grouped_cv)

unlist(check_var_per_cv) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# Difference as a number
check_var_abs_cv <- lapply(names(components_list_grouped_cv), check.var.abs, data=components_list_grouped_cv)
range(unlist(check_var_abs_cv))
names(check_var_abs_cv) <- names(components_list_grouped_cv)

unlist(check_var_abs_cv) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_histogram(aes(.))

# Which are the countries where the proportional different is higher than x%?
x <- .01
not <- unique(substr(names(components_list_grouped_cv)[lapply(check_var_per_cv,abs)>x], start=2,stop=5))
not

save(components_grouped_cv, file="Data/components_grouped_cv.RData")

# IPM counterfactual difference decomposition ------------------------------------------------------

# Select the years
first_cv <- components_grouped_cv %>%
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

middle_cv <- components_grouped_cv %>%
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


last_cv <- components_grouped_cv %>%
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
data_first_cv <- rbind.data.frame(first_cv[first_cv$sex=="f",],middle_cv[middle_cv$sex=="f",],
                                  first_cv[first_cv$sex=="m",],middle_cv[middle_cv$sex=="m",])

data_first_cv$id <- str_sub(data_first_cv$id,end=5)

data_first_list_cv <- split(data_first_cv, list(data_first_cv$id))

# Actual difference decomposition
dec_first_cv <- lapply(names(data_first_list_cv),FUN=counterfactual,  
                       data=data_first_list_cv,
                       causes=1:5,
                       all="All", measure="cv2")

names(dec_first_cv) <- names(data_first_list_cv)

# Turn into a dataframe and assign country names
dec_first_cv <- do.call(rbind.data.frame, dec_first_cv)
dec_first_cv$pop_name_lab <- factor(dec_first_cv$pop_name,
                                    levels = c("HUN","POL","BGR",
                                               "BEL","AUT","FRATNP","NLD",
                                               "ESP","ITA","PRT"),
                                    labels = c("Hungary","Poland","Bulgaria",
                                               "Belgium","Austria","France","Netherlands",
                                               "Spain","Italy","Portugal"))


# Decomposition between middle and last year
data_last_cv <- rbind.data.frame(middle_cv[middle_cv$sex=="f",],last_cv[last_cv$sex=="f",],
                                 middle_cv[middle_cv$sex=="m",],last_cv[last_cv$sex=="m",])

data_last_cv$id <- str_sub(data_last_cv$id,end=5)

data_last_list_cv <- split(data_last_cv, list(data_last_cv$id))

# Actual difference decomposition
dec_last_cv <- lapply(names(data_last_list_cv),FUN=counterfactual,  
                      data=data_last_list_cv,
                      causes=1:5, all="All", measure="cv2")

names(dec_last_cv) <- names(data_last_list_cv)

# Turn into a dataframe and assign country names
dec_last_cv <- do.call(rbind.data.frame, dec_last_cv)
dec_last_cv$pop_name_lab <- factor(dec_last$pop_name,
                                   levels = c("HUN","POL","BGR",
                                              "BEL","AUT","FRATNP","NLD",
                                              "ESP","ITA","PRT"),
                                   labels = c("Hungary","Poland","Bulgaria",
                                              "Belgium","Austria","France","Netherlands",
                                              "Spain","Italy","Portugal"))



# Decomposition between first and last year
data_full_cv <- rbind.data.frame(first_cv[first_cv$sex=="f",],last_cv[last_cv$sex=="f",],
                                 first_cv[first_cv$sex=="m",],last_cv[last_cv$sex=="m",])
data_full_cv$id <- str_sub(data_full_cv$id,end=5)

data_full_list_cv <- split(data_full_cv, list(data_full_cv$id))

# Actual difference decomposition
dec_full_cv <- lapply(names(data_full_list_cv),FUN=counterfactual,  
                      data=data_full_list_cv,
                      causes=1:5, all="All", measure="cv2")

names(dec_full_cv) <- names(data_full_list_cv)

# Turn into a dataframe and assign country names
dec_full_cv <- do.call(rbind.data.frame, dec_full_cv)
dec_full_cv$pop_name_lab <- factor(dec_full_cv$pop_name,
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

dec_first_cv$region[dec_first_cv$pop_name%in%West] <- "West"
dec_first_cv$region[dec_first_cv$pop_name%in%East] <- "East"
dec_first_cv$region[dec_first_cv$pop_name%in%South] <- "South"

dec_last_cv$region[dec_last_cv$pop_name%in%West] <- "West"
dec_last_cv$region[dec_last_cv$pop_name%in%East] <- "East"
dec_last_cv$region[dec_last_cv$pop_name%in%South] <- "South"

dec_full_cv$region[dec_full_cv$pop_name%in%West] <- "West"
dec_full_cv$region[dec_full_cv$pop_name%in%East] <- "East"
dec_full_cv$region[dec_full_cv$pop_name%in%South] <- "South"
