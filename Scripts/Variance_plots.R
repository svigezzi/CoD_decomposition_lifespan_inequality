###################################################################
# Aim: Plots of results tied to the variance
# Date: 17/02/2023
# Notes: Requires running previous scripts
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
###################################################################


# Figure 3 - Variance trends ----------------------------------------------

# Where to add the labels to the plot
data_text <- data.frame(
  label = c("Italy", "Portugal", "Spain", "Austria", "Belgium", "France", "Netherlands", "Bulgaria", "Hungary", "Poland"),
  x     = c(   1980,       2017,    1985,      2000,      1980,     2015,          1982,       2017,      2017,    1981,
               1985,       2017,    1989,      2019,      1980,     2015,          2017,       2012,      2013,    1983),
  y     = c(    265,        180,     200,       165,       280,      200,           205,        230,       165,     330,
                240,        230,     305,       218,       270,      250,           160,        290,       210,     390),
  region = c("South", "South", "South", "West", "West", "West", "West","East", "East", "East"),
  sex = c("f", "f", "f", "f", "f", "f", "f", "f", "f", "f",
          "m", "m", "m", "m", "m", "m", "m", "m", "m", "m")
)

West <- c("BEL","DEUTNP","AUT",
          "FRATNP","LUX","NLD",
          "GBR_NP","IRL")

East <- c("CZE","EST","HUN",
          "LTU","LTV","POL","SVN",
          "LVA","BGR","HRV",
          "RUS","SVK","UKR")

South <- c("ESP","GRC","ITA","PRT")

components_grouped$region[components_grouped$pop_name%in%West] <- "West"
components_grouped$region[components_grouped$pop_name%in%East] <- "East"
components_grouped$region[components_grouped$pop_name%in%South] <- "South"

components_grouped <- components_grouped %>% 
  mutate(pop_name_lab = factor(pop_name, levels=c("ITA","AUT","BGR",
                                                  "PRT","BEL","HUN",
                                                  "ESP","FRATNP","POL",
                                                  "NLD"),
                               labels=c("Italy","Austria","Bulgaria",
                                        "Portugal","Belgium","Hungary",
                                        "Spain","France","Poland",
                                        "Netherlands")))

components_grouped %>% 
  filter(cause==1) %>% 
  mutate(region = factor(region, levels=c("South", "West","East"))) %>%
  ggplot() + 
  ggtitle("Variance trends") +
  geom_line(aes(x=as.numeric(year), y=C, col=pop_name_lab, group=pop_name), linewidth=2, alpha=.6) +
  scale_colour_viridis_d("Country") +
  scale_x_continuous("Year", minor_breaks=NULL) +
  scale_y_continuous("Variance", minor_breaks=NULL) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15),
        axis.text.y = element_text(size=15),
        legend.position = "",
        legend.direction = "vertical") +
  guides(color=guide_legend(ncol=1, byrow=TRUE)) +
  grids(col="grey95") +
  facet_grid(rows=vars(region),col=vars(sex), labeller=labeller(sex=c("f"="Women","m"="Men"))) +
  geom_text(data=data_text %>% mutate(region = factor(region, levels=c("South", "West","East"))),
            mapping = aes(x = x, y = y, label = label, col=label))
ggsave(filename="Figure3.png", 
       path= "Plots",
       width=35,height=25, units="cm", device="png")


# Figure 4 - Variance level decomposition ------------------------------------------

# Coefficient for double y axis
coeff <- 375

# Women
# Determine order of countries for women (by highest variance in first year)
order_f <- components_grouped %>% 
  filter(year==year_first, cause==1, sex=="f") %>% 
  arrange(desc(C)) %>% 
  pull(pop_name_lab)


components_grouped %>% 
  filter(year%in%c(1985,2000,2015), sex=="f", cause!=1) %>% 
  # Names of causes
  mutate(cause = factor(cause,
                        levels=2:6,
                        labels=c("Neoplasms",  "Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>% 
  # Order countries by order_f
  mutate(pop_name_lab = factor(pop_name_lab, levels=order_f)) %>%
  ggplot(mapping=aes(x = pop_name_lab)) +
  ggtitle("Women") +
  geom_col(aes(y = C, fill = fct_rev(cause)), position = "fill") +
  geom_hline(yintercept=.25, linetype="dashed", col="grey50", size=1.2) +
  geom_hline(yintercept=.50, linetype="dashed", col="grey50", size=1.2) +
  geom_hline(yintercept=.75, linetype="dashed", col="grey50", size=1.2) +
  geom_point(data=components_grouped %>% 
               filter(year%in%c(1985,2000,2015), sex=="f", cause==1),
             aes(y=C/coeff), col="grey80", size=3) +
  scale_x_discrete("Country") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Proportional Contribution",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Variance")
  ) +
  scale_fill_viridis_d("Cause", option="G", direction=-1) +
  theme_tufte(ticks=F) +
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15, vjust=1,
                                   hjust=1),
        axis.text.y = element_text(size=15),
        axis.ticks.length.x = unit(.25,"cm"),
        axis.ticks.x = element_line(size=.5),
        legend.direction = "vertical",
        legend.key.height = unit(2,"cm")) +
  grids(col="grey95") +
  facet_wrap("year")
ggsave(filename="Figure4_f.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")


# Men
# Determine order of countries for men (by highest variance in first year)
order_m <- components_grouped %>% 
  filter(year==year_first, cause==1, sex=="m") %>% 
  arrange(desc(C)) %>% 
  pull(pop_name_lab)

components_grouped %>% 
  filter(year%in%c(year_first,year_middle,year_last), sex=="m", cause!=1) %>% 
  mutate(cause = factor(cause,
                        levels=2:6,
                        labels=c("Neoplasms",  "Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>% 
  mutate(pop_name_lab = factor(pop_name_lab, levels=order_m)) %>%
  ggplot(mapping=aes(x = pop_name_lab)) +
  ggtitle("Men") +
  geom_col(aes(y = C, fill = fct_rev(cause)), position = "fill") +
  geom_hline(yintercept=.25, linetype="dashed", col="grey50", size=1.2) +
  geom_hline(yintercept=.50, linetype="dashed", col="grey50", size=1.2) +
  geom_hline(yintercept=.75, linetype="dashed", col="grey50", size=1.2) +
  geom_point(data=components_grouped %>% 
               filter(year%in%c(1985,2000,2015), sex=="m", cause==1),
             aes(y=C/coeff), col="grey80", size=3) +
  scale_x_discrete("Country") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Proportional Contribution",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Variance")
  ) +
  scale_fill_viridis_d("Cause", option="G", direction=-1) +
  theme_tufte(ticks=F) +
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15, vjust=1,
                                   hjust=1),
        axis.text.y = element_text(size=15),
        axis.ticks.length.x = unit(.25,"cm"),
        axis.ticks.x = element_line(size=.5),
        legend.direction = "vertical",
        legend.key.height = unit(2,"cm")) +
  grids(col="grey95") +
  facet_wrap("year")
ggsave(filename="Figure4_m.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")


# Figure 5 - Variance counterfactual difference decomposition -------------------

# Put together all difference decomposition
dec <- rbind.data.frame(cbind(dec_first,year="1985-2000"),
                        cbind(dec_last,year="2000-2015"),
                        cbind(dec_full,year="1985-2015"))

# Women
dec %>% 
  filter(!is.na(region), sex=="f") %>%
  mutate(P = as.numeric(P),
         M = as.numeric(M),
         I = as.numeric(I),
         C = P+M+I) %>% 
  # Pivot longer to accommodate ggplot2
  pivot_longer(cols=c("P","M","I","C"),
               names_to="measure") %>% 
  mutate(year = factor(year,
                       levels=c("1985-2000","2000-2015","1985-2015")),
         cause = factor(cause,
                        levels=1:5,
                        labels=c("Neoplasms",  "Circulatory",
                                 "Perinatal and\n congenital", "External", "Other")),
         measure = factor(measure, labels=c("I","P","M","Total"),
                          levels=c("I","P","M","C")),
         sex = factor(sex,
                      levels=c("f","m"),
                      labels=c("Females","Males"))) %>%
  mutate(pop_name_lab = factor(pop_name_lab, levels=rev(c("Italy","Portugal","Spain",
                                                          "Austria","Belgium","France","Netherlands",
                                                          "Bulgaria","Hungary","Poland"))),
         region = factor(region, levels=c("South", "West","East"))) %>%
  ggplot(aes(y = pop_name_lab, x = value, fill = fct_rev(cause))) +
  ggtitle("Women") +
  geom_col() +
  geom_vline(xintercept=0) +
  scale_x_continuous("Contribution", minor_breaks=NULL) +
  ylab("Country") +
  scale_fill_viridis_d("Cause", option="G", direction=-1) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15, vjust=1,
                                   hjust=1),
        axis.text.y = element_text(size=15),
        axis.ticks.length.x = unit(.25,"cm"),
        axis.ticks.x = element_line(linewidth=.5),
        legend.direction = "vertical",
        legend.key.height = unit(2,"cm"),
        strip.text.y.right = element_text(angle = 0)) +
  grids(col="grey95") +
  facet_grid(measure~year)
ggsave(filename="Figure5_f.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")

# Men
dec %>% 
  filter(!is.na(region), sex=="m") %>%
  mutate(P = as.numeric(P),
         M = as.numeric(M),
         I = as.numeric(I),
         C = P+M+I) %>% 
  # Pivot longer to accommodate ggplot2
  pivot_longer(cols=c("P","M","I","C"),
               names_to="measure") %>% 
  mutate(year = factor(year,
                       levels=c("1985-2000","2000-2015","1985-2015")),
         cause = factor(cause,
                        levels=1:5,
                        labels=c("Neoplasms",  "Circulatory",
                                 "Perinatal and\n congenital", "External", "Other")),
         measure = factor(measure, labels=c("I","P","M","Total"),
                          levels=c("I","P","M","C")),
         sex = factor(sex,
                      levels=c("f","m"),
                      labels=c("Females","Males"))) %>%
  mutate(pop_name_lab = factor(pop_name_lab, levels=rev(c("Italy","Portugal","Spain",
                                                          "Austria","Belgium","France","Netherlands",
                                                          "Bulgaria","Hungary","Poland"))),
         region = factor(region, levels=c("South", "West","East"))) %>%
  
  ggplot(aes(y = pop_name_lab, x = value, fill = fct_rev(cause))) +
  ggtitle("Men") +
  geom_col() +
  geom_vline(xintercept=0) +
  scale_x_continuous("Contribution", minor_breaks=NULL) +
  ylab("Country") +
  scale_fill_viridis_d("Cause", option="G", direction=-1) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15, vjust=1,
                                   hjust=1),
        axis.text.y = element_text(size=15),
        axis.ticks.length.x = unit(.25,"cm"),
        axis.ticks.x = element_line(size=.5),
        legend.direction = "vertical",
        legend.key.height = unit(2,"cm"),
        strip.text.y.right = element_text(angle = 0)) +
  grids(col="grey95") +
  facet_grid(measure~year)
ggsave(filename="Figure5_m.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")
