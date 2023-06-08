###################################################################
# Aim: Plots of results tied to the coefficient of variation squared
# Date: 17/02/2023
# Notes: Requires running previous scripts
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
###################################################################


# Figure 3 - CV2 trends ----------------------------------------------

# Where to add the labels to the plot
data_text <- data.frame(
  label = c("Italy", "Portugal", "Spain", "Austria", "Belgium", "France", "Netherlands", "Bulgaria", "Hungary", "Poland"),
  x     = c(   1980,       1982,    1981,      2000,      1995,     1981,          1985,       2017,      1992,    2005,
               1988,       1989,    1986,      1983,      1983,     1993,          2010,       2017,      2016,    2000),
  y     = c(    .05,        .07,     .03,      .023,      .045,     .052,           .025,        .043,       .06,     .025,
                .04,        .08,     .062,     .075,      .05,     .065,           .023,       .06,      .035,     .05),
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

components_grouped_cv$region[components_grouped_cv$pop_name%in%West] <- "West"
components_grouped_cv$region[components_grouped_cv$pop_name%in%East] <- "East"
components_grouped_cv$region[components_grouped_cv$pop_name%in%South] <- "South"

components_grouped_cv  <- components_grouped_cv %>% 
  mutate(pop_name_lab = factor(pop_name, levels=c("ITA","AUT","BGR",
                                                  "PRT","BEL","HUN",
                                                  "ESP","FRATNP","POL",
                                                  "NLD"),
                               labels=c("Italy","Austria","Bulgaria",
                                        "Portugal","Belgium","Hungary",
                                        "Spain","France","Poland",
                                        "Netherlands")))


components_grouped_cv %>% 
  filter(cause==1) %>% 
  mutate(region = factor(region, levels=c("South", "West","East"))) %>%
  ggplot() + 
  ggtitle(expression("CV"^2~" trends")) +
  geom_line(aes(x=as.numeric(year), y=C, col=pop_name_lab, group=pop_name), linewidth=2, alpha=.6) +
  scale_colour_viridis_d("Country", guide=F) +
  scale_x_continuous("Year", minor_breaks=NULL) +
  scale_y_continuous(expression("CV"^2), minor_breaks=NULL) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15),
        axis.text.y = element_text(size=15),
        #legend.position = "",
        legend.direction = "vertical") +
  grids(col="grey95") +
  facet_grid(rows=vars(region),col=vars(sex), labeller=labeller(sex=c("f"="Women","m"="Men"))) +
  geom_text(data=data_text %>% mutate(region = factor(region, levels=c("South", "West","East"))),
            mapping = aes(x = x, y = y, label = label, col=label), show.legend=F)
ggsave(filename="FigureD1.png", 
       path= "Plots",
       width=35,height=25, units="cm", device="png")


# Figure 4 - CV2 level decomposition ------------------------------------------

# Coefficient for double y axis
coeff <- .1

# Women
# Determine order of countries for women (by highest variance in first year)
order_f_cv <- components_grouped_cv %>% 
  filter(year==year_first, cause==1, sex=="f") %>% 
  arrange(desc(C)) %>% 
  pull(pop_name_lab)


components_grouped_cv %>% 
  filter(year%in%c(1985,2000,2015), sex=="f", cause!=1) %>% 
  # Names of causes
  mutate(cause = factor(cause,
                        levels=2:6,
                        labels=c("Neoplasms",  "Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>% 
  # Order countries by order_f_cv
  mutate(pop_name_lab = factor(pop_name_lab, levels=order_f_cv)) %>%
  ggplot(mapping=aes(x = pop_name_lab)) +
  ggtitle("Women") +
  geom_col(aes(y = C, fill = fct_rev(cause)), position = "fill") +
  geom_hline(yintercept=.25, linetype="dashed", col="grey50", size=1.2) +
  geom_hline(yintercept=.50, linetype="dashed", col="grey50", size=1.2) +
  geom_hline(yintercept=.75, linetype="dashed", col="grey50", size=1.2) +
  geom_point(data=components_grouped_cv %>% 
               filter(year%in%c(1985,2000,2015), cause==1, sex=="f"),
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
ggsave(filename="FigureD2_f.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")


# Men
# Determine order of countries for men (by highest variance in first year)
order_m <- components_grouped_cv %>% 
  filter(year==year_first, cause==1, sex=="m") %>% 
  arrange(desc(C)) %>% 
  pull(pop_name_lab)

components_grouped_cv %>% 
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
  geom_point(data=components_grouped_cv %>% 
               filter(year%in%c(1985,2000,2015), cause==1, sex=="m"),
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
ggsave(filename="FigureD2_m.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")



# Figure 5 - CV2 counterfactual difference decomposition -------------------

# Put together all difference decomposition
dec_cv <- rbind.data.frame(cbind(dec_first_cv,year="1985-2000"),
                           cbind(dec_last_cv,year="2000-2015"),
                           cbind(dec_full_cv,year="1985-2015"))

# Women
dec_cv %>% 
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
ggsave(filename="FigureD3_f.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")

# Men
dec_cv %>% 
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
ggsave(filename="FigureD3_m.png",
       path= "Plots",
       width=35, height=25, units="cm", device="png")
