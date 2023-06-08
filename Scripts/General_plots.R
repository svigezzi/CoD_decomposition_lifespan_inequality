#######################################################################
# Aim: Plots, not tied to variance or coefficient of variation squared
# Date: 17/02/2023
# Notes: Requires running previous scripts
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
#######################################################################

load("Data/who.RData")

# Figure 2 - Cause-of-death lifetable deaths distribution -----------------

full_grouped %>%
  filter(sex=="f", !is.na(region), year %in% c(1985,2000,2015), 
         pop_name%in%c("NLD","ESP","HUN")) %>%
  mutate(cause = factor(cause,
                        levels=c("1","2","3","4",
                                 "5","6"),
                        labels=c("All","Neoplasms","Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>%
  ggplot() +
  ggtitle("Women") +
  geom_line(aes(x=age,y=dx_i,col=as.factor(year),
                group=interaction(year,pop_name))) +
  scale_colour_viridis_d("Year",option="B", end=.9) +
  scale_x_continuous("Age", limits=c(0,110), minor_breaks=NULL) + 
  scale_y_continuous("Lifetable Deaths", minor_breaks=NULL) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15),
        axis.text.y = element_text(size=15),
        legend.key.height = unit(2,"cm")) +
  grids(col="grey95") +
  facet_grid(rows=vars(pop_name_lab), cols=vars(cause))
ggsave(filename="figure2_f.png", 
       path= "Plots",
       width=35,height=25, units="cm", device="png")

full_grouped %>%
  filter(sex=="m", !is.na(region), year %in% c(1985,2000,2015), pop_name%in%c("NLD","ESP","HUN")) %>%
  mutate(cause = factor(cause,
                        levels=c("1","2","3","4",
                                 "5","6"),
                        labels=c("All","Neoplasms","Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>%
  ggplot() +
  ggtitle("Men") +
  geom_line(aes(x=age,y=dx_i,col=as.factor(year),
                group=interaction(year,pop_name))) +
  scale_colour_viridis_d("Year",option="B", end=.9) +
  scale_x_continuous("Age", limits=c(0,110), minor_breaks=NULL) + 
  scale_y_continuous("Lifetable Deaths", minor_breaks=NULL) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15),
        axis.text.y = element_text(size=15),
        legend.key.height = unit(2,"cm")) +
  grids(col="grey95") +
  facet_grid(rows=vars(pop_name_lab), cols=vars(cause))
ggsave(filename="figure2_m.png", 
       path= "Plots",
       width=35,height=25, units="cm", device="png")



# Appendices (except for results of coefficient of variation squared) --------------------------------------------------------------

# Figure B1 - Cause-specific proportion of deaths

last <- who %>% 
  # Filter for countries included in analysis
  filter(country%in%unique(components_grouped %>% pull(country)), who=="9") %>% 
  group_by(country,sex) %>% 
  # Which is the last year of ICD-9 for each country and sex?
  mutate(last9 = last(year),
         country=factor(country),
         sex=ifelse(sex==2,"f","m")) %>%
  ungroup() %>%
  select(country,sex,last9) %>% 
  distinct()

# Join last ICD-9 year with data on proportion of deaths
inner_join(components_grouped, last, by=c("country","sex")) %>% 
  filter(cause!=1) %>% 
  mutate(sex = ifelse(sex=="f", "Women", "Men"),
         Cause = factor(cause,
                        levels=2:6,
                        labels=c("Neoplasms",  "Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>%
  ggplot() +
  geom_col(aes(x=as.numeric(year), y=P, fill=fct_rev(Cause))) +
  geom_vline(aes(xintercept=last9+0.5), linewidth=1, linetype="dashed", colour="grey60") +
  xlab("Year") +
  ylab("Proportion") +
  scale_fill_viridis_d("Cause", option="G", direction=-1, guide=F) +
  theme_tufte(ticks=F) + 
  facet_grid(cols=vars(pop_name_lab), rows=vars(sex))
ggsave(filename="FigureB1.png",
       path= "Plots",
       width=35, height=10, units="cm", device="png")


# Figure C1 - Age at death distributions for all countries

full_grouped %>%
  filter(sex=="f", !is.na(region), year %in% c(1985,2000,2015), 
         !(pop_name%in%c("NLD","ESP","HUN"))) %>% # take out countries shown in main text (Figure 2)
  mutate(cause = factor(cause,
                        levels=c("1","2","3","4",
                                 "5","6"),
                        labels=c("All","Neoplasms","Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>%
  ggplot() +
  ggtitle("Women") +
  geom_line(aes(x=age,y=dx_i,col=as.factor(year),
                group=interaction(year,pop_name))) +
  scale_colour_viridis_d("Year",option="B", end=.9) +
  scale_x_continuous("Age", limits=c(0,110), minor_breaks=NULL) + 
  scale_y_continuous("Lifetable Deaths", minor_breaks=NULL) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15),
        axis.text.y = element_text(size=15),
        legend.key.height = unit(2,"cm")) +
  grids(col="grey95") +
  facet_grid(rows=vars(pop_name_lab), cols=vars(cause))
ggsave(filename="FigureC1_f.png", 
       path= "Plots",
       width=35,height=25, units="cm", device="png")

full_grouped %>%
  filter(sex=="m", !is.na(region), year %in% c(1985,2000,2015), 
         !(pop_name%in%c("NLD","ESP","HUN"))) %>% # take out countries shown in main text (Figure 2)
  mutate(cause = factor(cause,
                        levels=c("1","2","3","4",
                                 "5","6"),
                        labels=c("All","Neoplasms","Circulatory",
                                 "Perinatal and\n congenital", "External", "Other"))) %>%
  ggplot() +
  ggtitle("Men") +
  geom_line(aes(x=age,y=dx_i,col=as.factor(year),
                group=interaction(year,pop_name))) +
  scale_colour_viridis_d("Year",option="B", end=.9) +
  scale_x_continuous("Age", limits=c(0,110), minor_breaks=NULL) + 
  scale_y_continuous("Lifetable Deaths", minor_breaks=NULL) +
  theme_tufte(ticks=F) + 
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=45, size=15),
        axis.text.y = element_text(size=15),
        legend.key.height = unit(2,"cm")) +
  grids(col="grey95") +
  facet_grid(rows=vars(pop_name_lab), cols=vars(cause))
ggsave(filename="FigureC1_m.png", 
       path= "Plots",
       width=35,height=25, units="cm", device="png")
