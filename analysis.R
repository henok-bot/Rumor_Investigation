############################################################################-
########################## SWEPR rumor investigation analysis ############## 
############################################################################-

# Load the necessary packages
pacman::p_load(
  tidyverse,     # General data management and visualization
  epiDisplay,    # Epidemiological displays and statistics
  epiR,          # Epidemiological analysis functions
  lubridate,     # Date and time manipulation
  janitor,       # Data cleaning
  rio,           # Data import/export
  gtsummary,     # Summary tables and statistics
  forcats,       # Factor management
  ggpubr,        # Publication-ready plots
  here,           # Manage file paths
  epitools, epibasix, survival, RColorBrewer, extrafont, RVAideMemoire
)

font_import(prompt = FALSE)
loadfonts(device = "win")

mdata <- import("SEWPR_merged.xlsx")

glimpse(mdata)
summary(mdata)
str(mdata)



# Pie chart for sex distribution ########

mdata %>% count(sex) %>% 
  ggplot(aes(x="", y= n, fill = sex)) +
  geom_col(width = 1) +
  coord_polar(theta = "y")+
  labs(title = "Sex distribution of study participants", y= "count") +
  theme_void() +
  theme(legend.position = "right")

mdata %>% count(sex) %>% mutate(percentage = n/sum(n)*100) %>% 
  ggplot(aes(x="", y= n, fill = sex)) +
  geom_col(width = 1) +
  coord_polar(theta = "y")+
  labs(title = "Sex distribution of study participants", y= "count") +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 1, face = "bold"))+
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5), color = "white")


# Column chart for age distribution  ##################
mdata <- mdata %>% mutate(age1 = as.factor(age))

mdata <- mdata %>% mutate(agegroup = cut(age, 
                                         breaks = seq(0, max(age)+5, by = 5),right = FALSE,
                                         labels = paste0(seq(0, max(age), by = 5), "-",
                                                         seq(4, max(age) + 4, by = 5))))

ggplot(mdata, aes(x = agegroup)) + 
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of study participants", x = "Age", y = "count")+
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

   # to plot with a label on top of the bars
agegroup_counts <- mdata %>% group_by(agegroup) %>% 
  summarise(count = n(), .groups = 'drop')

ggplot(agegroup_counts, aes(x = agegroup,y= count)) + 
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, size = 4 ) +
  labs(title = "Age Distribution of study participants", x = "Age", y = "count")+
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Bar chart proportion of patients number of visits ########

mdata_pi %>% count(visit_times) %>% 
  ggplot(aes(x= as.factor(visit_times), y=n, fill = as.factor(visit_times)))+
  geom_bar(stat = "identity") +
  labs(title = "patient number of visits over six month", x = "number of visits",
       y="count") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# To reorder bars based on burden and add percentage on top

visit_summary <-  mdata %>% count(visit_times) %>% 
  mutate(percentage = n/sum(n) * 100)

ggplot(visit_summary, aes(x= fct_reorder(as.factor(visit_times), n, .desc = TRUE),
                          y = n, fill = as.factor(visit_times))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage,1), "%")), vjust = -0.5, size = 4)+
  
  labs(title = "Number of times patient visited in the previous six months", 
       x = "Number of visits",
       y="Count",
       fill = "Visit frequency") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Bar chart for Zone and woreda of study participants #######

mdata %>% filter(!is.na(zone)) %>% count(zone) %>% mutate(percentage = n/sum(n)*100) %>% 
  ggplot(aes(x= reorder(zone, n), y= n, fill = zone)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "zone distribuiton of study participats",x = "Zone", y = "count")+
  theme_minimal()


 # since only two zone changed to pie chart

mdata %>% filter(!is.na(zone)) %>% count(zone) %>% 
  mutate(percentage = n/sum(n)*100) %>% 
  ggplot(aes(x="", y= n, fill = zone)) +
  geom_col(width = 1) +
  coord_polar(theta = "y")+
  labs(title = "Zone  of study participants", y= "count") +
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5), color = "black", fontface = "bold")+
theme_void() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 1., face = "bold"))
  

mdata %>%
  filter(!is.na(zone)) %>%  # Remove NA values
  count(zone) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(zone, ": ", round(percentage, 1), "%")) %>%
  ggplot(aes(x = "", y = percentage, fill = zone)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Zone Distribution of Study Participants") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

 # woreda distribution column chart 

#recode woreda names

table(mdata$woreda, useNA = "always")
mdata <- mdata %>% mutate(woreda = recode(woreda,
                                          "Debub bench" = "Debub Bench",
                                          "Mizan aman" = "Mizan Aman",
                                          "Semen bench" = "Semen Bench"))

mdata %>% filter(!is.na(woreda)) %>% count(woreda) %>% mutate(percentage = n/sum(n)*100) %>% 
  ggplot(aes(x= reorder(woreda, n), y= n, fill = woreda)) +
  geom_bar(stat = "identity",  fill = "#4BACC6") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
  coord_flip() +
  labs(title = "Woreda  of study participats",x = "Woreda", y = "Count")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", family = "Arial"),
        axis.title = element_text(family = "Arial"),
        axis.text = element_text(family = "Arial")) +
  theme(legend.position = "none")
  

#Species diagnosis colum chart

table(mdata_pi$dx_current)

dx_summary <-  mdata_pi %>% drop_na(dx_current) %>% count(dx_current) %>% 
  mutate(percentage = n/sum(n) * 100)

ggplot(dx_summary, aes(x= fct_reorder(as.factor(dx_current), n, .desc = TRUE),
                          y = n, fill = as.factor(dx_current))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage,1), "%")), vjust = -0.5, size = 4)+
  
  labs(title = "Diagnosed species in the current visit", 
       x = "Species type",
       y="Count",
       fill = "species type") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Duration between visits ####

mdata_pi <- mdata_pi %>% mutate(dur_bn_visit_lat = difftime(visit_date,latest_date, 
                                                      units = "days"))


ggplot(mdata, aes(x = dur_bn_visit_lat))+
  geom_histogram(fill = "lightblue", color = "black", binwidth = 7) +
  labs(title = "Duration Between Visits", x = "Days", y = "Frequency") +
  theme_minimal() 
  

# in weeks if necessary
ggplot(mdata, aes(x = dur_bn_visit_lat))+
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Duration Between Visits", x = "Weeks", y = "Frequency") +
  theme_minimal()

# to add a line indicating 28 days  and the median value 
median_dur1 <- median(mdata$dur_bn_visit_lat, na.rm = TRUE)
per_bel_28 <- sum(mdata$dur_bn_visit_lat <= 28, na.rm = TRUE)/
  sum(!is.na(mdata$dur_bn_visit_lat))*100
per_bel_med <- sum(mdata$dur_bn_visit_lat <= median_dur1, na.rm = TRUE)/
  sum(!is.na(mdata$dur_bn_visit_lat))*100


ggplot(mdata, aes(x = dur_bn_visit_lat))+
  geom_histogram(fill = "lightblue", color = "black", binwidth = 7) +
  geom_vline(aes(xintercept = 28), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = median_dur1), color = "blue", linetype = "dashed")+
  annotate("text", x = 28, y = Inf, 
           label = paste0("<28days/n(",round(per_bel_28,1),"%)"), 
           vjust = 2, hjust = 1, color = "red") +
  annotate("text", x = median_dur1, y = Inf, 
           label = paste0("Median:", round(median_dur1,1),
            "days/n(below median = ", round(per_bel_med,1),"%)"),vjust = 2, hjust = -0.1, color = "blue")+
  labs(title = "Distribution of  duration between current and prior Visits", x = "Weeks", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  

## Simple comparison of factors  and repeated visit 

# Place of visit

mdata_pi <- mdata_pi %>% mutate(visit_group_dn = ifelse(dur_bn_visit_lat <= 28, "<=28 days", 
                                                        ">28 days" )) %>% 
  filter(!latest_hf == "Hospital") %>% 
  mutate(lat_med_dur_c = ifelse(latest_med_dur <= 2, "Less than 3 days", 
                                                                     "3 days" ))

mdata_pi %>% drop_na(latest_hf,visit_group_dn) %>%   
ggplot(aes(x = latest_hf, fill = visit_group_dn)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Place of visit comparision by return time",
       x = "Place of visit", y = "count", fill = "return time") +
  theme_minimal() 
 
# To reorder the bars and drop hospital since it is only one observation 

# Calculate counts first
latest_hf_counts <- mdata_pi %>% 
  drop_na(latest_hf, visit_group_dn) %>% 
  group_by(latest_hf, visit_group_dn) %>% 
  summarise(count = n(), .groups = 'drop') %>% filter(!latest_hf == "Hospital")

# Plot with reordered bars
ggplot(latest_hf_counts, aes(x = reorder(latest_hf, -count), y = count, fill = visit_group_dn)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Duration between Visits comparision by Place of Visits",
       x = "Place of Visit", y = "Count", fill = "Duration between visits") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.7, face = "bold"))

p <- mood.medtest(dur_bn_visit_lat ~ latest_hf, data = mdata_pi)

wilcox.test(as.numeric(dur_bn_visit_lat) ~ latest_hf, data = mdata_pi, exact = FALSE)

print(p)

# Place of medication purchase -----------#

# Calculate counts first
latest_purc_pla_counts <- mdata_pi %>% 
  drop_na(latest_purchase_place, visit_group_dn) %>% 
  group_by(latest_purchase_place, visit_group_dn) %>% 
  summarise(count = n(), .groups = 'drop') 

# Plot with reordered bars
ggplot(latest_purc_pla_counts, aes(x = reorder(latest_purchase_place, -count), 
                                   y = count, fill = visit_group_dn)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Duration between Visits comparison by Place of Medicaiton purchase",
       x = "Place of medicaiton purchase", y = "Count", fill = "Duration between visits") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.25, face = "bold"))

mood.medtest(dur_bn_visit_lat ~ latest_purchase_place, data = mdata_pi)
wilcox.test(as.numeric(dur_bn_visit_lat) ~ latest_purchase_place, 
            data = mdata_pi, exact = FALSE)

x= fct_reorder(as.factor(visit_times), n, .desc = TRUE)
# Duration of medication -----------#


table(mdata_pi$latest_med_dur)

table(mdata_pi$latest_med_dur_spe)


med_dur_count <- mdata_pi %>%  filter(!latest_med_dur == "other") %>% 
  drop_na(latest_med_dur, visit_group_dn)%>% 
   mutate(lat_med_dur_c = ifelse(latest_med_dur <= 2, "Less than 3 days", 
                                                        "3 days" )) %>% 
  group_by(lat_med_dur_c, visit_group_dn) %>% 
  summarise(count = n(), .groups = 'drop')

ggplot(med_dur_count, aes(x = reorder(lat_med_dur_c, -count),
                          y = count, fill = visit_group_dn)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Duration between Visits comparision by treatment adherence",
       x = "treatment span", y = "count", fill = "Duration betwee visits") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.25, face = "bold"))

mood.medtest(dur_bn_visit_lat ~ lat_med_dur_c, data = mdata_pi)
wilcox.test(as.numeric(dur_bn_visit_lat) ~ lat_med_dur_c, 
            data = mdata_pi, exact = FALSE)


glimpse(med_dur_count)
view(med_dur_count)


# Define repeated visit outcome variable 
mdata <- mdata %>% mutate(rep_visit = ifelse(visit_times_nu>=2, "yes", "No"))
table(mdata$rep_visit)

mdata_pi <- mdata %>% filter(source == "PI") %>% drop_na(net_f, net_use_f)



mdata <- mdata %>% mutate(rep_visit_n = if_else(rep_visit == "yes",1,0)) %>% 
  mutate(rep_visit_n = as.factor(rep_visit_n))

mdata <- mdata %>% mutate(net_f = as.factor(net),
                          net_use_f = as.factor(net_use),
                          invns_f = as.factor(invns),
                          invs = as.vector(invns))

table(mdata$net_f)
table(mdata$net_use_f) 


logistic_model <- glm(rep_visit_n ~ net_use_f+ invs , 
                      data = mdata, 
                      family = binomial(link = "logit"))

summary(logistic_model)



summary(mdata_pi$net_f)
summary(mdata$net_use_f)
summary(mdata$invs)
sum(is.na(mdata$invns))
sum(!is.na(mdata$invns))

view(mdata)
glimpse(mdata)


mdata %>% group_by(source) %>% summarise(count = n())






