### This is an R script to analyze
### The ability of FrogID to estimate
### species richness compared with 'known' species richness
### I have cleaned the data and provide the 'richness per grid'
### for both FrogID and Cogger
### so some of these packages might not be necessary

### packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(vegan)
library(tibble)
library(mgcv)
library(patchwork)
library(purrr)
library(broom)
library(mgcv.helper)
library(forcats)


source("R/global_functions.R")

# read in grid dataset necessary for later
## spatial dataset
load("Data/aus_grids.RData")

# will also need this grid_lookup table
grid_lookup <- readRDS("Data/grid_lookup.RDS")


####################################################################
####################################################################
##### NOW START THE ANALYSIS
####################################################################
####################################################################

## see species richness per grid for hal
richness_grid_hal <- readRDS("Data/cogger_grid_richness.RDS")


## see species richness per grid for frogid
richness_grid_frogid <- readRDS("Data/frogid_grid_richness.RDS")


## make scatterplot of the two
## combine two richness points
true_richness <- richness_grid_hal %>%
  left_join(., richness_grid_frogid) %>%
  replace_na(list(Species_richness_frogid=0))

ggplot(true_richness, aes(y=Species_richness_hal, x=Species_richness_frogid))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))

#ggsave("Figures/scatterplot_comparison.png", 
#       width=5.5, height=4.2, units="in")

# there are a lot of points here
# and we aren't too worried about making predictions from these data
# we just want to know the correlation
# so we decided to use a linear model, because in this instance, 
# the simplicity is fine for our case
summary(lm(Species_richness_hal ~ Species_richness_frogid, data=true_richness))
# glm shows similar pattern, so we stick with linear model in presentation
summary(glm(Species_richness_hal ~ Species_richness_frogid, family="poisson", data=true_richness))

## plot the same thing, but get rid of any 'unsampled'
## grids by FrogID
true_richness_no_zeros <- true_richness %>%
  dplyr::filter(Species_richness_frogid > 0) %>%
  dplyr::filter(Species_richness_hal > 0)


ggplot(true_richness_no_zeros, aes(y=Species_richness_hal, x=Species_richness_frogid))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))

#ggsave("Richness_estimation_paper/Figures/scatterplot_comparison_no_zeros.png", 
#       width=5.5, height=4.2, units="in")

summary(lm(Species_richness_hal ~ Species_richness_frogid, data=true_richness_no_zeros))
# glm shows similar pattern, so we stick with linear model in presentation
summary(glm(Species_richness_hal ~ Species_richness_frogid, family="poisson", data=true_richness_no_zeros))

## Now put the plots together using patchwork
a <- ggplot(true_richness, aes(y=Species_richness_hal, x=Species_richness_frogid))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("a)")

b <- ggplot(true_richness_no_zeros, aes(y=Species_richness_hal, x=Species_richness_frogid))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("b)")

a + b + plot_layout(ncol=2)

#ggsave("Richness_estimation_paper/Figures/scatterplot_comparison_both.png", 
#      width=7.5, height=4.2, units="in")

## plot a histogram of the difference between
## cogger and FrogID
## this is of the 'absolute difference' between the two datasets
true_richness %>%
  mutate(diff=Species_richness_hal-Species_richness_frogid) %>%
  ggplot(., aes(x=diff))+
  geom_histogram(color="black", fill="blue", bins=30)+
  xlab("Difference between Cogger and FrogID")+
  ylab("Count")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  geom_vline(xintercept=0, color="red")

#ggsave("Richness_estimation_paper/Figures/difference_histogram.png", 
#      width=5.5, height=4.2, units="in")

# summarize this information for the paper
true_richness %>%
  mutate(diff=Species_richness_hal-Species_richness_frogid) %>%
  summarize(median=median(diff),
            mean=mean(diff),
            sd=sd(diff))

true_richness %>%
  mutate(diff=Species_richness_hal-Species_richness_frogid) %>%
  dplyr::filter(diff<0) %>%
  summarise(N=n(),
            min=min(diff))


## Try plotting the richness values in space for each of
## the two sources
plasma_pal <- c(viridis::plasma(n = 6))

aus_grids %>%
  dplyr::select(grid_id, lon, lat) %>%
  right_join(., richness_grid_hal, by="grid_id") %>%
  distinct() %>%
  left_join(., richness_grid_frogid, by="grid_id") %>%
  replace_na(list(Species_richness_frogid=0)) %>%
  dplyr::select(-lon, -lat) %>%
  right_join(., grid_lookup, by="grid_id") %>%
  gather(., key="Dataset", value="Species_richness", Species_richness_hal, Species_richness_frogid) %>%
  mutate(Dataset=gsub("Species_richness_hal", "Cogger", .$Dataset)) %>%
  mutate(Dataset=gsub("Species_richness_frogid", "FrogID", .$Dataset)) %>%
  ggplot(., aes(x=lon, y=lat))+
  geom_point(aes(color=Species_richness), size=2)+
  theme_classic()+
  xlab("")+
  ylab("")+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  theme(axis.line=element_blank())+
  facet_wrap(~Dataset)+
  theme(legend.position="bottom")+
  scale_color_gradientn(colours=plasma_pal)+
  labs(color="Species richness")

#ggsave("Richness_estimation_paper/Figures/raw_map_comparison.png", 
#       width=7.5, height=4.2, units="in")

## Plot the absolute difference in space
true_richness %>%
  mutate(diff=Species_richness_hal-Species_richness_frogid) %>%
  ggplot(., aes(x=lon, y=lat, color=diff))+
  geom_point(size=1.2)+
  xlab("")+
  ylab("")+
  theme_classic()+
  scale_color_gradientn(colours=plasma_pal)+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  theme(axis.line=element_blank())+
  theme(legend.position="right")+
  labs(color="Absolute\ndifference")

#ggsave("Richness_estimation_paper/Figures/map_of_absolute_differences.png", 
#       width=4.1, height=2.8, units="in")

## Now plot the standardized relative difference
## when comparing the two datasets in space
## done by taking the absolute difference and then
## dividing by the known richness in a grid cell
true_richness %>%
  mutate(diff=Species_richness_hal-Species_richness_frogid) %>%
  mutate(relative_diff=diff/Species_richness_hal) %>%
  ggplot(., aes(x=lon, y=lat, color=relative_diff))+
  geom_point(size=1.2)+
  xlab("")+
  ylab("")+
  theme_classic()+
  scale_color_gradient2()+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  theme(axis.line=element_blank())+
  theme(legend.position="right")+
  labs(color="Relative\ndifference")

#ggsave("Richness_estimation_paper/Figures/map_of_relative_differences.png", 
#       width=4.1, height=2.8, units="in")

## summarize a quick statistic of the above for the paper
relative_difference <- true_richness %>%
  mutate(diff=Species_richness_hal-Species_richness_frogid) %>%
  mutate(relative_diff=1-(diff/Species_richness_hal))

median(relative_difference$relative_diff)
mean(relative_difference$relative_diff)
sd(relative_difference$relative_diff)


### read in estimated frog ID species richness per grid
### this was done separately and is not available here
### as the FrogID raw data are not yet publicly available
frogid_estimated_richness <- readRDS("Data/frogid_estimated_richness_grids.RDS")


### plot estimated richness versus
### cogger true richness
estimated_richness <- richness_grid_hal %>%
  left_join(., frogid_estimated_richness, by="grid_id") %>%
  replace_na(list(jack1=0))

ggplot(estimated_richness, aes(y=Species_richness_hal, x=jack1))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID estimated species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))

#ggsave("Richness_estimation_paper/Figures/scatterplot_comparison_estimated.png", 
#       width=5.5, height=4.2, units="in")

summary(lm(Species_richness_hal ~ jack1, data=estimated_richness))

## Now repeat the above, but remove the zeros (as we did with the 'true richness')
## plot the same thing, but get rid of any 'unsampled'
## grids by FrogID
estimated_richness_no_zeros <- estimated_richness %>%
  dplyr::filter(jack1 > 0) %>%
  dplyr::filter(Species_richness_hal > 0)


ggplot(estimated_richness_no_zeros, aes(y=Species_richness_hal, x=jack1))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID estimated species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))

#ggsave("Richness_estimation_paper/Figures/scatterplot_comparison_estimated_no_zeros.png", 
#       width=5.5, height=4.2, units="in")

summary(lm(Species_richness_hal ~ jack1, data=estimated_richness_no_zeros))


## Combine the plots and save
## will be a supplementary figure
a <- ggplot(estimated_richness, aes(y=Species_richness_hal, x=jack1))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID estimated species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("a)")

b <- ggplot(estimated_richness_no_zeros, aes(y=Species_richness_hal, x=jack1))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method="lm")+
  ylab("Cogger species richness")+
  xlab("FrogID estimated species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("b)")

a + b + plot_layout(ncol=2)

#ggsave("Richness_estimation_paper/Figures/scatterplot_comparison_both_estimated.png", 
#       width=7.5, height=4.2, units="in")


## plot actual richness versus samples
ggplot(estimated_richness, aes(x=n, y=Species))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method="lm")+
  xlab("# of FrogID submissions")+
  ylab("Species richness")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))

#ggsave("Richness_estimation_paper/Figures/richness_vs_sample_size.png", 
#       width=5.5, height=4.2, units="in")

summary(lm(Species ~ n, data=estimated_richness))


### Now I want to see how many samples (submissions)
### are required to meet the threshold of comparison with Hal data
### I also think it is informative to find how many samples are necessary 
### to get 90%
### 80%
### and 70% of the hal richness
samples_analysis <- estimated_richness %>%
  replace_na(list(Species=0)) %>%
  mutate(threshold_100 = ifelse(Species >= Species_richness_hal, "Yes", "No")) %>%
  mutate(threshold_90 = ifelse(Species >= (0.9*Species_richness_hal), "Yes", "No")) %>%
  mutate(threshold_80 = ifelse(Species >= (0.8*Species_richness_hal), "Yes", "No")) %>%
  mutate(threshold_70 = ifelse(Species >= (0.7*Species_richness_hal), "Yes", "No")) %>%
  mutate(threshold_60 = ifelse(Species >= (0.6*Species_richness_hal), "Yes", "No")) %>%
  mutate(threshold_50 = ifelse(Species >= (0.5*Species_richness_hal), "Yes", "No"))

# A bit hacky here to get to what I want!
a <- samples_analysis %>%
  dplyr::select(-threshold_90, -threshold_80, -threshold_70, -threshold_60, -threshold_50) %>%
  rename(Threshold = threshold_100) %>%
  mutate(Value=case_when(
    Threshold=="Yes" ~ "Yes",
    Threshold=="No" ~ "No"
  )) %>%
  mutate(Threshold="100%")

b <- samples_analysis %>%
  dplyr::select(-threshold_100, -threshold_80, -threshold_70, -threshold_60, -threshold_50) %>%
  rename(Threshold = threshold_90) %>%
  mutate(Value=case_when(
    Threshold=="Yes" ~ "Yes",
    Threshold=="No" ~ "No"
  )) %>%
  mutate(Threshold="90%")

c <- samples_analysis %>%
  dplyr::select(-threshold_100, -threshold_90, -threshold_70, -threshold_60, -threshold_50) %>%
  rename(Threshold = threshold_80) %>%
  mutate(Value=case_when(
    Threshold=="Yes" ~ "Yes",
    Threshold=="No" ~ "No"
  )) %>%
  mutate(Threshold="80%")

d <- samples_analysis %>%
  dplyr::select(-threshold_100, -threshold_80, -threshold_90, -threshold_60, -threshold_50) %>%
  rename(Threshold = threshold_70) %>%
  mutate(Value=case_when(
    Threshold=="Yes" ~ "Yes",
    Threshold=="No" ~ "No"
  )) %>%
  mutate(Threshold="70%")

e <- samples_analysis %>%
  dplyr::select(-threshold_100, -threshold_80, -threshold_90, -threshold_70, -threshold_50) %>%
  rename(Threshold = threshold_60) %>%
  mutate(Value=case_when(
    Threshold=="Yes" ~ "Yes",
    Threshold=="No" ~ "No"
  )) %>%
  mutate(Threshold="60%")

f <- samples_analysis %>%
  dplyr::select(-threshold_100, -threshold_80, -threshold_70, -threshold_60, -threshold_90) %>%
  rename(Threshold = threshold_50) %>%
  mutate(Value=case_when(
    Threshold=="Yes" ~ "Yes",
    Threshold=="No" ~ "No"
  )) %>%
  mutate(Threshold="50%")

samples_plot <- bind_rows(a, b, c, d, e, f) %>%
  rename(`Threshold met` = Value)

ggplot(samples_plot, aes(x=Threshold, y=n, fill=`Threshold met`))+
  geom_violin(position=position_dodge()) +
  geom_boxplot(width=0.1, color="black", position = position_dodge(width =0.9))+
  scale_y_log10()+
  ylab("# of FrogID submissions")+
  xlab("Threshold")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  scale_x_discrete(limits=c("50%", "60%", "70%", "80%", "90%", "100%"),
                   labels=c("50%", "60%", "70%", "80%", "90%", "100%"))+
  labs(fill="Threshold met")


#ggsave("Richness_estimation_paper/Figures/thresholds_versus_cogger.png", 
#       width=5.8, height=4.2, units="in")


## Summarize median number of samples needed for each threshold
## for summary stuff for the paper
samples_plot %>%
  dplyr::filter(`Threshold met` == "Yes") %>%
  group_by(Threshold) %>%
  summarise(med=median(n))


## Write a function which
## makes a specaccum object for each grid and returns a dataframe
## for plotting
## this function won't work as the frogid data are not
## publicly available and thus not in this repository
## but I left the function in the script for posterity sake
## the data from this function, however, are available and read in below the function

# spec_accumulation_function <- function(id_of_grid) {
#   
# dat <- frogid %>%
#   dplyr::filter(grid_id == id_of_grid) %>%
#   dplyr::select(id, species) %>%
#   mutate(presence=1) %>%
#   spread(., species, presence) %>%
#   replace(., is.na(.), 0) %>%
#   column_to_rownames("id")
#   
# temp <- specaccum(dat, method="rarefaction")
# 
# plot_df <- data.frame(Samples=temp$sites,
#                       Richness=temp$richness,
#                       sd=temp$sd) %>%
#   mutate(grid_id=id_of_grid) %>%
#   left_join(., richness_grid_hal, by="grid_id") %>%
#   rename(Total_richness = Species_richness_hal) %>%
#   mutate(richness_90=Total_richness*.9) %>%
#   mutate(richness_80=Total_richness*.8) %>%
#   mutate(richness_70=Total_richness*.7) %>%
#   mutate(richness_60=Total_richness*.6) %>%
#   mutate(richness_50=Total_richness*.5) %>%
#   mutate(threshold_50=ifelse(Richness >= richness_50, "Yes", "No")) %>%
#   mutate(threshold_60=ifelse(Richness >= richness_60, "Yes", "No")) %>%
#   mutate(threshold_70=ifelse(Richness >= richness_70, "Yes", "No")) %>%
#   mutate(threshold_80=ifelse(Richness >= richness_80, "Yes", "No")) %>%
#   mutate(threshold_90=ifelse(Richness >= richness_90, "Yes", "No")) %>%
#   mutate(threshold_100=ifelse(Richness >= Total_richness, "Yes", "No"))
# 
# plot_df %>%
#   dplyr::select(threshold_50, Samples) %>%
#   dplyr::filter(threshold_50=="Yes") %>%
#   slice(1)
# 
# 'Yes' %in% plot_df$threshold_50
# 
# match('Yes', plot_df$threshold_50)
# 
# 
# threshold_50_df <- data.frame(value=ifelse('Yes' %in% plot_df$threshold_50, 
#                                            slice(plot_df, match('Yes', plot_df$threshold_50))$Samples,
#                                            NA)) %>%
#   mutate(Threshold="50%")
# 
# threshold_60_df <- data.frame(value=ifelse('Yes' %in% plot_df$threshold_60, 
#                                            slice(plot_df, match('Yes', plot_df$threshold_60))$Samples,
#                                            NA)) %>%
#   mutate(Threshold="60%")
# 
# threshold_70_df <- data.frame(value=ifelse('Yes' %in% plot_df$threshold_70, 
#                                            slice(plot_df, match('Yes', plot_df$threshold_70))$Samples,
#                                            NA)) %>%
#   mutate(Threshold="70%")
# 
# threshold_80_df <- data.frame(value=ifelse('Yes' %in% plot_df$threshold_80, 
#                                            slice(plot_df, match('Yes', plot_df$threshold_80))$Samples,
#                                            NA)) %>%
#   mutate(Threshold="80%")
# 
# threshold_90_df <- data.frame(value=ifelse('Yes' %in% plot_df$threshold_90, 
#                                            slice(plot_df, match('Yes', plot_df$threshold_90))$Samples,
#                                            NA)) %>%
#   mutate(Threshold="90%")
# 
# threshold_100_df <- data.frame(value=ifelse('Yes' %in% plot_df$threshold_100, 
#                                            slice(plot_df, match('Yes', plot_df$threshold_100))$Samples,
#                                            NA)) %>%
#   mutate(Threshold="100%")
# 
# summary_df <- bind_rows(threshold_50_df,
#                         threshold_60_df,
#                         threshold_70_df,
#                         threshold_80_df,
#                         threshold_90_df,
#                         threshold_100_df) %>%
#   mutate(grid_id=id_of_grid) %>%
#   mutate(value=round(value))
# 
# return(summary_df)
# 
# }

#results_list_accumulation_curves <- lapply(grids_to_estimate, function(x){spec_accumulation_function(x)})

#accumulation_curves <- bind_rows(results_list_accumulation_curves)

# read in the data from the repository
accumulation_curves <- readRDS("Data/accumulation_curves_analysis.RDS")

## Summarise this analysis
ggplot(accumulation_curves, aes(x=Threshold, y=value))+
  geom_violin(position=position_dodge(), fill="gray80") +
  geom_boxplot(width=0.1, fill="green", color="black", position = position_dodge(width =0.9))+
  scale_y_log10()+
  ylab("# of FrogID submissions")+
  xlab("Threshold")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  scale_x_discrete(limits=c("50%", "60%", "70%", "80%", "90%", "100%"),
                   labels=c("50%", "60%", "70%", "80%", "90%", "100%"))+
  geom_label(aes(1, 2500,
                 label = paste0("N=", accumulation_curves %>% 
                   dplyr::filter(Threshold=="50%") %>% 
                   na.omit() %>% 
                   nrow(.))))+
  geom_label(aes(2, 2500,
                 label = paste0("N=", accumulation_curves %>% 
                                  dplyr::filter(Threshold=="60%") %>% 
                                  na.omit() %>% 
                                  nrow(.))))+
  geom_label(aes(3, 2500,
                 label = paste0("N=", accumulation_curves %>% 
                                  dplyr::filter(Threshold=="70%") %>% 
                                  na.omit() %>% 
                                  nrow(.))))+
  geom_label(aes(4, 2500,
                 label = paste0("N=", accumulation_curves %>% 
                                  dplyr::filter(Threshold=="80%") %>% 
                                  na.omit() %>% 
                                  nrow(.))))+
  geom_label(aes(5, 2500,
                 label = paste0("N=", accumulation_curves %>% 
                                  dplyr::filter(Threshold=="90%") %>% 
                                  na.omit() %>% 
                                  nrow(.))))+
  geom_label(aes(6, 2500,
                 label = paste0("N=", accumulation_curves %>% 
                                  dplyr::filter(Threshold=="100%") %>% 
                                  na.omit() %>% 
                                  nrow(.))))
  

#ggsave("Richness_estimation_paper/Figures/boxplots_of_samples_needed.png", 
#       width=6.2, height=5.5, units="in")


## summarize the information for the paper
accumulation_curves %>%
  dplyr::group_by(Threshold) %>%
  summarize(mean=mean(value, na.rm=TRUE))


## Plot the relationship between actual cogger richness in a grid cell
## and the number of samples necessary to meet
## the threshold
true_richness %>%
  right_join(., accumulation_curves, by="grid_id") %>%
  ggplot(., aes(x=value, y=Species_richness_hal, color=Threshold))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  scale_x_log10()+
  theme_classic()+
  xlab("# of FrogID submissions")+
  ylab("Cogger species richness")+
  theme(axis.text=element_text(color="black"))+
  scale_color_discrete(breaks=c("100%","90%","80%", "70%", "60%", "50%"))
  

#ggsave("Richness_estimation_paper/Figures/cogger_richness_vs_samples_needed.png", 
#       width=6.2, height=5.5, units="in")


## Now investigate the influence of 'remoteness'
## on the results
## using a 2015 map of global accessibility
## first plot the relationship between number of submissions
## in a grid cell and the remoteness of the grid cell
remoteness_test <- true_richness %>%
  replace_na(list(Number_of_samples=0)) %>%
  dplyr::filter(Number_of_samples != 0)

ggplot(remoteness_test, aes(y=Number_of_samples, x=Remoteness))+
  geom_point()+
  scale_y_log10()+
  scale_x_log10()+
  geom_smooth(method="lm")+
  theme_classic()+
  ylab("# of FrogID submissions")+
  xlab("Remoteness")+
  theme(axis.text=element_text(color="black"))

#ggsave("Richness_estimation_paper/Figures/Remoteness_vs_number_of_submissions.png", 
#       width=6.2, height=5.5, units="in")

summary(lm(log(Number_of_samples) ~ log(Remoteness), data=remoteness_test))


### Now lets see if remoteness is an explainer of
### whether a cell has met its threshold
### function to apply to each threshold size
apply_glm <- function(threshold_size){
  
  dat <- accumulation_curves %>%
    replace_na(list(value=0)) %>%
    mutate(value=case_when(
      value==0 ~ 0,
      value>0 ~ 1
    )) %>%
    right_join(., true_richness, by="grid_id") %>%
    dplyr::select(value, Threshold, grid_id) %>%
    left_join(., grid_lookup, by="grid_id") %>%
    replace_na(list(Threshold=threshold_size)) %>%
    replace_na(list(value=0)) %>%
    dplyr::filter(Threshold==threshold_size)
  
  mod <- glm(value ~ log(Remoteness), 
              data=dat, family=binomial())      
  
  return(mod)
}

glm_100 <- apply_glm("100%")
glm_90 <- apply_glm("90%")
glm_80 <- apply_glm("80%")
glm_70 <- apply_glm("70%")
glm_60 <- apply_glm("60%")
glm_50 <- apply_glm("50%")



summary(glm_100)
summary(glm_90)
summary(glm_80)
summary(glm_70)
summary(glm_60)
summary(glm_50)

temp1 <- tidy(glm_100) %>%
  mutate(Threshold="100%")
temp2 <- tidy(glm_90) %>%
  mutate(Threshold="90%")
temp3 <- tidy(glm_80) %>%
  mutate(Threshold="80%")
temp4 <- tidy(glm_70) %>%
  mutate(Threshold="70%")
temp5 <- tidy(glm_60) %>%
  mutate(Threshold="60%")
temp6 <- tidy(glm_50) %>%
  mutate(Threshold="50%")

summary_df <- bind_rows(temp1, temp2, temp3,
                        temp4, temp5, temp6)

#write_csv(summary_df, "Data/glm_remoteness_mod_results.csv")

# Make a plot of this to include in the paper
return_dat_for_plotting <- function(threshold_size){
  
  dat <- accumulation_curves %>%
    replace_na(list(value=0)) %>%
    mutate(value=case_when(
      value==0 ~ 0,
      value>0 ~ 1
    )) %>%
    right_join(., true_richness, by="grid_id") %>%
    dplyr::select(value, Threshold, grid_id) %>%
    left_join(., grid_lookup, by="grid_id") %>%
    replace_na(list(Threshold=threshold_size)) %>%
    replace_na(list(value=0)) %>%
    dplyr::filter(Threshold==threshold_size)
  
  return(dat)
}


dat_100 <- return_dat_for_plotting("100%")
dat_90 <- return_dat_for_plotting("90%")
dat_80 <- return_dat_for_plotting("80%")
dat_70 <- return_dat_for_plotting("70%")
dat_60 <- return_dat_for_plotting("60%")
dat_50 <- return_dat_for_plotting("50%")


bind_rows(dat_100, dat_90, dat_80, 
          dat_70, dat_60, dat_50) %>%
  mutate(value=case_when(
    value==1 ~ "Yes",
    value==0 ~ "No"
  )) %>%
  transform(., Threshold=factor(Threshold, levels=c("100%", "90%", "80%", "70%", "60%", "50%"))) %>%
  ggplot(., aes(x=Threshold, y=Remoteness, fill=value))+
  geom_violin(position=position_dodge()) +
  geom_boxplot(width=0.1, color="black", position = position_dodge(width =0.9))+
  scale_y_log10()+
  ylab("Remoteness")+
  xlab("Threshold")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  scale_x_discrete(limits=c("50%", "60%", "70%", "80%", "90%", "100%"),
                   labels=c("50%", "60%", "70%", "80%", "90%", "100%"))+
  labs(fill="Threshold met")


#ggsave("Richness_estimation_paper/Figures/thresholds_met_vs_remoteness.png", 
#       width=5.8, height=4.2, units="in")


##############################################
##############################################
####### now lets try to explain the 'difference' between the two datasets
####### the relative difference as plotted above
##############################################
difference_test <- true_richness %>%
  mutate(diff=Species_richness_hal-Species_richness_frogid) %>%
  mutate(relative_diff=diff/Species_richness_hal)

## Going to model the relative difference between the
## two datasets
## This will attempt to explain what drives the difference
## between FrogID and Cogger
## The two explanatory variables are ecoregion and remoteness
## remoteness has already been explored above a little bit

## check distribution of response
hist(difference_test$relative_diff)

## Data don't look super normal, but we'll try it and see
## what the model looks like
## Do a linear model with multiple predictors
## First I'm going to flip the sign of each
## element in the vector so that the smaller numbers are the 'worst'
## and the highest numbers are the 'best'
## this will ease inrepretation I think
difference_test$relative_diff2 <- (difference_test$relative_diff*-1)+1

## Now check the distribution again
hist(difference_test$relative_diff2)

## Now we have a zero-inflated looking model
## lets try a gam with a tweedie distribution
## this is better than negbin, and is flexible accounting for the many
## zeros in the data.
## this distribution probably works the best
mod1 <- mgcv::gam(relative_diff2 ~ log(Remoteness), data=difference_test, family=tw())

summary(mod1)

## now add the interaction term to the model
mod2 <- mgcv::gam(relative_diff2 ~ Ecoregion + log(Remoteness) + Ecoregion:log(Remoteness), data=difference_test, family=tw())

summary(mod2)


mod_results <- data.frame(mod2$coefficients) %>%
  rownames_to_column(var="Term") %>%
  mutate(se=summary(mod2)$se) %>%
  mutate(p=summary(mod2)$p.pv) %>%
  mutate(lower=confint(mod2)[[5]]) %>%
  mutate(upper=confint(mod2)[[6]]) %>%
  mutate(Term=case_when(
    Term == "(Intercept)" ~ "Intercept",
    Term == "EcoregionMediterranean Forests" ~ "Med. Forests", 
    Term == "EcoregionTemperate Broadleaf & Mixed Forests" ~ "Temp. Broad. & Mixed For.",
    Term == "EcoregionTemperate Grasslands" ~ "Temp. Grass.",
    Term == "EcoregionTropical & Subtropical Grasslands" ~ "Trop. & Subtrop. Grass.", 
    Term == "EcoregionTropical & Subtropical Moist Broadleaf Forests" ~ "Trop. & Subtrop. Moist Broad. For.",
    Term == "EcoregionMontane Grasslands & Shrublands" ~ "Montane Grass. & Shrub.",
    Term == "log(Remoteness)" ~ "Remoteness",
    Term == "EcoregionMediterranean Forests:log(Remoteness)" ~ "Med. For.*Remoteness",
    Term == "EcoregionTemperate Broadleaf & Mixed Forests:log(Remoteness)" ~ "Temp. Broad. & Mixed For.*Remoteness",
    Term == "EcoregionTemperate Grasslands:log(Remoteness)" ~ "Temp. Grass.*Remoteness",
    Term == "EcoregionTropical & Subtropical Grasslands:log(Remoteness)" ~ "Trop. & Subtrop. Grass.*Remoteness",
    Term == "EcoregionTropical & Subtropical Moist Broadleaf Forests:log(Remoteness)" ~ "Trop. & Subtrop. Moist Broad. For.*Remoteness",
    Term == "EcoregionMontane Grasslands & Shrublands:log(Remoteness)" ~ "Montane Grass. & Shrub.*Remoteness"
  )) %>%
  mutate(estimate=mod2.coefficients*-1) %>%
  mutate(Significant = case_when(
    p <= 0.05 ~ "Yes",
    p > 0.05 ~ "No"
  )) %>%
  arrange(desc(estimate)) %>%
  mutate(upper=upper*-1) %>%
  mutate(lower=lower*-1)

ggplot(mod_results, aes(x=fct_inorder(Term), y=estimate, shape=Significant))+
  geom_hline(yintercept=0, color="red")+
  geom_point()+
  coord_flip()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  theme_classic()+
  ylab("Parameter estimate")+
  xlab("Term")+
  theme(axis.text=element_text(color="black"))
  
  
#ggsave("Richness_estimation_paper/Figures/region_remote_model_param_estimates.png", 
#       width=7.5, height=4.2, units="in")

# Now plot what the data actually looks like underlying this semi-complicated modelling approach!

ggplot(na.omit(difference_test %>% dplyr::select(-Number_of_samples)), aes(x=Remoteness, y=relative_diff, color=Ecoregion))+
  geom_point()+
  scale_x_log10()+
  theme_classic()+
  geom_smooth(method="lm")+
  xlab("Remoteness")+
  ylab("Relative difference between datasets")+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~Ecoregion)+
  theme(legend.position="none")

#ggsave("Richness_estimation_paper/Figures/region_remote_faceted_data_plot.png", 
#       width=8.5, height=5.2, units="in")




















