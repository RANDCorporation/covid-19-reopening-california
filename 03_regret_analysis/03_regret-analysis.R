

#------------------------------------------------------------------------------#
# Code for the paper: Reopening California
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#


#### COVID-19 Regret Analysis ----------------------------------------------####

#------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This file produces the Regret Analysis.
# Creation Date: Jan 2021
#------------------------------------------------------------------------------#

library(dplyr)
library(purrr)
library(c19randepimod)
library(viridis)
library(patchwork)
library(GGally)
library(tidyr)
source("./03_regret_analysis/regret-functions.R")
# results path:


# Setting up paths that are not relative to the working directory.
# All paths should be defined here. Update your .Rprofile file accordingly.
# Ths is not relative to the working directory because large files belong to Onedrive, not git.
large_files_path = "./02_future_runs/outputs/"
paper_figures_path = "./03_regret_analysis/outputs/"
output_path = "./03_regret_analysis/outputs/"

results_path = "./02_future_runs/outputs/final_results_final"
full_results_path = "./02_future_runs/outputs/final_results_full"


#### Analyzing Regret of all Policies --------------------------------------####

# Running Regret Analysis for All policies:
all_experiment_model_rdm = readRDS(paste0(large_files_path,"experiment_model_rdm.rds"))
#all_policies_results_rdm = readRDS(paste0(large_files_path,"results_rdm.rds"))

all_policies_results_rdm <- list.files(path = results_path, pattern = ".rds", full.names = T) %>%
  purrr::map_dfr(readRDS)


# Check if there is an Experiment id not in the results:
missing_experiments = all_experiment_model_rdm$future_experimental_design$ExperimentID[!(unique(all_experiment_model_rdm$future_experimental_design$ExperimentID) %in% unique(all_policies_results_rdm$ExperimentID))]

missing_experiments


# Dataframe with all days:
#all_policies_results_rdm <- list.files(path = full_results_path, pattern = ".rds", full.names = T) %>%
#  purrr::map_dfr(readRDS)

# Are all times the same?
length(unique(all_policies_results_rdm$time))

# Do we have all experiments up to the final?
# Experiments we have:
length(unique(all_policies_results_rdm$ExperimentID))

# Maximum experiment:
max(all_policies_results_rdm$ExperimentID)

# Are all the experiments here?
length(unique(all_policies_results_rdm$ExperimentID)) == max(all_policies_results_rdm$ExperimentID)


# Filtering experiments in the experimental design that are in the results
# This is useful when one is looking at intermediate results:
# all_experiment_model_rdm$future_experimental_design = all_experiment_model_rdm$future_experimental_design %>%
#   filter(ExperimentID %in% all_policies_results_rdm$ExperimentID)


# IMABC calibration procedure requires us to duplicate the runs that were sampled more than one time:
# So, here I need to reconstruct the future_experimental_design ids to account for that:


new_run_ids = data.frame(ScenarioID = all_experiment_model_rdm$scenario_run_ids_sample) %>%
  left_join(all_experiment_model_rdm$scenarios %>% select(RunID, ScenarioID), by = "ScenarioID") %>%
  mutate(NewRunID = row_number())


new_experimental_design = tidyr::expand_grid(new_run_ids,
                                             LHSExperimentID = unique(all_experiment_model_rdm$future_experimental_design$LHSExperimentID),
                                             GridExperimentID = unique(all_experiment_model_rdm$future_experimental_design$GridExperimentID)
                                             )

new_experimental_design_full = new_experimental_design %>%
  dplyr::left_join(all_experiment_model_rdm$future_experimental_design, by = c("RunID", "GridExperimentID", "LHSExperimentID")) %>%
  dplyr::mutate(NewExperimentID = row_number())


# The all_policies_results_rdm should contain only one line per experiment.
new_results_full = new_experimental_design_full %>% 
  dplyr::select(ExperimentID, NewExperimentID, NewRunID) %>%
  dplyr::left_join(all_policies_results_rdm, by = "ExperimentID") %>%
  mutate(ExperimentID = NewExperimentID,
         RunID = NewRunID) %>%
  # Select only a subset of the data:
  select(time, Date, ExperimentID, CumulativeDeaths, CumulativeRealCases, YLL, days_of_interventions, CumulativePercentIncomeLoss, S)


# Update ids of the new_experimental_design_full object:
all_experiment_model_rdm$future_experimental_design = new_experimental_design_full %>%
  mutate(ExperimentID = NewExperimentID,
         RunID = NewRunID) 


# Run Regret Analysis for all policies:
regret_analysis_all_policies = run_regret_analysis(experiment_model_rdm = all_experiment_model_rdm, 
                                                   results_rdm = new_results_full, 
                                                   output_path = paste0(output_path), write_csv = T)


# Example strategy with fixed level of caution
baseline_strategy = 295

### Summary of Regret Analysis:

policies_summary = regret_analysis_all_policies$regret_summary_data %>% 
  filter(Category == "Non-Dominated, Selected" | GridExperimentID == baseline_strategy) %>%
  select(Strategy, Deaths, `Income Loss`, `Days of NPIs`, npi_type, LevelOfCaution, TransitionDate, NewLevelOfCautionMultiplier, AdaptiveCautionMidpoint, Category)


# Selecting Non-Dominated Strategies for Further Analysis:

non_dominated_strategies = regret_analysis_all_policies$regret_summary_data$GridExperimentID[!regret_analysis_all_policies$regret_summary_data$Dominated]

# Is the Baseline strategy non-dominated?
baseline_strategy %in% non_dominated_strategies

length(non_dominated_strategies)

# These are the ids of the Grid Points of Non-Dominated Strategies:
dput(as.numeric(non_dominated_strategies))

# These ids can be used to run the model just for the non-dominated policies.

# Take this vector and go run another the second batch.

# Open Tableau Workbook to look at results of the regret analysis and get plots:
# shell("C:/users/plima/onedriverand/02-PhD-RAND-Personal/012-Dissertation/2020-Paper-1/tableau/rdm-paper-figures.twb", wait = F)



#### Results Plots in R ----------------------------------------------------####


### Parallel Plots:

coordinates = c("Cases","YLL", "Deaths", "Days of NPIs", "Income Loss")


three_color_pallete1 = c("#9504B5", "#E4C94E", "#E3E1E3")
three_color_pallete2 = c("#9504B5", "#E4C94E", "#656565")

# three_color_pallete2 = c("#E5E3E8", "#F56618","#8A00F5")

regret_summary_data_wide = regret_analysis_all_policies$regret_summary_data %>%
  mutate(Category = factor(Category, levels = c("Non-Dominated, Selected", "Non-Dominated", "Dominated"), ordered = T)) %>%
  arrange(desc(Category), Deaths) %>%
  mutate(npi_type = ifelse(npi_type == "Time-Based" & NewLevelOfCautionMultiplier == 1, "Constant", npi_type))

regret_data = regret_analysis_all_policies$regret_data %>%
  mutate(Category = factor(Category, levels = c("Non-Dominated, Selected", "Non-Dominated", "Dominated"), ordered = T))


# Plot by selection status:
parallel_plot_selected = regret_summary_data_wide %>%
  parallel_plot(., coordinates = coordinates,color_variable =  "Category") +
  scale_color_manual(values=three_color_pallete1)

parallel_plot_selected 

# By Level of Caution:

parallel_plot_caution = regret_summary_data_wide %>%
  mutate(LevelOfCaution = as.factor(LevelOfCaution)) %>%
  parallel_plot(., coordinates = coordinates,color_variable =  "LevelOfCaution") +
  scale_colour_viridis(discrete = T, name = "Baseline Level of Caution", direction = -1)

parallel_plot_caution 

# By type of NPI:

parallel_plot_npi = regret_summary_data_wide %>%
  parallel_plot(., coordinates = coordinates,color_variable =  "npi_type") +
  scale_color_viridis(discrete=TRUE, name = "NPI Controller Type")

parallel_plot_npi 


## Add some annotations to the geom_plot:

baseline_strategy_name = regret_summary_data_wide$Strategy[regret_summary_data_wide$GridExperimentID == baseline_strategy]

# Deaths vs Income Regret plot - with all strategies
dispersion_plots = regret_summary_data_wide %>%
  mutate(Label = ifelse(Strategy %in% c("C-24-1", "V-6-3", baseline_strategy_name, "V-6-2"), Strategy, "")) %>%
  #mutate(Label = ifelse(Strategy %in% policies_summary$Strategy, Strategy, "")) %>%
  #mutate(Label = ifelse(Label == baseline_strategy_name, paste0("Baseline-",Label),Label)) %>%
  ggplot(mapping = aes(y = Deaths, x = `Days of NPIs`, color = Category, size = `Income Loss`)) + 
  geom_point(fill = "black") + 
  ylab('Deaths / 100k 75th Regret Percentile') + 
  xlab('NPI Days 75th Regret Percentile') +
  theme_classic() + 
  guides(color=FALSE) + 
  scale_color_manual(values=three_color_pallete1) + 
  theme(legend.position = "bottom")  +
  ggrepel::geom_label_repel(aes(label = Label),size = 3.5, point.padding = 0.5, show.legend = F, force = 2)
  
dispersion_plots



# Dispersion plot focusing on the strategies with low number of deaths
dispersion_focus = regret_summary_data_wide %>%
  mutate(Label = ifelse(Strategy %in% c("T-12-2", "T-24-2", "V-24-6", baseline_strategy_name), Strategy, "")) %>%
  #mutate(Label = ifelse(Strategy %in% policies_summary$Strategy, Strategy, "")) %>%
  mutate(Label = ifelse(Label == baseline_strategy_name, paste0("Baseline-",Label),Label)) %>%
  ggplot(mapping = aes(y = Deaths, x = `Days of NPIs`, color = Category, shape = npi_type)) + 
  geom_point(fill = "black", size = 4) + 
  ylab('Deaths / 100k 75th Regret Percentile') + 
  xlab('NPI Days 75th Regret Percentile') +
  theme_classic() + 
  guides(color=FALSE) +
  labs(shape = "Strategy Type") + 
  scale_color_manual(values=three_color_pallete2) + 
  theme(legend.position = "bottom") + 
  ggrepel::geom_label_repel(aes(label = Label),size = 3.5, point.padding = 1, show.legend = F, force = 4) + 
  ylim(0,30) + 
  xlim(150,250)


dispersion_focus

ggsave(filename = paste0(output_path, "dispersion_focus.png"), plot = dispersion_focus, device = "png", width = 7, height = 5, units = "in", scale = 1.2)


death_regret_plot = regret_data  %>%
  filter(Category == "Non-Dominated, Selected" | Strategy == "C-6-1" | (Category == "Non-Dominated" & rank_order <= 65) ) %>%
  ggplot(., aes(y = CumulativeDeathsRegret,x = reorder(Strategy,CumulativeDeathsRegret), fill = Category, group = Strategy)) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual(values=three_color_pallete1) + 
  ylab("Deaths / 100 K Regret") + 
  xlab("Strategy") + 
  guides(fill=FALSE) + 
  #facet_wrap(facets = "vaccination_strategy_name", scales = "free_x") + 
  theme(strip.background = element_blank()) 

death_regret_plot


# Patch Plot for the PNAS paper:
patch_plot1 = ((death_regret_plot / parallel_plot_caution)) | (parallel_plot_selected /  dispersion_focus)

patch_plot1 = patch_plot1 + plot_annotation(tag_levels = 'A') + 
  #plot_layout(guides = 'collect') +
  theme(plot.tag = element_text(size = 8))

patch_plot1

ggsave(filename = paste0(paper_figures_path, "patch_figure_01.pdf"), plot = patch_plot1, device = "pdf", width = 7, height = 5, units = "in", scale = 2)


# Individual plots for the arxiv:
ggsave(filename = paste0(paper_figures_path, "death_regret_plot.pdf"), plot = patch_plot1, device = "pdf", width = 7, height = 3.5, units = "in", scale = 2)




ggsave(filename = paste0(output_path, "patch_figure_01.png"), plot = patch_plot1, device = "png", width = 13, height = 7, units = "in", scale = 1.2)


# Plots for presentations:

# Pareto surfaces:

patch_plot2 = (parallel_plot_caution | parallel_plot_selected) +
               plot_annotation(tag_levels = 'A') + 
               #plot_layout(guides = 'collect') +
               theme(plot.tag = element_text(size = 8))

patch_plot2
ggsave(filename = paste0(output_path, "patch_plot2.png"), plot = patch_plot2, device = "png", width = 12, height = 5, units = "in", scale = 1.2)


patch_plot2.1 = (parallel_plot_selected | dispersion_plots) +
  #plot_annotation(tag_levels = 'A') + 
  #plot_layout(guides = 'collect') +
  theme(plot.tag = element_text(size = 8))

patch_plot2.1
ggsave(filename = paste0(output_path, "patch_plot2.1.png"), plot = patch_plot2.1, device = "png", width = 12, height = 5, units = "in", scale = 1.2)





## Timeseries plot
example_timeseries_data = readRDS(file = "./02_future_runs/outputs/final_results_full/results_1_280.rds") %>%
  filter(ExperimentID <= 78)

# Running Regret Analysis for All policies:
all_experiment_model_rdm = readRDS(paste0(large_files_path,"experiment_model_rdm.rds"))

# Plots with trajectories
timeseries_plots = example_timeseries_data %>%
  left_join(all_experiment_model_rdm$future_experimental_design, by = c("ExperimentID", "RunID", "LocationID")) %>%
  left_join(regret_analysis_all_policies$regret_summary_data %>% select(GridExperimentID, Strategy), by = "GridExperimentID") %>%
  filter(Strategy %in%  c("C-6-1", "C-24-1", "C-0.5-1")) %>%
  #filter(RunID == min(RunID) & LHSExperimentID == min(LHSExperimentID)) %>%
  select(Date, PortfolioID, CumulativeDeaths, Prevalence, Strategy, ExperimentID, CumulativePercentIncomeLoss) %>%
  group_by(ExperimentID) %>%
  mutate(CumulativeDeaths = CumulativeDeaths - min(CumulativeDeaths)) %>%
  ungroup() %>%
  rename("NPI Level" = PortfolioID,
        # Susceptibles = S,
         "Additional Deaths / 100K" = CumulativeDeaths,
         "Cumulative Income Loss [%]" = CumulativePercentIncomeLoss,
         "Prevalence [%]" = Prevalence) %>%
  tidyr::pivot_longer(cols = -contains(c("Date","strategy", "ExperimentID"))) %>%
  mutate(value = ifelse(name == "Additional Deaths / 100K", value * 100000, value)) %>%
  mutate(value = ifelse(name %in% c("Cumulative Income Loss [%]", "Prevalence [%]") , value * 100, value)) %>%
  ggplot(mapping = aes(x = Date, color = Strategy, y = value, group = ExperimentID)) + 
  geom_line(size=1) + 
  facet_wrap(facets = ~name,ncol = 1, scales = "free") + 
  theme_classic() +
  theme(strip.background = element_blank()) + 
  scale_color_viridis(discrete = T) + 
  theme(legend.position = "bottom")

timeseries_plots

ggsave(filename = paste0(output_path, "time_series_plots.png"), plot = timeseries_plots, device = "png", width = 7, height = 7, units = "in", scale = 1.2)

ggsave(filename = paste0(paper_figures_path, "time_series_plot.pdf"), plot = timeseries_plots, device = "pdf", width = 3.42, height = 5, units = "in", scale = 1.2)

ggsave(filename = paste0(paper_figures_path, "time_series_plot_wider.pdf"), plot = timeseries_plots, device = "pdf", width = 6, height = 5, units = "in", scale = 1.2)

