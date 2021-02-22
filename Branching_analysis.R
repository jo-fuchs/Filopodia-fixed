####Desription######################################################################################################
### Neuron filopodia analysis part 2
###
###   Goals:
###     - primary summary statistics for each neuron (Numbers and lengths of all categories)
###     - secondary statistic for each neuron (Filopodia / length, Branches / length, Mean lengths, Total lengths, Max length, Axon/Dendrite Ratio)
###     - Create summary Figures
###
###   requires data created through: ImageJ plugin NeuronJ - tracing measurements exported as .txt
###
###   Version 0.1 (03.02.2021)
###   Joachim Fuchs
###
##################################################################################################################


#### 1. Load packages, source functions ##########################################################################
library(tidyverse)
library(ggbeeswarm)
library(ggsci)
source("SummarizeNeurons.R")


####  2. Analysis  ################################################################################################


# define Directory of Line profiles
folderdir <- dirname(file.choose(new = FALSE))

## Run functions
SummarizeNeurons(folderdir)



## add identifiers, set order of factors
# load data
resultsdir <- file.path(folderdir, "Results")
Axon_results <- read.table(file.path(resultsdir, "Axon_results.txt"), sep = "\t", header = TRUE)
Dendrite_results <- read.table(file.path(resultsdir, "Neurite_results.txt"), sep = "\t", header = TRUE)
Misclass <- read.table(file.path(resultsdir, "Misclassified.txt"), sep = "\t", header = TRUE)

## merge with data from other folders if required
Total <- merge(Axon_results, Dendrite_results) %>% 
  extract(Image, into = c("Image", "Cell"), regex = "(.+).csv_(.+)")


# unblinding (if necessary)
library(readxl)
Blinding <- read_excel(file.path(folderdir, "Groups.xlsx"))

Total <- left_join(Total, Blinding, by = c("Image" = "Name"))

# set order
 Total$Genotype<-factor(Total$Genotype, levels=c("WT", "KO"))
  


#### 3. Plots ######################################################################################################


plot_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1.15)),
    # axis.text.x = element_blank(), axis.ticks.y = element_line(color = "black"), #
    strip.background = element_rect(fill = "grey90", color = NA), # Facet header color
    plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold")
  )



# total axon length
axon_plot <- ggplot(Total, aes(x = Genotype, y = total_axon, col = Genotype)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)

  # Axes and Title
  ggtitle("Total Axon length") +
  xlab("") +
  ylab("Total axon length [�m]\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
# ggsave("Total_axon_length.jpg",
#   plot = axon_plot, device = "jpeg", path = resultsdir,
#   scale = 1, width = 15, height = 15, units = "cm"
# )


# primary axon length
axon_plot <- ggplot(Total, aes(x = Genotype, 
                               col = Genotype,
                               y = primary_axon)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  
  # Axes and Title
  ggtitle("Primary Axon length") +
  xlab("") +
  ylab("Primary axon length [µm]\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
# ggsave("Primary_axon_length.jpg",
#   plot = axon_plot, device = "jpeg", path = resultsdir,
#   scale = 1, width = 15, height = 15, units = "cm"
# )


# Axon filopodia density
axon_plot <- ggplot(Total, aes(x = Genotype, 
                               col = Genotype,
                               y = total_filopodia_by_total_length)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  
  # Axes and Title
  ggtitle("Axon filopodia density") +
  xlab("") +
  ylab("Filopodia / total axon length\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
# ggsave("Filo_density.jpg",
#   plot = axon_plot, device = "jpeg", path = resultsdir,
#   scale = 1, width = 15, height = 15, units = "cm"
# )


# Axon filopodia length (mean)
axon_plot <- ggplot(Total, aes(x = Genotype, 
                               col = Genotype,
                               y = mean_filo_length)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  
  # Axes and Title
  ggtitle("Mean filopodium length") +
  xlab("") +
  ylab("mean length") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
# ggsave("Filo_mean.jpg",
#        plot = axon_plot, device = "jpeg", path = resultsdir,
#        scale = 1, width = 15, height = 15, units = "cm"
# )


# Axon filopodia length (mean)
axon_plot <- ggplot(Total, aes(x = Genotype, 
                               col = Genotype,
                               y = max_filo_length)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  
  # Axes and Title
  ggtitle("Max filopodium length") +
  xlab("") +
  ylab("max length") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
# ggsave("Filo_max.jpg",
#        plot = axon_plot, device = "jpeg", path = resultsdir,
#        scale = 1, width = 15, height = 15, units = "cm"
# )

# Neurite length
dend_plot <- ggplot(Total, aes(x = Genotype, 
                               col = Genotype,
                               y = total_neurite)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  
  # Axes and Title
  ggtitle("Total dendrite length") +
  xlab("") +
  ylab("Total dendrite length\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
# ggsave("Dendrite_length.jpg",
#   plot = dend_plot, device = "jpeg", path = resultsdir,
#   scale = 1, width = 15, height = 15, units = "cm"
# )


# Dendrite number
dend_plot <- ggplot(Total, aes(x = Genotype, y = Number_neurites, col = Genotype)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  

  # Axes and Title
  ggtitle("Dendrites per cell") +
  xlab("") +
  ylab("Dendrites per cell\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
# ggsave("Dendrite_number.jpg",
#   plot = dend_plot, device = "jpeg", path = resultsdir,
#   scale = 1, width = 15, height = 15, units = "cm"
# )




# Total neurite length
dend_plot <- ggplot(Total, aes(x = Genotype,
                               col = Genotype,
                               y = total_length)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  
  # Axes and Title
  ggtitle("Total neurite length") +
  xlab("") +
  ylab("Total neurite length [µm]\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
# ggsave("Neurite_length.jpg",
#   plot = dend_plot, device = "jpeg", path = resultsdir,
#   scale = 1, width = 15, height = 15, units = "cm"
# )


# Axon-dendrite ratio
dend_plot <- ggplot(Total, aes(x = Genotype,
                               col = Genotype,
                               y = axon_by_neurite)) +
  geom_quasirandom(size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Treatment) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  

  # Axes and Title
  ggtitle("Axon / Neurite ratio") +
  xlab("") +
  ylab("Total axon length / Total neurite length\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
# ggsave("Axon_dendrite.jpg",
#   plot = dend_plot, device = "jpeg", path = resultsdir,
#   scale = 1, width = 15, height = 15, units = "cm"
# )
