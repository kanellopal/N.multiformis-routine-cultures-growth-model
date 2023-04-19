#### Package Load ----
library(readxl)
library(ggplot2)
library(dplyr)
library(sicegar)
library(gridExtra)

#### Initial data import and processing ----
data_plastic <- read_excel("N.multiformis routine cultures and concentrated cultures growth in plastic.xlsx", sheet = 2, col_names = TRUE)
data_plastic <- data_plastic[,-7]
names(data_plastic) <- c("Time", "Biological_Replicate", "Generation", "Nitrite", "Mean", "St.Dev")
View(data_plastic)
data_plastic_T0_T7 <- filter(data_plastic, Time <= 7)
View(data_plastic_T0_T7)

#### ggplot of MG3 growth Days 0 - 7 and Days 0 - 9 ----
data_plastic_T0_T7 <- data_plastic_T0_T7[,c(1:3,5:6)]
data_plastic <- data_plastic[,c(1:3,5:6)]
ggplot_with_smooth_line_T0_T7 <- ggplot(data_plastic_T0_T7, aes(x = Time, y = Mean)) + geom_point() + theme_light() + labs(title = "MG3 Routine Cultures Growth Curve", subtitle = "Days 0 - 7", x = "Time (Days)", y = "[Nitrite] (uM)")
ggplot_with_smooth_line_T0_T7 <- ggplot_with_smooth_line_T0_T7 + geom_point(aes(colour = Generation)) + geom_errorbar(aes(ymin = Mean - St.Dev, ymax = Mean + St.Dev))
print(ggplot_with_smooth_line_T0_T7)
ggplot_with_smooth_line <- ggplot(data_plastic, aes(x = Time, y = Mean)) + geom_point() + theme_light() + labs(title = "MG3 Routine Cultures Growth Curve", subtitle = "Days 0 - 9", x = "Time (Days)", y = "[Nitrite] (uM)")
ggplot_with_smooth_line <- ggplot_with_smooth_line + geom_point(aes(colour = Generation)) + geom_errorbar(aes(ymin = Mean - St.Dev, ymax = Mean + St.Dev))
print(ggplot_with_smooth_line)

#### Preparation of data for model fit ----
data_plastic_only_lines_with_mean_sd <- filter(data_plastic, !is.na(Mean))
data_plastic_T0_T7_only_lines_with_mean_sd <- filter(data_plastic_T0_T7, !is.na(Mean))
data_plastic_Time_Mean <- data_plastic_only_lines_with_mean_sd[,c(1,4)]
data_plastic_T0_T7_Time_Mean <- data_plastic_T0_T7_only_lines_with_mean_sd[,c(1,4)]
names(data_plastic_Time_Mean) <- c("time", "intensity")
names(data_plastic_T0_T7_Time_Mean) <- c("time", "intensity")
View(data_plastic_only_lines_with_mean_sd)
View(data_plastic_T0_T7_only_lines_with_mean_sd)

#### Sigmoidal curve model fit for Days 0 - 7 and Days 0 - 9 ----
fitObj_sm_T0_T7 <- fitAndCategorize(dataInput = data_plastic_T0_T7_Time_Mean, threshold_t0_max_int = 2)
growth_sigmoid_model_plot_T0_T7 <- figureModelCurves(dataInput = fitObj_sm_T0_T7$normalizedInput, sigmoidalFitVector = fitObj_sm_T0_T7$sigmoidalModel)
growth_sigmoid_model_plot_T0_T7 <- growth_sigmoid_model_plot_T0_T7 + labs(title = "Nitrosospira multiformis Routine Cultures Growth Model", subtitle = "Days 0 - 7", x = "Time (Days)", y = "[Nitrite] (uM)")
print(growth_sigmoid_model_plot_T0_T7)
fitObj_sm <- fitAndCategorize(dataInput = data_plastic_Time_Mean, threshold_t0_max_int = 2)
growth_sigmoid_model_plot <- figureModelCurves(dataInput = fitObj_sm$normalizedInput, sigmoidalFitVector = fitObj_sm$sigmoidalModel)
growth_sigmoid_model_plot <- growth_sigmoid_model_plot + labs(title = "Nitrosospira multiformis Routine Cultures Growth Model", subtitle = "Days 0 - 9", x = "Time (Days)", y = "[Nitrite] (uM)")
print(growth_sigmoid_model_plot)

#### Export the Sigmoidal growth curves for Days 0 - 7 and Days 0 - 9 in .pdf files ----
cairo_pdf("MG3 growth curves.pdf", height = 6, width = 12, onefile = TRUE)
grid.arrange(ggplot_with_smooth_line_T0_T7, ggplot_with_smooth_line, ncol = 2)
dev.off()

cairo_pdf("MG3 sigmoidal growth curves.pdf", height = 6, width = 6, onefile = TRUE)
print(growth_sigmoid_model_plot_T0_T7)
print(growth_sigmoid_model_plot)
dev.off()

cairo_pdf("MG3 sigmoidal growth curves (2).pdf", height = 6, width = 12, onefile = TRUE)
grid.arrange(growth_sigmoid_model_plot_T0_T7, growth_sigmoid_model_plot, ncol = 2)
dev.off()

cairo_pdf("MG3 routine cultures growth curves (merged).pdf", height = 12, width = 12, onefile = TRUE)
par(mfrow = c(2, 2))
grid.arrange(ggplot_with_smooth_line_T0_T7, ggplot_with_smooth_line, growth_sigmoid_model_plot_T0_T7, growth_sigmoid_model_plot, ncol = 2)
dev.off()