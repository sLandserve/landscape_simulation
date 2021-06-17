# load libraries
library(tidyverse)
library(broom)
library(jtools)

# load simulation results
f <- list.files("results/benefit_replicates", full.names = TRUE)

# function to read simulation results and manipulate data
# here we standardise ee_threah and es_thresh to be the proportion of the diagonal length of the landscape
# we also log(x + 1) transform the benefit values so the regressions relate to approximately proportional changes in benefit
# since the actual benefit values are arbitrary
read_rda <- function(x) {
	load(x)
	res <- out %>%
				 mutate_at(.vars = c("ee_thresh", "es_thresh"), function(x) (x / sqrt(65^2 + 65^2))) %>%
				 mutate(benefit = log(benefit + 1), alpha = relevel(as.factor(alpha), ref = "1") %>% recode_factor("1" = "Linear", "0.3" = "SAR"),
				 rival = relevel(as.factor(rival), ref = "FALSE") %>% recode_factor("FALSE" = "Non_rival", "TRUE" = "Rival")) %>%
				 select(benefit, p_supply, p_demand, f_supply, f_demand, inter, ee_thresh, es_thresh, alpha, beta, gamma, rival)
  rm(out)
	invisible(gc(verbose = FALSE))
	return(res)
}

# compile and fit linear models (using a loop to avoid memory issues)
FirstDone <- FALSE
for (i in 1:length(f)) {
	# load data
	options(warn = 2) # so as to catch warnings as well as errors
	Data <- try(read_rda(f[[i]]), silent = TRUE)
	print(class(Data))
	options(warn = 0)

	# fit linear models
	if (class(Data)[1] != "try-error") {
		# models with landscape structure main effects only
		LandMain <- coef(lm(benefit ~ p_supply + p_demand + f_supply + f_demand + inter, data = Data))

		# models with landscape structure main effects and ecosystem service trait main effects, plus interaction effects between landscape structure and ecosystem service traits
		LandMainES <- coef(lm(benefit ~ p_supply + p_demand + f_supply + f_demand + inter + ee_thresh + es_thresh + alpha + beta + gamma + rival +
										p_supply:ee_thresh + p_demand:ee_thresh + f_supply:ee_thresh + f_demand:ee_thresh + inter:ee_thresh +
										p_supply:es_thresh + p_demand:es_thresh + f_supply:es_thresh + f_demand:es_thresh + inter:es_thresh +
										p_supply:alpha + p_demand:alpha + f_supply:alpha + f_demand:alpha + inter:alpha +
										p_supply:beta + p_demand:beta + f_supply:beta + f_demand:beta + inter:beta +
										p_supply:gamma + p_demand:gamma + f_supply:gamma + f_demand:gamma + inter:gamma +
										p_supply:rival + p_demand:rival + f_supply:rival + f_demand:rival + inter:rival, data = Data))

		# compile coefficient estimates
		if (FirstDone == FALSE) {
			LandMain_Coefs <- LandMain
			LandMainES_Coefs <- LandMainES
			FirstDone <- TRUE
		} else {
			LandMain_Coefs <- rbind(LandMain_Coefs, LandMain)
			LandMainES_Coefs <- rbind(LandMainES_Coefs, LandMainES)
		}
	}

	rm(Data)
	gc()
}

# get coefficients and save
LandMain_Coefs <- as_tibble(LandMain_Coefs)
LandMainES_Coefs <- as_tibble(LandMainES_Coefs)
saveRDS(LandMain_Coefs, file = "results/landmain_coefs.rds")
saveRDS(LandMainES_Coefs, file = "results/landmaines_coefs.rds")

# plot results

# load coefficient estimates if necessary
LandMain_Coefs <- readRDS("results/landmain_coefs.rds")
LandMainES_Coefs <- readRDS("results/landmaines_coefs.rds")

# Frag Supply Plots

CoefsPlotMain <- LandMain_Coefs %>% dplyr::select(f_supply, f_demand, inter)
CoefsPlotInter_FSupply <- LandMainES_Coefs %>% mutate(LowEE = `f_supply:ee_thresh`)

CoefsPlotInter_FSupply <- LandMainES_Coefs %>% mutate(Low_EE = f_supply + 0.1 * `f_supply:ee_thresh`, High_EE = f_supply + 0.5 * `f_supply:ee_thresh`,
														Low_Linear = f_supply + `f_supply:alphaSAR`, High_Linear = f_supply,
														Low_ConBen = f_supply - 0.1 * `f_supply:beta`, High_ConBen = f_supply + 0.1 * `f_supply:beta`,
														Low_Rival = f_supply, High_Rival = f_supply + `f_supply:rivalRival`,
														Low_Excl = f_supply + 0.5 * `f_supply:es_thresh`, High_Excl = f_supply + 0.1 * `f_supply:es_thresh`,
														Low_Subs = f_supply + 0.5 * `f_supply:gamma`, High_Subs = f_supply + 0 * `f_supply:gamma`) %>%
														dplyr::select(Low_EE, High_EE, Low_Linear, High_Linear, Low_ConBen, High_ConBen, Low_Rival, High_Rival, Low_Excl, High_Excl, Low_Subs, High_Subs) %>%
														gather(Label, Coef, Low_EE, High_EE, Low_Linear, High_Linear, Low_ConBen, High_ConBen, Low_Rival, High_Rival, Low_Excl, High_Excl, Low_Subs, High_Subs) %>%
														mutate(Value = Label, Variable = Label) %>%
														mutate_at("Value", ~str_replace_all(., c("Low.*" = "Low", "High.*" = "High")))

CoefsPlotInter_FSupply[which(grepl("EE", CoefsPlotInter_FSupply$Variable)), "Variable"] <- "Connectivity spatial scale"
CoefsPlotInter_FSupply[which(grepl("Linear", CoefsPlotInter_FSupply$Variable)), "Variable"] <- "Linearity of area effect"
CoefsPlotInter_FSupply[which(grepl("ConBen", CoefsPlotInter_FSupply$Variable)), "Variable"] <- "Connectivity benefit"
CoefsPlotInter_FSupply[which(grepl("Rival", CoefsPlotInter_FSupply$Variable)), "Variable"] <- "Rivalness"
CoefsPlotInter_FSupply[which(grepl("Excl", CoefsPlotInter_FSupply$Variable)), "Variable"] <- "Spatial excludability"
CoefsPlotInter_FSupply[which(grepl("Subs", CoefsPlotInter_FSupply$Variable)), "Variable"] <- "Substitutability"

CoefsPlotInter_FSupply$Value <- factor(CoefsPlotInter_FSupply$Value, levels(factor(CoefsPlotInter_FSupply$Value))[c(2,1)])
CoefsPlotInter_FSupply$Variable <- factor(CoefsPlotInter_FSupply$Variable, levels(factor(CoefsPlotInter_FSupply$Variable))[c(3,2,1,4,5,6)])

ggplot(CoefsPlotInter_FSupply, aes(x = Variable, y = Coef, fill = Value)) + geom_boxplot() +
ggtitle("Fragmentation of Supply") + xlab("") + ylab("Effect size") + geom_hline(yintercept = 0, linetype = "dashed") + coord_flip() +
theme(axis.text.x = element_text(size = 14, angle = 0, hjust = 0, vjust = 0, face = "plain"),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1, vjust = 0.5, face = "plain"),
        axis.title.x = element_text(size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
				plot.title = element_text(size = 20, hjust = 0.5), legend.text = element_text(size = 14),
				legend.title = element_text(size = 14))

# Frag Demand Plots

CoefsPlotMain <- LandMain_Coefs %>% dplyr::select(f_supply, f_demand, inter)
CoefsPlotInter_FDemand <- LandMainES_Coefs %>% mutate(LowEE = `f_demand:ee_thresh`)

CoefsPlotInter_FDemand <- LandMainES_Coefs %>% mutate(Low_EE = f_demand + 0.1 * `f_demand:ee_thresh`, High_EE = f_demand + 0.5 * `f_demand:ee_thresh`,
														Low_Linear = f_demand + `f_demand:alphaSAR`, High_Linear = f_demand,
														Low_ConBen = f_demand - 0.1 * `f_demand:beta`, High_ConBen = f_demand + 0.1 * `f_demand:beta`,
														Low_Rival = f_demand, High_Rival = f_demand + `f_demand:rivalRival`,
														Low_Excl = f_demand + 0.5 * `f_demand:es_thresh`, High_Excl = f_demand + 0.1 * `f_demand:es_thresh`,
														Low_Subs = f_demand + 0.5 * `f_demand:gamma`, High_Subs = f_demand + 0 * `f_demand:gamma`) %>%
														dplyr::select(Low_EE, High_EE, Low_Linear, High_Linear, Low_ConBen, High_ConBen, Low_Rival, High_Rival, Low_Excl, High_Excl, Low_Subs, High_Subs) %>%
														gather(Label, Coef, Low_EE, High_EE, Low_Linear, High_Linear, Low_ConBen, High_ConBen, Low_Rival, High_Rival, Low_Excl, High_Excl, Low_Subs, High_Subs) %>%
														mutate(Value = Label, Variable = Label) %>%
														mutate_at("Value", ~str_replace_all(., c("Low.*" = "Low", "High.*" = "High")))

CoefsPlotInter_FDemand[which(grepl("EE", CoefsPlotInter_FDemand$Variable)), "Variable"] <- "Connectivity spatial scale"
CoefsPlotInter_FDemand[which(grepl("Linear", CoefsPlotInter_FDemand$Variable)), "Variable"] <- "Linearity of area effect"
CoefsPlotInter_FDemand[which(grepl("ConBen", CoefsPlotInter_FDemand$Variable)), "Variable"] <- "Connectivity benefit"
CoefsPlotInter_FDemand[which(grepl("Rival", CoefsPlotInter_FDemand$Variable)), "Variable"] <- "Rivalness"
CoefsPlotInter_FDemand[which(grepl("Excl", CoefsPlotInter_FDemand$Variable)), "Variable"] <- "Spatial excludability"
CoefsPlotInter_FDemand[which(grepl("Subs", CoefsPlotInter_FDemand$Variable)), "Variable"] <- "Substitutability"

CoefsPlotInter_FDemand$Value <- factor(CoefsPlotInter_FDemand$Value, levels(factor(CoefsPlotInter_FDemand$Value))[c(2,1)])
CoefsPlotInter_FDemand$Variable <- factor(CoefsPlotInter_FDemand$Variable, levels(factor(CoefsPlotInter_FDemand$Variable))[c(3,2,1,4,5,6)])

ggplot(CoefsPlotInter_FDemand, aes(x = Variable, y = Coef, fill = Value)) + geom_boxplot() +
ggtitle("Fragmentation of Demand") + xlab("") + ylab("Effect size") + geom_hline(yintercept = 0, linetype = "dashed") + coord_flip() +
theme(axis.text.x = element_text(size = 14, angle = 0, hjust = 0, vjust = 0, face = "plain"),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1, vjust = 0.5, face = "plain"),
        axis.title.x = element_text(size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
				plot.title = element_text(size = 20, hjust = 0.5), legend.text = element_text(size = 14),
				legend.title = element_text(size = 14))

# Interspersion Plots

CoefsPlotMain <- LandMain_Coefs %>% dplyr::select(f_supply, f_demand, inter)
CoefsPlotInter_Inter <- LandMainES_Coefs %>% mutate(LowEE = `inter:ee_thresh`)

CoefsPlotInter_Inter <- LandMainES_Coefs %>% mutate(Low_EE = inter + 0.1 * `inter:ee_thresh`, High_EE = inter + 0.5 * `inter:ee_thresh`,
														Low_Linear = inter + `inter:alphaSAR`, High_Linear = inter,
														Low_ConBen = inter - 0.1 * `inter:beta`, High_ConBen = inter + 0.1 * `inter:beta`,
														Low_Rival = inter, High_Rival = inter + `inter:rivalRival`,
														Low_Excl = inter + 0.5 * `inter:es_thresh`, High_Excl = inter + 0.1 * `inter:es_thresh`,
														Low_Subs = inter + 0.5 * `inter:gamma`, High_Subs = inter + 0 * `inter:gamma`) %>%
														dplyr::select(Low_EE, High_EE, Low_Linear, High_Linear, Low_ConBen, High_ConBen, Low_Rival, High_Rival, Low_Excl, High_Excl, Low_Subs, High_Subs) %>%
														gather(Label, Coef, Low_EE, High_EE, Low_Linear, High_Linear, Low_ConBen, High_ConBen, Low_Rival, High_Rival, Low_Excl, High_Excl, Low_Subs, High_Subs) %>%
														mutate(Value = Label, Variable = Label) %>%
														mutate_at("Value", ~str_replace_all(., c("Low.*" = "Low", "High.*" = "High")))

CoefsPlotInter_Inter[which(grepl("EE", CoefsPlotInter_Inter$Variable)), "Variable"] <- "Connectivity spatial scale"
CoefsPlotInter_Inter[which(grepl("Linear", CoefsPlotInter_Inter$Variable)), "Variable"] <- "Linearity of area effect"
CoefsPlotInter_Inter[which(grepl("ConBen", CoefsPlotInter_Inter$Variable)), "Variable"] <- "Connectivity benefit"
CoefsPlotInter_Inter[which(grepl("Rival", CoefsPlotInter_Inter$Variable)), "Variable"] <- "Rivalness"
CoefsPlotInter_Inter[which(grepl("Excl", CoefsPlotInter_Inter$Variable)), "Variable"] <- "Spatial excludability"
CoefsPlotInter_Inter[which(grepl("Subs", CoefsPlotInter_Inter$Variable)), "Variable"] <- "Substitutability"

CoefsPlotInter_Inter$Value <- factor(CoefsPlotInter_Inter$Value, levels(factor(CoefsPlotInter_Inter$Value))[c(2,1)])
CoefsPlotInter_Inter$Variable <- factor(CoefsPlotInter_Inter$Variable, levels(factor(CoefsPlotInter_Inter$Variable))[c(3,2,1,4,5,6)])

ggplot(CoefsPlotInter_Inter, aes(x = Variable, y = Coef, fill = Value)) + geom_boxplot() +
ggtitle("Supply-Demand Interspersion") + xlab("") + ylab("Effect size") + geom_hline(yintercept = 0, linetype = "dashed") + coord_flip() +
theme(axis.text.x = element_text(size = 14, angle = 0, hjust = 0, vjust = 0, face = "plain"),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1, vjust = 0.5, face = "plain"),
        axis.title.x = element_text(size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
				plot.title = element_text(size = 20, hjust = 0.5), legend.text = element_text(size = 14),
				legend.title = element_text(size = 14))
