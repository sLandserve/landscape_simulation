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
		#LandMain <- coef(lm(benefit ~ p_supply + p_demand + f_supply + f_demand + inter, data = Data))

		# models with landscape structure main and amount x amount and amount x fragmentation/interspersion interaction effects only
		LandMainInt <- coef(lm(benefit ~ p_supply + p_demand + p_supply:p_demand + f_supply + f_demand + inter + p_supply:f_supply + p_demand:f_demand + p_supply:inter + p_demand:inter, data = Data))
		gc()

		# models with landscape structure main effects and ecosystem service characteristics main effects, plus interaction effects between landscape structure and ecosystem service characteristics
		LandMainES <- coef(lm(benefit ~ p_supply + p_demand + f_supply + f_demand + inter + ee_thresh + es_thresh + alpha + beta + gamma + rival +
										p_supply:ee_thresh + p_demand:ee_thresh + f_supply:ee_thresh + f_demand:ee_thresh + inter:ee_thresh +
										p_supply:es_thresh + p_demand:es_thresh + f_supply:es_thresh + f_demand:es_thresh + inter:es_thresh +
										p_supply:alpha + p_demand:alpha + f_supply:alpha + f_demand:alpha + inter:alpha +
										p_supply:beta + p_demand:beta + f_supply:beta + f_demand:beta + inter:beta +
										p_supply:gamma + p_demand:gamma + f_supply:gamma + f_demand:gamma + inter:gamma +
										p_supply:rival + p_demand:rival + f_supply:rival + f_demand:rival + inter:rival, data = Data))

		# models with landscape structure main and interaction effects and ecosystem service characteristics main effects, plus interaction effects between landscape structure and ecosystem service characteristics
		LandMainIntES <- coef(lm(benefit ~ p_supply + p_demand + p_supply:p_demand + f_supply + f_demand + inter + p_supply:f_supply + p_demand:f_demand + p_supply:inter + p_demand:inter +
										ee_thresh + es_thresh + alpha + beta + gamma + rival +
										p_supply:ee_thresh + p_demand:ee_thresh + p_supply:p_demand:ee_thresh + f_supply:ee_thresh + f_demand:ee_thresh + inter:ee_thresh + p_supply:f_supply:ee_thresh + p_demand:f_demand:ee_thresh + p_supply:inter:ee_thresh + p_demand:inter:ee_thresh +
										p_supply:es_thresh + p_demand:es_thresh + p_supply:p_demand:es_thresh + f_supply:es_thresh + f_demand:es_thresh + inter:es_thresh + p_supply:f_supply:es_thresh + p_demand:f_demand:es_thresh + p_supply:inter:es_thresh + p_demand:inter:es_thresh +
										p_supply:alpha + p_demand:alpha + p_supply:p_demand:alpha + f_supply:alpha + f_demand:alpha + inter:alpha + p_supply:f_supply:alpha + p_demand:f_demand:alpha + p_supply:inter:alpha + p_demand:inter:alpha +
										p_supply:beta + p_demand:beta + p_supply:p_demand:beta + f_supply:beta + f_demand:beta + inter:beta + p_supply:f_supply:beta + p_demand:f_demand:beta + p_supply:inter:beta + p_demand:inter:beta +
										p_supply:gamma + p_demand:gamma + p_supply:p_demand:gamma + f_supply:gamma + f_demand:gamma + inter:gamma + p_supply:f_supply:gamma + p_demand:f_demand:gamma + p_supply:inter:gamma + p_demand:inter:gamma +
										p_supply:rival + p_demand:rival + p_supply:p_demand:rival + f_supply:rival + f_demand:rival + inter:rival + p_supply:f_supply:rival + p_demand:f_demand:rival + p_supply:inter:rival + p_demand:inter:rival, data = Data))

		# compile coefficient estimates
		if (FirstDone == FALSE) {
			#LandMain_Coefs <- LandMain
			LandMainInt_Coefs <- LandMainInt
			#LandMainES_Coefs <- LandMainES
			#LandMainIntES_Coefs <- LandMainIntES
			FirstDone <- TRUE
		} else {
			#LandMain_Coefs <- rbind(LandMain_Coefs, LandMain)
			LandMainInt_Coefs <- rbind(LandMainInt_Coefs, LandMainInt)
			#LandMainES_Coefs <- rbind(LandMainES_Coefs, LandMainES)
			#LandMainIntES_Coefs <- rbind(LandMainIntES_Coefs, LandMainIntES)
		}
	}

	rm(Data)
	gc()
}

# get coefficients and save
#LandMain_Coefs <- as_tibble(LandMain_Coefs)
LandMainInt_Coefs <- as_tibble(LandMainInt_Coefs)
LandMainES_Coefs <- as_tibble(LandMainES_Coefs)
LandMainIntES_Coefs <- as_tibble(LandMainIntES_Coefs)
saveRDS(LandMain_Coefs, file = "results/landmain_coefs.rds")
saveRDS(LandMainInt_Coefs, file = "results/landmainint_coefs.rds")
saveRDS(LandMainES_Coefs, file = "results/landmaines_coefs.rds")
saveRDS(LandMainIntES_Coefs, file = "results/landmainintes_coefs.rds")

# sumamrise coefficient estimates for paper

# load coefficient estimates if necessary
LandMainES_Coefs <- readRDS("results/landmaines_coefs.rds")

# amount of supply
TabInter_PSupply <- LandMainES_Coefs %>% mutate(main = p_supply, ee_thresh = `p_supply:ee_thresh`, alphaSAR = `p_supply:alphaSAR`, beta = `p_supply:beta`,
  												 es_thresh = `p_supply:es_thresh`, rivalRival = `p_supply:rivalRival`, gamma = `p_supply:gamma`) %>%
													 dplyr::select(main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma) %>%
													 gather(Label, Coef, main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma)
TabInter_PSupply[which(grepl("main", TabInter_PSupply$Label)), "Label"] <- "Main Supply"
TabInter_PSupply[which(grepl("ee_thresh", TabInter_PSupply$Label)), "Label"] <- "Supply-supply spatial scale"
TabInter_PSupply[which(grepl("alphaSAR", TabInter_PSupply$Label)), "Label"] <- "SAR patch size effect"
TabInter_PSupply[which(grepl("beta", TabInter_PSupply$Label)), "Label"] <- "Supply connectivity benefit"
TabInter_PSupply[which(grepl("es_thresh", TabInter_PSupply$Label)), "Label"] <- "Supply-demand spatial scale"
TabInter_PSupply[which(grepl("rivalRival", TabInter_PSupply$Label)), "Label"] <- "Rivalness"
TabInter_PSupply[which(grepl("gamma", TabInter_PSupply$Label)), "Label"] <- "Non-substitutability"
TabInter_PSupply$Label <- factor(TabInter_PSupply$Label, levels(factor(TabInter_PSupply$Label))[c(1, 6, 4, 7, 5, 3, 2)])
TabInter_PSupply <- TabInter_PSupply %>% group_by(Label) %>% summarise(mean = mean(Coef), lower = quantile(Coef, 0.025), upper = quantile(Coef, 0.975))
write.csv(TabInter_PSupply, file = "results/psupply_coefs.csv", row.names = FALSE)

# amount of demand
TabInter_PDemand <- LandMainES_Coefs %>% mutate(main = p_demand, ee_thresh = `p_demand:ee_thresh`, alphaSAR = `p_demand:alphaSAR`, beta = `p_demand:beta`,
  												 es_thresh = `p_demand:es_thresh`, rivalRival = `p_demand:rivalRival`, gamma = `p_demand:gamma`) %>%
													 dplyr::select(main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma) %>%
													 gather(Label, Coef, main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma)
TabInter_PDemand[which(grepl("main", TabInter_PDemand$Label)), "Label"] <- "Main Demand"
TabInter_PDemand[which(grepl("ee_thresh", TabInter_PDemand$Label)), "Label"] <- "Supply-supply spatial scale"
TabInter_PDemand[which(grepl("alphaSAR", TabInter_PDemand$Label)), "Label"] <- "SAR patch size effect"
TabInter_PDemand[which(grepl("beta", TabInter_PDemand$Label)), "Label"] <- "Supply connectivity benefit"
TabInter_PDemand[which(grepl("es_thresh", TabInter_PDemand$Label)), "Label"] <- "Supply-demand spatial scale"
TabInter_PDemand[which(grepl("rivalRival", TabInter_PDemand$Label)), "Label"] <- "Rivalness"
TabInter_PDemand[which(grepl("gamma", TabInter_PDemand$Label)), "Label"] <- "Non-substitutability"
TabInter_PDemand$Label <- factor(TabInter_PDemand$Label, levels(factor(TabInter_PDemand$Label))[c(1, 6, 4, 7, 5, 3, 2)])
TabInter_PDemand <- TabInter_PDemand %>% group_by(Label) %>% summarise(mean = mean(Coef), lower = quantile(Coef, 0.025), upper = quantile(Coef, 0.975))
write.csv(TabInter_PDemand, file = "results/pdemand_coefs.csv", row.names = FALSE)

# fragmentation of supply
TabInter_FSupply <- LandMainES_Coefs %>% mutate(main = f_supply, ee_thresh = `f_supply:ee_thresh`, alphaSAR = `f_supply:alphaSAR`, beta = `f_supply:beta`,
  												 es_thresh = `f_supply:es_thresh`, rivalRival = `f_supply:rivalRival`, gamma = `f_supply:gamma`) %>%
													 dplyr::select(main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma) %>%
													 gather(Label, Coef, main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma)
TabInter_FSupply[which(grepl("main", TabInter_FSupply$Label)), "Label"] <- "Main Supply"
TabInter_FSupply[which(grepl("ee_thresh", TabInter_FSupply$Label)), "Label"] <- "Supply-supply spatial scale"
TabInter_FSupply[which(grepl("alphaSAR", TabInter_FSupply$Label)), "Label"] <- "SAR patch size effect"
TabInter_FSupply[which(grepl("beta", TabInter_FSupply$Label)), "Label"] <- "Supply connectivity benefit"
TabInter_FSupply[which(grepl("es_thresh", TabInter_FSupply$Label)), "Label"] <- "Supply-demand spatial scale"
TabInter_FSupply[which(grepl("rivalRival", TabInter_FSupply$Label)), "Label"] <- "Rivalness"
TabInter_FSupply[which(grepl("gamma", TabInter_FSupply$Label)), "Label"] <- "Non-substitutability"
TabInter_FSupply$Label <- factor(TabInter_FSupply$Label, levels(factor(TabInter_FSupply$Label))[c(1, 6, 4, 7, 5, 3, 2)])
TabInter_FSupply <- TabInter_FSupply %>% group_by(Label) %>% summarise(mean = mean(Coef), lower = quantile(Coef, 0.025), upper = quantile(Coef, 0.975))
write.csv(TabInter_FSupply, file = "results/fsupply_coefs.csv", row.names = FALSE)

# fragmentation of demand
TabInter_FDemand <- LandMainES_Coefs %>% mutate(main = f_demand, ee_thresh = `f_demand:ee_thresh`, alphaSAR = `f_demand:alphaSAR`, beta = `f_demand:beta`,
  												 es_thresh = `f_demand:es_thresh`, rivalRival = `f_demand:rivalRival`, gamma = `f_demand:gamma`) %>%
													 dplyr::select(main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma) %>%
													 gather(Label, Coef, main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma)
TabInter_FDemand[which(grepl("main", TabInter_FDemand$Label)), "Label"] <- "Main Demand"
TabInter_FDemand[which(grepl("ee_thresh", TabInter_FDemand$Label)), "Label"] <- "Supply-supply spatial scale"
TabInter_FDemand[which(grepl("alphaSAR", TabInter_FDemand$Label)), "Label"] <- "SAR patch size effect"
TabInter_FDemand[which(grepl("beta", TabInter_FDemand$Label)), "Label"] <- "Supply connectivity benefit"
TabInter_FDemand[which(grepl("es_thresh", TabInter_FDemand$Label)), "Label"] <- "Supply-demand spatial scale"
TabInter_FDemand[which(grepl("rivalRival", TabInter_FDemand$Label)), "Label"] <- "Rivalness"
TabInter_FDemand[which(grepl("gamma", TabInter_FDemand$Label)), "Label"] <- "Non-substitutability"
TabInter_FDemand$Label <- factor(TabInter_FDemand$Label, levels(factor(TabInter_FDemand$Label))[c(1, 6, 4, 7, 5, 3, 2)])
TabInter_FDemand <- TabInter_FDemand %>% group_by(Label) %>% summarise(mean = mean(Coef), lower = quantile(Coef, 0.025), upper = quantile(Coef, 0.975))
write.csv(TabInter_FDemand, file = "results/fdemand_coefs.csv", row.names = FALSE)

# interspersion of supply/demand
TabInter_Inter <- LandMainES_Coefs %>% mutate(main = inter, ee_thresh = `inter:ee_thresh`, alphaSAR = `inter:alphaSAR`, beta = `inter:beta`,
  												 es_thresh = `inter:es_thresh`, rivalRival = `inter:rivalRival`, gamma = `inter:gamma`) %>%
													 dplyr::select(main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma) %>%
													 gather(Label, Coef, main, ee_thresh, alphaSAR, beta, es_thresh, rivalRival, gamma)
TabInter_Inter[which(grepl("main", TabInter_Inter$Label)), "Label"] <- "Main Interspersion"
TabInter_Inter[which(grepl("ee_thresh", TabInter_Inter$Label)), "Label"] <- "Supply-supply spatial scale"
TabInter_Inter[which(grepl("alphaSAR", TabInter_Inter$Label)), "Label"] <- "SAR patch size effect"
TabInter_Inter[which(grepl("beta", TabInter_Inter$Label)), "Label"] <- "Supply connectivity benefit"
TabInter_Inter[which(grepl("es_thresh", TabInter_Inter$Label)), "Label"] <- "Supply-demand spatial scale"
TabInter_Inter[which(grepl("rivalRival", TabInter_Inter$Label)), "Label"] <- "Rivalness"
TabInter_Inter[which(grepl("gamma", TabInter_Inter$Label)), "Label"] <- "Non-substitutability"
TabInter_Inter$Label <- factor(TabInter_Inter$Label, levels(factor(TabInter_Inter$Label))[c(1, 6, 4, 7, 5, 3, 2)])
TabInter_Inter <- TabInter_Inter %>% group_by(Label) %>% summarise(mean = mean(Coef), lower = quantile(Coef, 0.025), upper = quantile(Coef, 0.975))
write.csv(TabInter_Inter, file = "results/inter_coefs.csv", row.names = FALSE)

# OLD PLOTS - CONSIDER DELETING

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
