########################################################################
#		Apply DiD methods to labor audits data
#		Yuequan
#		240623|250729
########################################################################



# preparation
	## install R packages
	# install.packages("")

	## load R packages
	lib <- c("haven","tidyverse","plm","mediation","did","modelsummary","ggeffects")
	lapply(lib,require,character.only=TRUE)

	## clear up space
	rm(list=ls())



# read in clean data
	dt <- read_dta("Aug31 data for analysis.dta")

	dt2 <- dt |>
		filter(dmy(AssesmentDate)<ymd(20200301)) |> # before COVID19
		drop_na(mngindex13,femalepc,regularwkpc,size,factoryageln) |> # ,union # remove rows w/ missing values
		mutate(
			AssesmentDate = dmy(AssesmentDate),
			fid = as.factor(FactoryAssessedID),
			ym = floor_date(AssesmentDate, unit="month"),
			y = year(AssesmentDate),
			T2 = case_when(
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				TRUE 																									~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # staggered treatment
			T2r = case_when(
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				# Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20100101)	~ "1",
				Country=="Cambodia"		& AssesmentDate>=ymd(20140301)	~ "1",
				TRUE 																									~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # robust treatment
			T3 = case_when(
				Country=="Vietnam" 		& y>=2016	~ "1",
				Country=="Jordan" 		& y>=2016	~ "1",
				Country=="Indonesia" 	& y>=2017	~ "1",
				Country=="Haiti" 			& y>=2017	~ "1",
				Country=="Nicaragua" 	& y>=2018	~ "1",
				TRUE 														~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # staggered treatment by year
			T3r = case_when(
				Country=="Vietnam" 		& y>=2016	~ "1",
				Country=="Jordan" 		& y>=2016	~ "1",
				Country=="Indonesia" 	& y>=2017	~ "1",
				# Country=="Haiti" 			& y>=2017	~ "1",
				Country=="Nicaragua" 	& y>=2018	~ "1",
				Country=="Haiti" 			& y>=2010	~ "1",
				Country=="Cambodia"		& y>=2014	~ "1",
				TRUE 														~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # robust treatment by year
			T3r2 = case_when(
				Country %in% c("Haiti", "Cambodia") ~ NA_character_,
				Country=="Vietnam" 		& y>=2016	~ "1",
				Country=="Jordan" 		& y>=2016	~ "1",
				Country=="Indonesia" 	& y>=2017	~ "1",
				Country=="Nicaragua" 	& y>=2018	~ "1",
				TRUE 														~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")) # robust treatment by year, NA for Haiti and Cambodia
			)
	# names(dt2)
	# with(dt2, table(mngindex13, exclude=NULL))
	# pdt2 <- pdata.frame(dt2, index=c("fid","ym"))
	pdt2 <- dt2 |>
		mutate(n = row_number(), .by=c(fid, y)) |>
		mutate( # treat duplicated fid in the same year as new factories
			fid2 = if_else(n>1, paste0(FactoryAssessedID, "_", n), as.character(FactoryAssessedID)) %>% as.factor
			) |>
		pdata.frame(index=c("fid2","y","Country")) # add Country to the index to allow for clustering

	dt3 <- dt |>
		mutate(
			AssesmentDate = dmy(AssesmentDate),
			fid = as.integer(FactoryAssessedID),
			ym = format(AssesmentDate, "%Y%m") %>% as.integer, ym_r = dense_rank(ym),
			T2_ym = case_when(
				Country=="Vietnam" 			~ 18, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 23, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 25, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 31, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 37, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				)
			) |>
		filter(AssesmentDate<ymd(20200301)) |>
		drop_na(mngindex13,union,femalepc,regularwkpc,size,factoryageln)
	# str(dt3$ym); dt3 |> arrange(ym) |> distinct(ym,ym_r) |> filter(ym==201801) |> print(n=Inf) # look up ym_r for the corresponding ym
	# cannot conduct robustness analysis such as in TWFE because the method excludes units that have no untreated obs (i.e., factories in Haiti and Cambodia will be excluded)



# DiD by two-way fixed effects panel models
	dvs <- c("reportedcompl","similarCPcompl","distantCPcompl")
	mos <- c("buyer1FTindexband","RRic2010","mngindex13","union")

## main
	T <- "T3" # set to "T2" or "T3" to switch between treatments

	fss <- sapply(dvs, function(dv) formula(paste0(dv," ~ ", T)))
	fs <- sapply(dvs, function(dv) formula(paste0(dv," ~ ", T, " + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle")))
	fms <- lapply(dvs, function(dv) {
		lapply(mos, function(mo) {
			formula(paste0(dv, " ~ ", T, "*", mo, " + ", paste(mos[-which(mos==mo)], collapse=" + "), " + femalepc + regularwkpc + size + factoryageln + Cycle"))
			})
		}) |> unlist(use.names=FALSE)

	mss <- lapply(fss, function(f) plm(f, data=pdt2, effect="twoways", model="within"))
	lapply(mss, function(m) summary(m, vcov=function(x) vcovHC(x, method="ar")))
	lapply(mss, function(m) summary(m, vcov=function(x) clubSandwich::vcovCR(x, cluster="Country", type="CR0")))
	clubSandwich::vcovCR(mss[[3]], cluster="Country", type="CR0")
	clubSandwich::vcovCR(mss[[3]], cluster="Country", type="CR1")

	ms <- lapply(fs, function(f) plm(f, data=pdt2, effect="twoways", model="within"))
	# lapply(ms, function(m) summary(m, vcov=function(x) vcovHC(x, method="ar")))
	mms <- lapply(fms, function(fm) plm(fm, data=pdt2, effect="twoways", model="within"))
	# lapply(mms[3], function(mm) summary(mm, vcov=function(x) vcovHC(x, method="ar")))


## robust (recoding Cambodia and Haiti as treatment)
	rfs <- sapply(dvs, function(dv) formula(paste0(dv," ~ T2r + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle")))
	rfms <- lapply(dvs, function(dv) {
		lapply(mos, function(mo) {
			formula(paste0(dv, " ~ T2r*", mo, " + ", paste(mos[-which(mos==mo)], collapse=" + "), " + femalepc + regularwkpc + size + factoryageln + Cycle"))
			})
		}) |> unlist(use.names=FALSE)

	rms <- lapply(rfs, function(rf) plm(rf, data=pdt2, effect="twoways", model="within"))
	# lapply(rms[1:4], function(rm) summary(rm, vcov=function(x) vcovHC(x, method="ar")))
	rmms <- lapply(rfms, function(rfm) plm(rfm, data=pdt2, effect="twoways", model="within"))
	# lapply(rmms[3], function(rmm) summary(rmm, vcov=function(x) vcovHC(x, method="ar")))


## regression tables
	modelsummary(c(mss,ms), output="main_new.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)
	modelsummary(mms, output="moderators.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)

	modelsummary(rms, output="rmain.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)
	modelsummary(rmms, output="rmoderators.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)


## moderation plots
	options(ggeffects_margin = "empirical")

	predict_response(mms[[1]], terms=c("T2",mos[1]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Fashion Transporancy Index Band (buyer 1)") + theme(legend.position = "bottom")
	ggsave(paste0("moderator_",mos[1],".png"))

	predict_response(mms[[2]], terms=c("T2",mos[2]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="RepRisk Incidents (since 2010)") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("moderator_",mos[2],".png"))

	predict_response(mms[[3]], terms=c("T2",mos[3]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Management System Index") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("moderator_",mos[3],".png"))

	predict_response(mms[[4]], terms=c("T2",mos[4]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Union") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("No", "Yes"))
	ggsave(paste0("moderator_",mos[4],".png"))


	predict_response(rmms[[1]], terms=c("T2r",mos[1]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Fashion Transporancy Index Band (buyer 1)") + theme(legend.position = "bottom")
	ggsave(paste0("rmoderator_",mos[1],".png"))

	predict_response(rmms[[2]], terms=c("T2r",mos[2]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="RepRisk Incidents (since 2010)") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("rmoderator_",mos[2],".png"))

	predict_response(rmms[[3]], terms=c("T2r",mos[3]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Management System Index") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("rmoderator_",mos[3],".png"))

	predict_response(rmms[[4]], terms=c("T2r",mos[4]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Union") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("No", "Yes"))
	ggsave(paste0("rmoderator_",mos[4],".png"))


## TESTING CODES
	# m_r_2 <- plm(reportedcompl ~ T2 + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2, vcov=function(x) vcovHC(x, method="ar"))

	# m_r_2b <- plm(reportedcompl ~ T2*buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2b, vcov=function(x) vcovHC(x, method="ar"))
	# m_r_2n <- plm(reportedcompl ~ T2*RRic2010 + buyer1FTindexband + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2n, vcov=function(x) vcovHC(x, method="ar"))
	# m_r_2m <- plm(reportedcompl ~ T2*mngindex13 + buyer1FTindexband + RRic2010 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2m, vcov=function(x) vcovHC(x, method="ar"))
	# m_r_2u <- plm(reportedcompl ~ T2*union + buyer1FTindexband + RRic2010 + mngindex13 + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2u, vcov=function(x) vcovHC(x, method="ar"))

	# sapply(1:4, function(i) {
	# 	predict_response(mms[[i]], terms=c("T2",mos[i]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(connect_lines=TRUE)
	# 	ggsave(paste0("moderator_",mos[i],".png"))
	# 	})

	# sapply(1:4, function(i) {
	# 	predict_response(rmms[[i]], terms=c("T2r",mos[i]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(connect_lines=TRUE)
	# 	ggsave(paste0("rmoderator_",mos[i],".png"))
	# 	})



# DiD by Callaway and Sant'Anna (2021)
	dvs_labels <- c("Reported","Similar","Distant")

## group-time ATEs for each of three outcomes
	m_gts <- lapply(dvs, function(dv) {
		set.seed(240921)
		att_gt(
			yname = dv, idname = "fid", tname = "ym_r", gname = "T2_ym",
			xformla = NULL, data = dt3,
			panel = TRUE, allow_unbalanced_panel = TRUE, clustervars = NULL,
			control_group = "nevertreated", #"notyettreated"
			est_method = "dr"
			)
		})
	# use the following code to understand the warning messages (in short, due to unbalanced panel and therefore missingness in certain periods)
	# subset(dt3, subset=!is.na(Country)) |> summarize(n=n(), .by=c(ym,ym_r, Country)) |> pivot_wider(names_from=Country, values_from=n) |> print(n=Inf)
	# the parallel trend assumption appears to be hold without covariates; including "size" and "factoryageln" as pre-treatment covariates does not substantially change the results

	for (i in seq_along(dvs)) {
		### pre-test the parallel assumption
		m_e <- aggte(m_gts[[i]], type="dynamic", na.rm=TRUE)
		# summary(m_e)
		ggdid(m_e) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title=paste(dvs_labels[i],"Items"), x="Exposure Periods")
		ggsave(paste0("DiD_dynamic_",dvs_labels[i],".png"), width=8, height=5)

		### summarize effects by country
		m_g <- aggte(m_gts[[i]], type="group", na.rm=TRUE)
		# summary(m_g)
		ggdid(m_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title=paste(dvs_labels[i],"Items"), y="Countries")
		ggsave(paste0("DiD_group_",dvs_labels[i],".png"), width=8, height=5)

		###	summarize the overall effect
		m_s <- aggte(m_gts[[i]], type="simple", na.rm=TRUE)
		sink(paste0("DiD_overall_",dvs_labels[i],".txt"))
		summary(m_s)
		sink()
	}


## TESTING CODES
	# m <- att_gt(
	# 	yname = "reportedcompl", idname = "fid", tname = "ym_r", gname = "T2_ym",
	# 	xformla = ~size+factoryageln, 
	# 	data = dt3,
	# 	# xformla = NULL, 
	# 	# data = subset(dt3, union==0),
	# 	panel = TRUE, allow_unbalanced_panel = TRUE, clustervars = NULL,
	# 	control_group = "nevertreated", #"notyettreated"
	# 	est_method = "ipw"
	# 	# est_method = "dr"
	# 	)
	# # warnings()

	# # summary(m)
	# ggdid(m) + theme(axis.text.y=element_text(size=10), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=10))

	# agg.es <- aggte(m, type="dynamic", na.rm=TRUE)
	# # summary(agg.es)
	# ggdid(agg.es) + scale_x_continuous(expand=c(0.01,0), n.breaks=10)

	# agg.cs <- aggte(m, type="calendar", na.rm=TRUE)
	# # summary(agg.cs)
	# ggdid(agg.cs) + scale_x_continuous(expand=c(0.01,0), guide=guide_axis(angle=90), n.breaks=10)

	# agg.gs <- aggte(m, type="group", na.rm=TRUE)
	# # summary(agg.gs)
	# ggdid(agg.gs) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"))

	# agg.simple <- aggte(m, type="simple", na.rm=TRUE)
	# summary(agg.simple)

	### moderation
	# mods <- list(
	# 	dt3_union0 = subset(dt3, union==0), dt3_union1 = subset(dt3, union==1),
	# 	dt3_mngl = subset(dt3, mng<7), dt3_mngh = subset(dt3, mng>=7),
	# 	dt3_b1l = subset(dt3, as.integer(buyer1FTindexband)<=3), dt3_b1h = subset(dt3, as.integer(buyer1FTindexband)<=3)
	# 	)
	# # sapply(mods, dim)

	# m_gt_mod <- lapply(mods, function(mod) {
	# 	lapply(dvs, function(dv) {
	# 		att_gt(
	# 			yname = dv,
	# 			idname = "fid",
	# 			tname = "ym",
	# 			gname = "T2_ym",
	# 			xformla = NULL,
	# 			data = mod,
	# 			panel = TRUE,
	# 			allow_unbalanced_panel = TRUE,
	# 			control_group = "nevertreated", #"notyettreated"
	# 			clustervars = NULL,
	# 			est_method = "dr"
	# 			)
	# 		})
	# 	})

	# for (i in seq_along(mods)) {
	# 	for (j in seq_along(dvs)) {
	# 		m_t <- aggte(m_gt_mod[[i]][[j]], type="dynamic", na.rm=TRUE)
	# 		summary(m_t)
	# 		ggdid(m_t) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title="Reported", x="Exposure Periods")
	# 		# ggsave(paste0("DiD_",str_remove(names(mods),"^dt3_")[i],"_",dvs[j],"_dynamic.png"))

	# 		m_g <- aggte(m_gt_mod[[i]][[j]], type="group", na.rm=TRUE)
	# 		summary(m_g)
	# 		ggdid(m_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title="Reported", y="Countries")
	# 		# ggsave(paste0("DiD_",str_remove(names(mods),"^dt3_")[i],"_",dvs[j],"_group.png"))
	# 	}
	# }