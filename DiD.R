########################################################################
#		Apply DiD methods to labor audits data
#		Yuequan
#		240623
########################################################################



# preparation
	## install R packages
	# install.packages("")

	## load R packages
	lib <- c("haven","tidyverse","plm","did","modelsummary","ggeffects")
	lapply(lib,require,character.only=TRUE)

	## clear up space
	rm(list=ls())



# read in clean data
	dt <- read_dta("Aug31 data for analysis.dta")

	dt2 <- dt |>
		mutate(
			AssesmentDate = dmy(AssesmentDate),
			fid = fct(as.character(FactoryAssessedID)),
			ym = floor_date(AssesmentDate, unit="month"),
			Cycle = as.factor(Cycle),
			buyer1FTindexband = as.factor(ifelse(buyer1FTindexband==0, 99,buyer1FTindexband)),
			# T1 = if_else(
			# 	Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua") & AssesmentDate>=ymd(20170701),
			# 	"1","0"
			# 	) %>% fct(levels=c("0","1")), # uniform treatment
			T2 = case_when(
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				TRUE 																									~ "0"
				) %>% fct(levels=c("0","1")), # staggered treatment
			T2r = case_when(
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				# Country=="Haiti" 			& AssesmentDate>=ymd(20100101)	~ "1",
				Country=="Cambodia"		& AssesmentDate>=ymd(20140301)	~ "1",
				TRUE 																									~ "0"
				) %>% fct(levels=c("0","1")) # robust treatment
			) |>
		filter(AssesmentDate<ymd(20200301)) |> # before COVID19
		drop_na(mngindex13,union,femalepc,regularwkpc,size,factoryageln) # remove rows w/ missing values
	# names(dt2)
	# with(dt2, table(mngindex13, exclude=NULL))
	pdt2 <- pdata.frame(dt2, index=c("fid","ym"))

	dt3 <- dt |>
		mutate(
			AssesmentDate = dmy(AssesmentDate),	y = year(AssesmentDate), m = month(AssesmentDate),
			fid = as.integer(FactoryAssessedID),
			ym = format(AssesmentDate, "%Y%m") %>% as.integer, ym_r = dense_rank(ym),
			# ym2 = str_c(y, ceiling(m/2)) %>% as.integer, ym2_r = dense_rank(ym2),
			# yq = str_c(y, quarter(AssesmentDate)) %>% as.integer, yq_r = dense_rank(yq),
			# T1 = if_else(
			# 	Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
			# 	2017,0
			# 	),
			# T1_ym = if_else(
			# 	Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
			# 	31,0
			# 	), # dense_rank(201707)==31
			# T1_ym2 = if_else(
			# 	Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
			# 	16,0 # dense_rank(20174)==16
			# 	),
			# T1_yq = if_else(
			# 	Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
			# 	11,0 # dense_rank(20173)==11
			# 	),
			# T2 = case_when(
			# 	Country=="Vietnam" 			~ 2016, # AssesmentDate>=ymd(20160601)
			# 	Country=="Jordan" 			~ 2016, # AssesmentDate>=ymd(20161101)
			# 	Country=="Indonesia" 		~ 2017, # AssesmentDate>=ymd(20170101)
			# 	Country=="Haiti" 				~ 2017, # AssesmentDate>=ymd(20170701)
			# 	Country=="Nicaragua" 		~ 2018, # AssesmentDate>=ymd(20180101)
			# 	TRUE 										~ 0
			# 	),
			T2_ym = case_when(
				Country=="Vietnam" 			~ 18, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 23, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 25, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 31, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 37, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				),
			# T2_ym2 = case_when(
			# 	Country=="Vietnam" 			~ 9, # AssesmentDate>=ymd(20160601)
			# 	Country=="Jordan" 			~ 12, # AssesmentDate>=ymd(20161101)
			# 	Country=="Indonesia" 		~ 13, # AssesmentDate>=ymd(20170101)
			# 	Country=="Haiti" 				~ 16, # AssesmentDate>=ymd(20170701)
			# 	Country=="Nicaragua" 		~ 19, # AssesmentDate>=ymd(20180101)
			# 	TRUE 										~ 0
			# 	),
			# T2_yq = case_when(
			# 	Country=="Vietnam" 			~ 6, # AssesmentDate>=ymd(20160601)
			# 	Country=="Jordan" 			~ 8, # AssesmentDate>=ymd(20161101)
			# 	Country=="Indonesia" 		~ 9, # AssesmentDate>=ymd(20170101)
			# 	Country=="Haiti" 				~ 11, # AssesmentDate>=ymd(20170701)
			# 	Country=="Nicaragua" 		~ 13, # AssesmentDate>=ymd(20180101)
			# 	TRUE 										~ 0
			# 	)
			) |>
		filter(AssesmentDate<ymd(20200301)) |>
		drop_na(mngindex13,union,femalepc,regularwkpc,size,factoryageln)
	# str(dt3$ym); dt3 |> arrange(ym) |> distinct(ym,ym_r) |> filter(ym==201801) |> print(n=Inf) # look up ym_r for the corresponding ym



# DiD by two-way fixed effects panel models
	dvs <- c("reportedcompl","similarCPcompl","distantCPcompl")
	mos <- c("buyer1FTindexband","RRic2010","mngindex13","union")

## main
	fs <- sapply(dvs, function(dv) formula(paste0(dv," ~ T2 + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle")))
	fms <- lapply(dvs, function(dv) {
		lapply(mos, function(mo) {
			formula(paste0(dv, " ~ T2*", mo, " + ", paste(mos[-which(mos==mo)], collapse=" + "), " + femalepc + regularwkpc + size + factoryageln + Cycle"))
			})
		}) |> unlist(use.names=FALSE)

	ms <- lapply(fs, function(f) plm(f, data=pdt2, effect="twoways", model="within"))
	# lapply(ms, function(m) summary(m, vcov=function(x) vcovHC(x, method="ar")))
	mms <- lapply(fms, function(fm) plm(fm, data=pdt2, effect="twoways", model="within"))
	# lapply(mms[4], function(mm) summary(mm, vcov=function(x) vcovHC(x, method="ar")))


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
	# lapply(rmms[4], function(rmm) summary(rmm, vcov=function(x) vcovHC(x, method="ar")))


## regression tables
	modelsummary(ms, output="main.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)
	modelsummary(mms, output="moderators.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)

	modelsummary(rms, output="rmain.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)
	modelsummary(rmms, output="rmoderators.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)


## moderation plots
	options(ggeffects_margin = "empirical")

	sapply(1:4, function(i) {
		predict_response(mms[[i]], terms=c("T2",mos[i]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(connect_lines=TRUE)
		ggsave(paste0("moderator_",mos[i],".png"))
		})

	sapply(1:4, function(i) {
		predict_response(rmms[[i]], terms=c("T2r",mos[i]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(connect_lines=TRUE)
		ggsave(paste0("rmoderator_",mos[i],".png"))
		})


## TESTING CODES
	m_r_2 <- plm(reportedcompl ~ T2 + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	summary(m_r_2, vcov=function(x) vcovHC(x, method="ar"))

	m_r_2b <- plm(reportedcompl ~ T2*buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	summary(m_r_2b, vcov=function(x) vcovHC(x, method="ar"))
	m_r_2n <- plm(reportedcompl ~ T2*RRic2010 + buyer1FTindexband + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	summary(m_r_2n, vcov=function(x) vcovHC(x, method="ar"))
	m_r_2m <- plm(reportedcompl ~ T2*mngindex13 + buyer1FTindexband + RRic2010 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	summary(m_r_2m, vcov=function(x) vcovHC(x, method="ar"))
	m_r_2u <- plm(reportedcompl ~ T2*union + buyer1FTindexband + RRic2010 + mngindex13 + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	summary(m_r_2u, vcov=function(x) vcovHC(x, method="ar"))



# DiD by Callaway and Sant'Anna (2021)
	dvs_labels <- c("Reported","Similar","Distant")

## group-time ATEs for each of three outcomes
	m_gts <- lapply(dvs, function(dv) {
		set.seed(240819)
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


## moderation
	# mods <- list(
	# 	dt3_union0 = subset(dt3, union==0), dt3_union1 = subset(dt3, union==1),
	# 	dt3_mngl = subset(dt3, mng<7), dt3_mngh = subset(dt3, mng>=7),
	# 	dt3_b1nn = subset(dt3, buyer1FTindex==-2), dt3_b1nr = subset(dt3, buyer1FTindex==-1), dt3_b1l = subset(dt3, buyer1FTindex<29), dt3_b1h = subset(dt3, buyer1FTindex>=29)
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


## TESTING CODES
	m <- att_gt(
		yname = "reportedcompl", idname = "fid", tname = "ym_r", gname = "T2_ym",
		xformla = ~size+factoryageln, data = dt3,
		panel = TRUE, allow_unbalanced_panel = TRUE, clustervars = NULL,
		control_group = "nevertreated", #"notyettreated"
		est_method = "ipw"
		)
	# warnings()

	# summary(m)
	ggdid(m) + theme(axis.text.y=element_text(size=10), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=10))

	agg.es <- aggte(m, type="dynamic", na.rm=TRUE)
	# summary(agg.es)
	ggdid(agg.es) + scale_x_continuous(expand=c(0.01,0), n.breaks=10)

	agg.cs <- aggte(m, type="calendar", na.rm=TRUE)
	# summary(agg.cs)
	ggdid(agg.cs) + scale_x_continuous(expand=c(0.01,0), guide=guide_axis(angle=90), n.breaks=10)

	agg.gs <- aggte(m, type="group", na.rm=TRUE)
	# summary(agg.gs)
	ggdid(agg.gs) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"))

	agg.simple <- aggte(m, type="simple", na.rm=TRUE)
	summary(agg.simple)