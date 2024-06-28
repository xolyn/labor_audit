########################################################################
#		Apply DiD methods to labor audits data
#		Yuequan
#		240623
########################################################################



# preparation
	## install R packages
	# install.packages("")

	## load R packages
	lib <- c("haven","tidyverse","plm","did","modelsummary")
	lapply(lib,require,character.only=TRUE)

	## clear up space
	rm(list=ls())


# sink("DiD_GT_240627.txt")
# read in clean data
	dt <- read_dta("June26audit variables for analysis.dta")

	dt2 <- dt %>%
		mutate(
			fid = fct(as.character(FactoryAssessedID)),
			AssesmentDate = dmy(AssesmentDate),
			ym = floor_date(AssesmentDate, unit="month"),
			b1i = if_else(buyer1FTindex<0, NA, buyer1FTindex),
			T1 = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua") & AssesmentDate>=ymd(20170701),
				"1","0"
				) %>% fct(levels=c("0","1")), # uniform treatment
			T2 = case_when(
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				TRUE 																									~ "0"
				) %>% fct(levels=c("0","1")) # staggered treatment
			)
	# names(dt2)
	# with(dt2, table(buyer1FTindex, exclude=NULL))

	dt3 <- dt %>%
		mutate(
			fid = as.integer(FactoryAssessedID),
			AssesmentDate = dmy(AssesmentDate),	y = year(AssesmentDate), m = month(AssesmentDate),
			ym = format(AssesmentDate, "%Y%m") %>% as.integer, ym_r = dense_rank(ym),
			ym2 = str_c(y, ceiling(m/2)) %>% as.integer, ym2_r = dense_rank(ym2),
			yq = str_c(y, quarter(AssesmentDate)) %>% as.integer, yq_r = dense_rank(yq),
			T1 = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				2017,0
				),
			T1_ym = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				31,0
				), # dense_rank(201707)==31
			T1_ym2 = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				16,0 # dense_rank(20174)==16
				),
			T1_yq = if_else(
				Country%in%c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"),
				11,0 # dense_rank(20173)==11
				),
			T2 = case_when(
				Country=="Vietnam" 			~ 2016, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 2016, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 2017, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 2017, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 2018, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				),
			T2_ym = case_when(
				Country=="Vietnam" 			~ 18, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 23, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 25, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 31, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 37, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				),
			T2_ym2 = case_when(
				Country=="Vietnam" 			~ 9, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 12, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 13, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 16, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 19, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				),
			T2_yq = case_when(
				Country=="Vietnam" 			~ 6, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 8, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 9, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 11, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 13, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				)
			)
	# str(dt3$ym)
	# with(dt3, table(yq, yq_r, exclude=NULL))
	# subset(dt3, subset=!is.na(Country)) %>% summarize(n=n(), .by=c(Country, ym)) %>% pivot_wider(names_from=Country, values_from=n) %>% print(n=Inf)



sink("DiD_GT_240628.txt")
# DiD by Callaway and Sant'Anna (2021)
## group-time ATEs for each of three outcomes
	dvs <- c("reportedcompl","similarCPcompl","distantCPcompl")
	dvs_labels <- c("Reported","Similar","Distant")

	m_gt <- lapply(dvs, function(dv) {
		att_gt(
			yname = dv, idname = "fid", tname = "ym_r", gname = "T2_ym",
			xformla = NULL, data = dt3,
			panel = TRUE, allow_unbalanced_panel = TRUE, clustervars = NULL,
			control_group = "nevertreated", #"notyettreated"
			est_method = "dr"
			)
		})

	for (i in seq_along(dvs)) {
		m_t <- aggte(m_gt[[i]], type="dynamic", na.rm=TRUE)
		summary(m_t)
		ggdid(m_t) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title=paste(dvs_labels[i],"Items"), x="Exposure Periods")
		ggsave(paste0("DiD_dynamic_",dvs_labels[i],".png"), width=8, height=5)

		m_g <- aggte(m_gt[[i]], type="group", na.rm=TRUE)
		summary(m_g)
		ggdid(m_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title=paste(dvs_labels[i],"Items"), y="Countries")
		ggsave(paste0("DiD_group_",dvs_labels[i],".png"), width=8, height=5)
	}
sink()


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
	# m <- att_gt(
	# 	yname = "similarCPcompl", idname = "fid", tname = "ym_r", gname = "T2_ym",
	# 	xformla = NULL, data = dt3,
	# 	panel = TRUE, allow_unbalanced_panel = TRUE, clustervars = NULL,
	# 	control_group = "nevertreated", #"notyettreated"
	# 	est_method = "dr"
	# 	)

	# summary(m)
	# ggdid(m) + theme(axis.text.y=element_text(size=10), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=10))

	# agg.es <- aggte(m, type="dynamic", na.rm=TRUE)
	# summary(agg.es)
	# ggdid(agg.es) + scale_x_continuous(expand=c(0.01,0), n.breaks=10)

	# # agg.cs <- aggte(m, type="calendar", na.rm=TRUE)
	# # summary(agg.cs)
	# # ggdid(agg.cs) + scale_x_continuous(expand=c(0.01,0), guide=guide_axis(angle=90), n.breaks=10)

	# agg.gs <- aggte(m, type="group", na.rm=TRUE)
	# summary(agg.gs)
	# ggdid(agg.gs) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"))

	# # agg.simple <- aggte(m, type="simple", na.rm=TRUE)
	# # summary(agg.simple)



# DiD by two-way fixed effects panel models
## reported
	m_r_1 <- plm(reportedcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_1, vcov=function(x) vcovHC(x, method="ar"))

	m_r_2 <- plm(reportedcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2, vcov=function(x) vcovHC(x, method="ar"))

	m_r_2u <- plm(reportedcompl ~ T2*union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2u, vcov=function(x) vcovHC(x, method="ar"))
	m_r_2m <- plm(reportedcompl ~ T2*mng + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2m, vcov=function(x) vcovHC(x, method="ar"))
	m_r_2b <- plm(reportedcompl ~ T2*b1i + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_r_2b, vcov=function(x) vcovHC(x, method="ar"))


## similar
	m_s_1 <- plm(similarCPcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_1, vcov=function(x) vcovHC(x, method="ar"))

	m_s_2 <- plm(similarCPcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2, vcov=function(x) vcovHC(x, method="ar"))

	m_s_2u <- plm(similarCPcompl ~ T2*union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2u, vcov=function(x) vcovHC(x, method="ar"))
	m_s_2m <- plm(similarCPcompl ~ T2*mng + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2m, vcov=function(x) vcovHC(x, method="ar"))
	m_s_2b <- plm(similarCPcompl ~ T2*b1i + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_s_2b, vcov=function(x) vcovHC(x, method="ar"))


## distant
	m_d_1 <- plm(distantCPcompl ~ T1 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_1, vcov=function(x) vcovHC(x, method="ar"))

	m_d_2 <- plm(distantCPcompl ~ T2 + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2, vcov=function(x) vcovHC(x, method="ar"))

	m_d_2u <- plm(distantCPcompl ~ T2*union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2u, vcov=function(x) vcovHC(x, method="ar"))
	m_d_2m <- plm(distantCPcompl ~ T2*mng + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2m, vcov=function(x) vcovHC(x, method="ar"))
	m_d_2b <- plm(distantCPcompl ~ T2*b1i + union + CBA + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym")) #  + mng
	summary(m_d_2b, vcov=function(x) vcovHC(x, method="ar"))


## coef plots
	m <- list(
		"reported/uniform"=m_r_1, "similar/uniform"=m_s_1, "distant/uniform"=m_d_1, 
		"reported/staggered"=m_r_2, "similar/staggered"=m_s_2, "distant/staggered"=m_d_2
		)
	modelplot(
		models=m,
		coef_map=c(
			"T21","T11"
			)
		) + geom_vline(xintercept=0) + scale_colour_brewer(palette="Set1") + theme_bw() + labs(title="DiD by FE panel models")
	# ggsave("DID_FE_240623.png")