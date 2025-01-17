library(tidyverse)
library(fixest)
library(haven)
library(modelsummary)

####loading data
load("positions.RData")
load("positions_by_actor.RData")
load("issues.RData")

#Removing non-EU counties
positions_by_actor <- positions_by_actor %>% filter(!(actor %in% c("Switzerland","Norway", "Iceland")))

####Extracting Applicant positions, CJEU positions, legal issue topics####

#Applicant Positions
applicant_positions <- positions_by_actor %>% filter(str_detect(actor, regex("applicant", ignore_case = TRUE))) %>%
  select(issue_id, "applicant_position" = position_id) %>% group_by(issue_id) %>%
  summarize(applicant_position = str_c(applicant_position, collapse = ', '))

#CJEU Positions
CJEU_positions <- positions_by_actor %>% filter(iuropa_actor_id == "A:I:CJEU") %>%
  select(issue_id, "CJEU_position" = position_id) %>% 
  group_by(issue_id) %>%
  summarize(CJEU_position = str_c(CJEU_position, collapse = ', '))

#Putting together CJEU positions and applicant positions and creating dependent variable
CJEU_applicant <- left_join(applicant_positions, CJEU_positions, by = "issue_id") %>%
  rowwise() %>%
  mutate(CJEU_position = str_split(CJEU_position, ', ')) %>%
  mutate(applicant_position = str_split(applicant_position, ', ')) %>%
  mutate(CJEU_for_applicant = list(applicant_position %in% CJEU_position),
         CJEU_not_applicant = list(!(applicant_position %in% CJEU_position))) %>%
  mutate(CJEU_for_applicant = sum(CJEU_for_applicant),
         CJEU_not_applicant = sum(CJEU_not_applicant)) %>%
  mutate(CJEU_for_applicant = ifelse(CJEU_for_applicant > 0, 1, 0),
         CJEU_not_applicant = ifelse(CJEU_not_applicant > 0, 1, 0),
         CJEU_all_for_applicant = ifelse(CJEU_for_applicant == CJEU_not_applicant, 0,CJEU_for_applicant)) #47 legal issues CJEU both for and against

#Separating out Commission positions
Commission_positions <- positions_by_actor %>% filter(iuropa_actor_id == "A:I:COM") %>%
  select(issue_id, "Commission_position" = position_id) %>% group_by(issue_id) %>%
  summarize(Commission_position = str_c(Commission_position, collapse = ', '))

#Adding in Commission positions and creating variable for Commission for/against applicant
CJEU_applicant <- left_join(CJEU_applicant, Commission_positions, by = "issue_id") %>%
  rowwise() %>%
  mutate(Commission_position = str_split(Commission_position, ', ')) %>%
  mutate(Commission_for_applicant = list(applicant_position %in% Commission_position),
         Commission_not_applicant = list(!(applicant_position %in% Commission_position))) %>%
  mutate(Commission_for_applicant = sum(Commission_for_applicant),
         Commission_not_applicant = sum(Commission_not_applicant)) %>%
  mutate(Commission_for_applicant = ifelse(Commission_for_applicant > 0, 1, 0),
         Commission_for_applicant = ifelse(is.na(Commission_for_applicant), 0, Commission_for_applicant),
         Commission_not_applicant = ifelse(Commission_not_applicant > 0, 1, 0),
         Commission_not_applicant = ifelse(is.na(Commission_not_applicant), 0, Commission_not_applicant),
         Commission_all_for_applicant = ifelse(Commission_for_applicant==Commission_not_applicant,0,Commission_for_applicant)) #38 issues in which Commission both is for applicant and has a position not favored by applicant

#Separating out AG positions
AG_positions <- positions_by_actor %>% filter(actor == "Advocate General") %>%
  select(issue_id, "AG_position" = position_id) %>% group_by(issue_id) %>%
  summarize(AG_position = str_c(AG_position, collapse = ', '))

#Adding in AG position and creating AG variables
CJEU_applicant <- left_join(CJEU_applicant, AG_positions, by = "issue_id") %>%
  rowwise() %>%
  mutate(AG_position = str_split(AG_position, ', ')) %>%
  mutate(AG_for_applicant = list(applicant_position %in% AG_position),
         AG_not_applicant = list(!(applicant_position %in% AG_position))) %>%
  mutate(AG_for_applicant = sum(AG_for_applicant),
         AG_not_applicant = sum(AG_not_applicant)) %>%
  mutate(AG_for_applicant = ifelse(AG_for_applicant > 0, 1, 0),
         AG_for_applicant = ifelse(is.na(AG_for_applicant), 0, AG_for_applicant),
         AG_not_applicant = ifelse(AG_not_applicant > 0,1,0),
         AG_not_applicant = ifelse(is.na(AG_not_applicant), 0, AG_not_applicant),
         AG_all_for_applicant = ifelse(AG_for_applicant == AG_not_applicant, 0, AG_for_applicant)) #39 issues in which AG agrees and disagrees with applicant

#Separating out member state positions
ms_positions <- positions_by_actor %>% filter(str_detect(iuropa_actor_id, regex("A:MS", ignore_case = TRUE))) %>%
  select(issue_id, actor, "ms_position" = position_id) %>% group_by(issue_id, actor) %>%
  summarize(ms_position = str_c(ms_position, collapse = ', '))

#Creating variables for member state positions
CJEU_applicant_netobs <- left_join(CJEU_applicant, ms_positions, by = "issue_id") %>%
  select(issue_id,applicant_position,ms_position,actor) %>%
  rowwise() %>%
  mutate(ms_position = str_split(ms_position, ', ')) %>%
  mutate(ms_for_applicant = list(applicant_position %in% ms_position)) %>%
  mutate(ms_not_applicant = list(!(applicant_position %in% ms_position))) %>%
  mutate(ms_for_applicant = sum(ms_for_applicant),
         ms_not_applicant = sum(ms_not_applicant)) %>%
  mutate(ms_for_applicant = ifelse(ms_for_applicant > 0, 1,0),
         ms_for_applicant = ifelse(is.na(ms_for_applicant), 0, ms_for_applicant),
         ms_not_applicant = ifelse(ms_not_applicant > 0,1,0),
         ms_not_applicant = ifelse(is.na(ms_not_applicant), 0, ms_not_applicant),
         ms_all_for_applicant = ifelse(ms_for_applicant == ms_not_applicant,0, ms_for_applicant)) %>% 
  group_by(issue_id) %>% 
  summarize(ms_for_applicant = sum(ms_for_applicant),
            ms_not_applicant = sum(ms_not_applicant),
            ms_all_for_applicant = sum(ms_all_for_applicant))

#Merging back in MS positions
CJEU_applicant <- left_join(CJEU_applicant, CJEU_applicant_netobs, by = "issue_id")

#Extracting topics for each legal issue
issues <- issues %>% select(iuropa_proceeding_id, case_year, issue_id, about_derogation, about_direct_effect, about_validity,
                            national_law_primary, national_law_secondary)
#Joining data together
CJEU_applicant <- left_join(CJEU_applicant, issues, by = "issue_id")

####Creating dataset with judges####
load("assignments.RData")
assignments <- assignments %>% select(iuropa_proceeding_id,cjeu_case_id,iuropa_judge_id, judge, is_judge_rapporteur)
load("judges.RData")
judges <- judges %>% select(iuropa_judge_id,iuropa_member_state_id,member_state)
assignments_judges <- left_join(assignments, judges, by = "iuropa_judge_id")
assignments_judges_positions <- left_join(positions_by_actor, assignments_judges, by = "iuropa_proceeding_id") %>%
  filter(iuropa_actor_id == iuropa_member_state_id) %>% group_by(issue_id, actor, is_judge_rapporteur) %>%
  summarize(position_id = str_c(position_id, collapse = ', '))

#Creating Home Observations and JR Observations variables
assignments_judges_positions_issues <- left_join(applicant_positions,assignments_judges_positions, by = "issue_id") %>%
  rowwise() %>%
  mutate(position_id = str_split(position_id, ', ')) %>%
  mutate(applicant_position = str_split(applicant_position, ', ')) %>%
  mutate(home_obs_for_applicant = list(applicant_position %in% position_id)) %>%
  mutate(home_obs_not_applicant = list(!(applicant_position %in% position_id))) %>%
  mutate(home_obs_for_applicant = sum(home_obs_for_applicant),
         home_obs_not_applicant = sum(home_obs_not_applicant)) %>%
  mutate(home_obs_for_applicant = ifelse(home_obs_for_applicant > 0,1,0),
         home_obs_for_applicant = ifelse(is.na(actor),0,home_obs_for_applicant),
         home_obs_not_applicant = ifelse(home_obs_not_applicant > 0,1,0),
         home_obs_not_applicant = ifelse(is.na(actor),0,home_obs_not_applicant),
         home_obs_all_for_applicant = ifelse(home_obs_for_applicant == home_obs_not_applicant,0,home_obs_for_applicant),
         jr_for_applicant = home_obs_for_applicant*is_judge_rapporteur,
         jr_for_applicant = ifelse(is.na(is_judge_rapporteur),0,jr_for_applicant),
         jr_not_applicant = home_obs_not_applicant*is_judge_rapporteur,
         jr_not_applicant = ifelse(is.na(is_judge_rapporteur),0,jr_not_applicant),
         jr_all_for_applicant = ifelse(jr_for_applicant == jr_not_applicant,0,jr_for_applicant)) %>%
  group_by(issue_id) %>% 
  summarize(jr_for_applicant = sum(jr_for_applicant),
            jr_not_applicant = sum(jr_not_applicant),
            jr_all_for_applicant = sum(jr_all_for_applicant),
            home_obs_for_applicant = sum(home_obs_for_applicant),
            home_obs_not_applicant = sum(home_obs_not_applicant),
            home_obs_all_for_applicant = sum(home_obs_all_for_applicant),
            non_jr_for_applicant = sum(home_obs_for_applicant) - sum(jr_for_applicant),
            non_jr_not_applicant = sum(home_obs_not_applicant)- sum(jr_not_applicant),
            non_jr_all_for_applicant = sum(home_obs_for_applicant) - sum(jr_all_for_applicant)) #no instances where member state home judges had both for applicant and against

####Creating Dataset for analysis####
dat <- left_join(CJEU_applicant, assignments_judges_positions_issues, by = "issue_id") %>%
  mutate(ms_for_applicant = ms_for_applicant - home_obs_for_applicant,
         ms_not_applicant = ms_not_applicant - home_obs_not_applicant,
         ms_all_for_applicant = ms_all_for_applicant - home_obs_all_for_applicant) #removing home obs variables from other member state variables

#adding judge variables
load("judgments.RData")
judgments <- judgments %>% select(iuropa_proceeding_id, list_judges, judge_rapporteur, decision_date, count_judges) %>%
  mutate(judgment_year = year(decision_date))
dat <- left_join(dat, judgments, by = "iuropa_proceeding_id")

#################CARRUBBA AND GABEL DATA####################
load("judgments.RData")
judgments <- judgments %>% arrange(desc(count_judges)) %>% distinct(cjeu_case_id, .keep_all = T) %>%
  select(cjeu_case_id, cjeu_decision_id)

####Loading Carrubba and Gabel Data####
chapters345 <- read_dta("chapters345.dta") %>%
  filter(!is.na(year))
chapters345 <- chapters345 %>% dplyr::rename("United KingdomDef" = UKDef, "United KingdomPl" = UKPl)
chapters345 <- rowid_to_column(chapters345, "ID") #creating ID column

#Cleaning CG data to make compatible with IUROPA judges data
chapters345$casenumber <- as.character(str_extract_all(chapters345$casenumber,"\\d+.\\d+"))
chapters345$casenumber <- paste0("C-", chapters345$casenumber)
a <- chapters345 %>% 
  dplyr::select(ID,GermanyDef:FinlandDef) %>% 
  pivot_longer(-c("ID"), names_to = "member_state", values_to = "obs_def") %>% 
  filter(obs_def == 1) %>% mutate(member_state = str_remove(member_state,"Def")) %>%
  group_by(ID) %>% summarize(ms_def = list(member_state),
                             ms_not_applicant = sum(obs_def))
c <- chapters345 %>% dplyr::select(ID,GermanyPl:FinlandPl) %>% #observations or plaintiff 
  pivot_longer(-ID, names_to = "member_state", values_to = "obs_pl") %>% 
  filter(obs_pl == 1) %>% mutate(member_state = str_remove(member_state,"Pl")) %>%
  group_by(ID) %>% summarize(ms_applicant = list(member_state),
                             ms_for_applicant = sum(obs_pl))

#Creating dataset with judges
load("assignments.RData")
assignments <- assignments %>% select(iuropa_proceeding_id,cjeu_decision_id,
                                      iuropa_judge_id, judge, is_judge_rapporteur)
assignments <- left_join(judgments,assignments, by = "cjeu_decision_id")
load("judges.RData")
judges <- judges %>% select(iuropa_judge_id,iuropa_member_state_id,member_state)
assignments_judges <- left_join(assignments, judges, by = "iuropa_judge_id") %>%
  mutate(jr_member_state = ifelse(is_judge_rapporteur == 1, member_state, 0)) %>%
  filter(cjeu_decision_id %in% judgments$cjeu_decision_id)

#Finding extra judges (CJEU had even number of judges so some cases had multiple judges from same country)
extra_judges <- assignments_judges %>% group_by(cjeu_case_id,member_state) %>% 
  distinct(cjeu_case_id,judge, .keep_all =  T) %>%
  summarize(n=n()) %>% mutate(extra_judge = ifelse(n>1,1,0),
                              ms_extra_judge = ifelse(extra_judge == 1, member_state, NA)) %>%
  filter(extra_judge == 1) %>%
  select(cjeu_case_id,ms_extra_judge) 

#merging back in 
assignments_judges <- left_join(assignments_judges,extra_judges, by = "cjeu_case_id")

#Separating out relevant variables from CG data
dat_cg <- chapters345 %>% select(ID, "cjeu_case_id" = casenumber, "legal_issue" = legalissue,
                                 article169:article179, 
                                 "Commission_is_defendant" = CommIsDef, "Commission_is_applicant" = CommIsPl,
                                 "Commission_for_applicant" = CommObsPl, "Commission_not_applicant" = CommObsDef,
                                 "Government_is_litigant" = govislit, "AG_for_applicant" = AGforPl,
                                 "CJEU_for_applicant" = NewECJPlAgree)

#Adding member state observations
dat_cg <- left_join(dat_cg, a, by = "ID") %>% 
  mutate(ms_not_applicant = ifelse(is.na(ms_not_applicant),0,ms_not_applicant)) %>% 
  mutate(across(everything(),~replace(., lengths(.) == 0, NA)))

dat_cg <- left_join(dat_cg,c, by = "ID") %>%
  mutate(ms_for_applicant = ifelse(is.na(ms_for_applicant),0,ms_for_applicant)) %>%
  mutate(across(everything(),~replace(., lengths(.) == 0, NA)))

#Putting together data and creating independent variables for analysis
assignments_judges_dat_cg <- left_join(dat_cg, assignments_judges, by = "cjeu_case_id") %>%
  rowwise() %>% 
  mutate(home_obs_for_applicant = ifelse(member_state %in% ms_applicant,1,0),
         home_obs_for_applicant = ifelse(is.na(member_state),0,home_obs_for_applicant),
         home_obs_not_applicant = ifelse(member_state %in% ms_def,1,0),
         home_obs_not_applicant = ifelse(is.na(member_state),0,home_obs_not_applicant),
         jr = ifelse(jr_member_state == member_state,1,0),
         jr_for_applicant = home_obs_for_applicant*jr,
         jr_for_applicant = ifelse(is.na(jr_member_state),0,jr_for_applicant),
         jr_not_applicant = home_obs_not_applicant*jr,
         extra_judge = ifelse(ms_extra_judge == member_state,1,0),
         home_obs_extra_for_applicant = home_obs_for_applicant*extra_judge,
         home_obs_extra_not_applicant = home_obs_not_applicant*extra_judge) %>%
  group_by(ID) %>%
  summarize(home_obs_for_applicant = sum(home_obs_for_applicant),
            home_obs_not_applicant = sum(home_obs_not_applicant),
            jr_for_applicant = sum(jr_for_applicant),
            jr_not_applicant = sum(jr_not_applicant),
            non_jr_for_applicant = sum(home_obs_for_applicant) - sum(jr_for_applicant),
            non_jr_not_applicant = sum(home_obs_not_applicant) - sum(jr_not_applicant),
            home_obs_extra_for_applicant = ifelse(sum(home_obs_extra_for_applicant) > 0,1,0),
            home_obs_extra_not_applicant = ifelse(sum(home_obs_extra_not_applicant) > 0,1,0)) %>%
  mutate(home_obs_extra_for_applicant = ifelse(is.na(home_obs_extra_for_applicant),0,home_obs_extra_for_applicant),
         home_obs_extra_not_applicant = ifelse(is.na(home_obs_extra_not_applicant),0,home_obs_extra_not_applicant))

#creating full dataset
dat_cg <- left_join(dat_cg, assignments_judges_dat_cg, by = "ID") %>%
  mutate(ms_not_applicant = ifelse(is.na(ms_not_applicant),0,ms_not_applicant), #replacing NAs with 0s
         ms_not_applicant = ms_not_applicant - home_obs_not_applicant + home_obs_extra_not_applicant, #taking into account extra judges
         ms_for_applicant = ifelse(is.na(ms_for_applicant),0,ms_for_applicant),
         ms_for_applicant = ms_for_applicant - home_obs_for_applicant + home_obs_extra_for_applicant) 

#adding judge variables to dataset
load("judgments.RData")
judgments <- judgments %>% select(cjeu_case_id,iuropa_proceeding_id, list_judges, judge_rapporteur, decision_date, count_judges) %>%
  mutate(judgment_year = year(decision_date)) %>% filter(cjeu_case_id %in% dat_cg$cjeu_case_id) %>%
  distinct(cjeu_case_id, .keep_all = T)

#full Carrubba and Gabel dataset
dat_cg <- left_join(dat_cg, judgments, by = "cjeu_case_id")

##################ANALYSIS FOR BOTH DATA SETS######################

#Carrubba and Gabel Data
mod_1_cg <- feols(CJEU_for_applicant~ home_obs_for_applicant + home_obs_not_applicant, 
                  cluster =~iuropa_proceeding_id,
                  data = filter(dat_cg, judgment_year < 1995))

mod_2_cg <- feols(CJEU_for_applicant~ home_obs_for_applicant + home_obs_not_applicant + ms_for_applicant +
                    ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                    Commission_is_applicant + Commission_is_defendant + Government_is_litigant + 
                    count_judges + article169 + article173 + article175 + article177 + article179, 
                  cluster =~iuropa_proceeding_id,
                  data = filter(dat_cg, judgment_year < 1995))

mod_3_cg <- feols(CJEU_for_applicant~ home_obs_for_applicant + home_obs_not_applicant + ms_for_applicant +
                    ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                    Commission_is_applicant + Commission_is_defendant + Government_is_litigant + AG_for_applicant +
                    count_judges + article169 + article173 + article175 + article177 + article179, 
                  cluster =~iuropa_proceeding_id,
                  data = filter(dat_cg, judgment_year < 1995))

mod_4_cg <- feols(CJEU_for_applicant~ jr_for_applicant + jr_not_applicant +
                    non_jr_for_applicant + non_jr_not_applicant, 
                  cluster =~iuropa_proceeding_id,
                  data = filter(dat_cg, judgment_year < 1995))

mod_5_cg <- feols(CJEU_for_applicant~ jr_for_applicant + jr_not_applicant +
                    non_jr_for_applicant + non_jr_not_applicant + ms_for_applicant +
                    ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                    Commission_is_applicant + Commission_is_defendant + Government_is_litigant + 
                    count_judges + article169 + article173 + article175 + article177 + article179, 
                  cluster =~iuropa_proceeding_id,
                  data = filter(dat_cg, judgment_year < 1995))

mod_6_cg <- feols(CJEU_for_applicant~ jr_for_applicant + jr_not_applicant +
                    non_jr_for_applicant + non_jr_not_applicant + ms_for_applicant +
                    ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                    Commission_is_applicant + Commission_is_defendant + Government_is_litigant + 
                    AG_for_applicant + count_judges +
                    article169 + article173 + article175 + article177 + article179, 
                  cluster =~iuropa_proceeding_id,
                  data = filter(dat_cg, judgment_year < 1995))

#IUROPA Issues and Positions data
mod_1 <- feols(CJEU_all_for_applicant ~ home_obs_all_for_applicant + home_obs_not_applicant,
               cluster =~ iuropa_proceeding_id,
               data = dat)

mod_2 <- feols(CJEU_all_for_applicant ~ home_obs_all_for_applicant + home_obs_not_applicant + 
                 Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant +
                 about_derogation + about_direct_effect +
                 about_validity + count_judges|national_law_primary + national_law_secondary,
               cluster =~ iuropa_proceeding_id,
               data = dat)

mod_3 <- feols(CJEU_all_for_applicant ~ home_obs_all_for_applicant + home_obs_not_applicant + 
                 Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant + AG_for_applicant +
                 about_derogation + about_direct_effect +
                 about_validity + count_judges|national_law_primary + national_law_secondary,
               cluster =~ iuropa_proceeding_id,
               data = dat)

mod_4 <- feols(CJEU_all_for_applicant ~ jr_all_for_applicant + jr_not_applicant + non_jr_not_applicant + non_jr_all_for_applicant,
               cluster =~ iuropa_proceeding_id,
               data = dat)

mod_5 <- feols(CJEU_all_for_applicant ~ jr_all_for_applicant + jr_not_applicant + non_jr_not_applicant + non_jr_all_for_applicant +
                 Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant + 
                 about_derogation + about_direct_effect  +
                 about_validity + count_judges|national_law_primary + national_law_secondary,
               cluster =~ iuropa_proceeding_id,
               data = dat)

mod_6 <- feols(CJEU_all_for_applicant ~ jr_all_for_applicant + jr_not_applicant + non_jr_not_applicant + non_jr_all_for_applicant +
                 Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant + AG_for_applicant +
                 about_derogation + about_direct_effect  +
                 about_validity + count_judges|national_law_primary + national_law_secondary,
               cluster =~ iuropa_proceeding_id,
               data = dat)

#Dictionary for variable names in manuscript tables
dict = c("CJEU_for_applicant" = "CJEU for Applicant", "jr_for_applicant" = "JR for Applicant", "jr_all_for_applicant" = "JR for Applicant",
         "non_jr_for_applicant" = "Non-JR for Applicant", "non_jr_all_for_applicant" = "Non-JR for Applicant", "non_jr_not_applicant" = "Non-JR not for Applicant",
         "jr_not_applicant" = "JR not for Applicant", "home_obs_for_applicant" = "Home Observations for Applicant",
         "home_obs_not_applicant" = "Home Observations not for Applicant","home_obs_all_for_applicant" = "Home Observations for Applicant",
         "ms_all_for_applicant" = "Other Observations for Applicant", "ms_for_applicant" = "Other Observations for Applicant",
         "ms_not_applicant" = "Other Observations not for Applicant", "Commission_all_for_applicant" = "Commission for Applicant",
         "Commission_for_applicant" = "Commission for Applicant", "Commission_not_applicant" = "Commission not for Applicant",
         "Commission_is_defendant" = "Commission is Defendant", "Commission_is_applicant" = "Commission is Applicant",
         "Government_is_litigant" = "Government is Litigant","about_derogationuncertain" = "About Derogation (Uncertain)",
         "about_derogationyes" = "About Derogation", "about_direct_effectuncertain" = "About Direct Effect (Uncertain)",
         "about_direct_effectyes" = "About Direct Effect", "about_validityuncertain" = "About Validity (Uncertain)",
         "about_validityyes" = "About Validity","ms_obs_not_applicant" = "Other Observations not for Applicant",
         "ms_obs_all_for_applicant" = "Other Observations for Applicant","AGforPl" = "AG for Plaintiff", 
         "govislit" = "Government is Litigant",  "article169" = "Infringement Case","article173" = "Annulment Case", 
         "article175" = "Failure to Act Case", "article177" = "Preliminary Reference Case","article179" = "Staff Case", 
         "national_law_primary" = "National Law Primary", "national_law_secondary" = "National Law Secondary",
         "count_judges" = "Number of Judges", "CJEU_all_for_applicant" = "CJEU Agrees with Applicant",
         "AG_for_applicant" = "AG for Applicant")

####Table 3###
etable(mod_1_cg,mod_2_cg,mod_3_cg,mod_4_cg,mod_5_cg,mod_6_cg,
       dict = dict, style.tex = style.tex("aer"), digits = 2,
       fitstat = ~ r2 + n, tex = T)

####Table 4####
etable(mod_1,mod_2,mod_3,mod_4,mod_5,mod_6,
       dict = dict, style.tex = style.tex("aer"), digits = 2,
       fitstat = ~ r2 + n, tex = T)

####Logit Robustness####

#Carrubba and Gabel Data
mod_1_cg_logit <- feglm(CJEU_for_applicant~ home_obs_for_applicant + home_obs_not_applicant, 
                        cluster =~iuropa_proceeding_id, family = "binomial",
                        data = filter(dat_cg, judgment_year < 1995))

mod_2_cg_logit <- feglm(CJEU_for_applicant~ home_obs_for_applicant + home_obs_not_applicant + ms_for_applicant +
                          ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                          Commission_is_applicant + Commission_is_defendant + Government_is_litigant + 
                          count_judges +
                          article169 + article173 + article175 + article177 + article179, 
                        cluster =~iuropa_proceeding_id, family = "binomial",
                        data = filter(dat_cg, judgment_year < 1995))

mod_3_cg_logit <- feglm(CJEU_for_applicant~ home_obs_for_applicant + home_obs_not_applicant + ms_for_applicant +
                          ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                          Commission_is_applicant + Commission_is_defendant + Government_is_litigant + 
                          AG_for_applicant + count_judges +
                          article169 + article173 + article175 + article177 + article179, 
                        cluster =~iuropa_proceeding_id, family = "binomial",
                        data = filter(dat_cg, judgment_year < 1995))

mod_4_cg_logit <- feglm(CJEU_for_applicant~ jr_for_applicant + jr_not_applicant +
                          non_jr_for_applicant + non_jr_not_applicant, 
                        cluster =~iuropa_proceeding_id, family = "binomial",
                        data = filter(dat_cg, judgment_year < 1995))

mod_5_cg_logit <- feglm(CJEU_for_applicant~ jr_for_applicant + jr_not_applicant +
                          non_jr_for_applicant + non_jr_not_applicant + ms_for_applicant +
                          ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                          Commission_is_applicant + Commission_is_defendant + Government_is_litigant + 
                          count_judges + article169 + article173 + article175 + article177 + article179, 
                        cluster =~iuropa_proceeding_id, family = "binomial",
                        data = filter(dat_cg, judgment_year < 1995))

mod_6_cg_logit <- feglm(CJEU_for_applicant~ jr_for_applicant + jr_not_applicant +
                          non_jr_for_applicant + non_jr_not_applicant + ms_for_applicant +
                          ms_not_applicant + Commission_for_applicant + Commission_not_applicant +
                          Commission_is_applicant + Commission_is_defendant + Government_is_litigant + 
                          AG_for_applicant + count_judges +
                          article169 + article173 + article175 + article177 + article179, 
                        cluster =~iuropa_proceeding_id, family = "binomial",
                        data = filter(dat_cg, judgment_year < 1995))

#IUROPA Data
mod_1_logit <- feglm(CJEU_all_for_applicant ~ home_obs_all_for_applicant + home_obs_not_applicant,
                     cluster =~ iuropa_proceeding_id, family = "binomial",
                     data = dat)

mod_2_logit <- feglm(CJEU_all_for_applicant ~ home_obs_all_for_applicant + home_obs_not_applicant + 
                       Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant + about_derogation + about_direct_effect +
                       about_validity + count_judges|national_law_primary + national_law_secondary,
                     cluster =~ iuropa_proceeding_id, family = "binomial",
                     data = dat)

mod_3_logit <- feglm(CJEU_all_for_applicant ~ home_obs_all_for_applicant + home_obs_not_applicant + 
                       Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant + about_derogation + about_direct_effect +
                       about_validity + count_judges + AG_for_applicant|national_law_primary + national_law_secondary,
                     cluster =~ iuropa_proceeding_id, family = "binomial",
                     data = dat)

mod_4_logit <- feglm(CJEU_all_for_applicant ~ jr_all_for_applicant + jr_not_applicant + non_jr_not_applicant + non_jr_all_for_applicant,
                     cluster =~ iuropa_proceeding_id, family = "binomial",
                     data = dat)

mod_5_logit <- feglm(CJEU_all_for_applicant ~ jr_all_for_applicant + jr_not_applicant + non_jr_not_applicant + non_jr_all_for_applicant +
                       Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant + about_derogation + about_direct_effect +
                       about_validity + count_judges|national_law_primary + national_law_secondary,
                     cluster =~ iuropa_proceeding_id, family = "binomial",
                     data = dat)

mod_6_logit <- feglm(CJEU_all_for_applicant ~ jr_all_for_applicant + jr_not_applicant + non_jr_not_applicant + non_jr_all_for_applicant +
                       Commission_all_for_applicant + ms_all_for_applicant + ms_not_applicant + about_derogation + about_direct_effect +
                       about_validity + count_judges + AG_for_applicant|national_law_primary + national_law_secondary,
                     cluster =~ iuropa_proceeding_id, family = "binomial", data = dat)

etable(mod_1_cg_logit,mod_2_cg_logit,mod_3_cg_logit,mod_4_cg_logit,mod_5_cg_logit,mod_6_cg_logit,
       dict = dict, style.tex = style.tex("aer"), digits = 2,
       fitstat = ~ pr2 + n, tex = T)

etable(mod_1_logit,mod_2_logit,mod_3_logit,mod_4_logit,mod_5_logit,mod_6_logit,
       dict = dict, style.tex = style.tex("aer"), digits = 2,
       fitstat = ~ pr2 + n, tex = T)

####Descriptive Statistics Tables####

####Table 2####
modsum_dat <- dat %>% select("CJEU for Applicant" = "CJEU_all_for_applicant",
                             "JR for Applicant" = "jr_all_for_applicant", 
                             "JR not for Applicant" = "jr_not_applicant",
                             "Non-JR for Applicant" = "non_jr_all_for_applicant", 
                             "Non-JR not for Applicant" = "non_jr_not_applicant",
                             "Home Observations for Applicant" = "home_obs_all_for_applicant",
                             "Home Observations not for Applicant" = "home_obs_not_applicant",
                             "Other Observations for Applicant" = "ms_all_for_applicant", 
                             "Other Observations not for Applicant" = "ms_not_applicant", 
                             "Commission for Applicant" = "Commission_all_for_applicant",
                             "Number of Judges" = "count_judges", "AG for Applicant" = "AG_for_applicant")
datasummary(All(modsum_dat)~ Mean + SD + Min + Max, output = 'latex', data = modsum_dat)

####TABLE 1####
modsum_dat_cg <- dat_cg %>% select("CJEU for Applicant" = "CJEU_for_applicant",
                                   "JR for Applicant" = "jr_for_applicant",
                                   "JR not for Applicant" = "jr_not_applicant",
                                   "Non-JR for Applicant" = "non_jr_for_applicant", 
                                   "Non-JR not for Applicant" = "non_jr_not_applicant",
                                   "Home Observations for Applicant" = "home_obs_for_applicant",
                                   "Home Observations not for Applicant" = "home_obs_not_applicant",
                                   "Other Observations for Applicant" = "ms_for_applicant", 
                                   "Other Observations not for Applicant" = "ms_not_applicant", 
                                   "Commission for Applicant" = "Commission_for_applicant",
                                   "Commission not for Applicant" = "Commission_not_applicant",
                                   "Number of Judges" = "count_judges", 
                                   "AG for Applicant" = "AG_for_applicant",
                                   "Government is Litigant" = "Government_is_litigant",  
                                   "Infringement Case" = "article169",
                                   "Annulment Case" = "article173", 
                                   "Failure to Act Case" = "article175", 
                                   "Preliminary Reference Case" = "article177",
                                   "Staff Case" = "article179")
datasummary(All(modsum_dat_cg)~ Mean + SD + Min + Max, output = 'latex', data = modsum_dat_cg)
