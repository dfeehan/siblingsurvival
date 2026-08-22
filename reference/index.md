# Package index

## All functions

- [`add_esc_ind_vis()`](http://dennisfeehan.org/siblingsurvival/reference/add_esc_ind_vis.md)
  : add individual visibility based on sib reprots to ego X sib X cell
  reports
- [`add_maternal_deaths()`](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)
  : add pregnancy-related and maternal death info to a sibling dataset
- [`agenames()`](http://dennisfeehan.org/siblingsurvival/reference/agenames.md)
  : make labels for age groups
- [`aggregate_maternal_estimates()`](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md)
  : calculate total rate based on point estimates
- [`attributes.to.long()`](http://dennisfeehan.org/siblingsurvival/reference/attributes.to.long.md)
  : attributes.to.long
- [`build_mics_ego_cols()`](http://dennisfeehan.org/siblingsurvival/reference/build_mics_ego_cols.md)
  : construct the respondent-level columns MICS does not supply
- [`calculate_sib_ind_visibility()`](http://dennisfeehan.org/siblingsurvival/reference/calculate_sib_ind_visibility.md)
  : given a sib dataset, calculate individual visibility weight for each
  sib
- [`cell_config()`](http://dennisfeehan.org/siblingsurvival/reference/cell_config.md)
  : Specify the cells (age groups, time period) to produce estimates for
- [`check_mics_varmap()`](http://dennisfeehan.org/siblingsurvival/reference/check_mics_varmap.md)
  : guard against the MM16 collision between MICS and the DHS
- [`check_varmap_cols()`](http://dennisfeehan.org/siblingsurvival/reference/check_varmap_cols.md)
  : report varmap columns that are missing from a dataset
- [`cpp_compute_occ_exp()`](http://dennisfeehan.org/siblingsurvival/reference/cpp_compute_occ_exp.md)
  : cpp_compute_occ_exp
- [`cpp_compute_occ_exp2()`](http://dennisfeehan.org/siblingsurvival/reference/cpp_compute_occ_exp2.md)
  : cpp_compute_occ_exp
- [`ex.ego`](http://dennisfeehan.org/siblingsurvival/reference/ex.ego.md)
  : Example ego-level dataset
- [`ex.sib`](http://dennisfeehan.org/siblingsurvival/reference/ex.sib.md)
  : Example sibling-level dataset
- [`finalize_sib_prep()`](http://dennisfeehan.org/siblingsurvival/reference/finalize_sib_prep.md)
  : shared tail of the sibling-history prep functions
- [`get_agg_est_from_ec()`](http://dennisfeehan.org/siblingsurvival/reference/get_agg_est_from_ec.md)
  : helper function for calculating aggregate visibility estimate from
  ego X cell data
- [`get_boot_ests_matrix()`](http://dennisfeehan.org/siblingsurvival/reference/get_boot_ests_matrix.md)
  : Fast bootstrap estimation using matrix multiplication
- [`get_ec_reports()`](http://dennisfeehan.org/siblingsurvival/reference/get_ec_reports.md)
  : get ego X cell reports
- [`get_ego_age_distn()`](http://dennisfeehan.org/siblingsurvival/reference/get_ego_age_distn.md)
  : get_ego_age_distribution
- [`get_ego_df()`](http://dennisfeehan.org/siblingsurvival/reference/get_ego_df.md)
  : helper to prep the ego dataset
- [`get_esc_reports()`](http://dennisfeehan.org/siblingsurvival/reference/get_esc_reports.md)
  : Get ego X sibling X cell reports
- [`get_ic_reports()`](http://dennisfeehan.org/siblingsurvival/reference/get_ic_reports.md)
  : get a dataset with reports used for internal-consistency checks
- [`get_ind_est_from_ec()`](http://dennisfeehan.org/siblingsurvival/reference/get_ind_est_from_ec.md)
  : helper function for calculating individual visibility estimate from
  ego X cell data
- [`get_sib_df()`](http://dennisfeehan.org/siblingsurvival/reference/get_sib_df.md)
  : helper to prep the sib dataset
- [`get_sibship_info()`](http://dennisfeehan.org/siblingsurvival/reference/get_sibship_info.md)
  : calculate number of sibs on frame for each respondent
- [`get_visibility()`](http://dennisfeehan.org/siblingsurvival/reference/get_visibility.md)
  : calculate visibility for each sibship and ego
- [`is_maternal_dhs()`](http://dennisfeehan.org/siblingsurvival/reference/is_maternal_dhs.md)
  : is each sibling's death maternal, by DHS coding?
- [`is_maternal_mics()`](http://dennisfeehan.org/siblingsurvival/reference/is_maternal_mics.md)
  : is each sibling's death maternal, by MICS coding?
- [`is_preg_related_dhs()`](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_dhs.md)
  : is each sibling's death pregnancy-related, by DHS coding?
- [`is_preg_related_mics()`](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_mics.md)
  : is each sibling's death pregnancy-related, by MICS coding?
- [`make.age.groups()`](http://dennisfeehan.org/siblingsurvival/reference/make.age.groups.md)
  : make an age.groups object
- [`make.even.age.groups()`](http://dennisfeehan.org/siblingsurvival/reference/make.even.age.groups.md)
  : make an age.groups object with evenly-sized intervals
- [`make.time.periods()`](http://dennisfeehan.org/siblingsurvival/reference/make.time.periods.md)
  : make a time.periods object
- [`mics_asked_maternity_questions()`](http://dennisfeehan.org/siblingsurvival/reference/mics_asked_maternity_questions.md)
  : were the MICS maternity questions asked of this sibling?
- [`model_dhs_dat`](http://dennisfeehan.org/siblingsurvival/reference/model_dhs_dat.md)
  : Model DHS dataset
- [`occ.exp()`](http://dennisfeehan.org/siblingsurvival/reference/occ.exp.md)
  : tabulate occurrences and exposures
- [`prep_dhs_sib_histories()`](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)
  : prepare a DHS dataset for analysis
- [`prep_mics_sib_histories()`](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md)
  : prepare a MICS dataset for analysis
- [`prep_nrsim_sib_histories()`](http://dennisfeehan.org/siblingsurvival/reference/prep_nrsim_sib_histories.md)
  : prepare a dataset from nrsimulatr for sibling analysis
- [`recode_mics_sib_vars()`](http://dennisfeehan.org/siblingsurvival/reference/recode_mics_sib_vars.md)
  : recode MICS sibling variables to the conventions the package expects
- [`reproductive_age_groups()`](http://dennisfeehan.org/siblingsurvival/reference/reproductive_age_groups.md)
  : the reproductive age groups used for maternal mortality estimates
- [`sib_ic_checks()`](http://dennisfeehan.org/siblingsurvival/reference/sib_ic_checks.md)
  : get calculate internal consistency checks for sibling reports
- [`sib_ic_checks_OLD()`](http://dennisfeehan.org/siblingsurvival/reference/sib_ic_checks_OLD.md)
  : get calculate internal consistency checks for sibling reports
- [`sibhist_varmap_dhs2`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_dhs2.md)
  : DHS sibling history variable map for DHS phase 2
- [`sibhist_varmap_dhs3`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_dhs3.md)
  : DHS sibling history variable map for DHS phase 3
- [`sibhist_varmap_dhs4`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_dhs4.md)
  : DHS sibling history variable map for DHS phase 4
- [`sibhist_varmap_dhs5`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_dhs5.md)
  : DHS sibling history variable map for DHS phase 5
- [`sibhist_varmap_dhs6`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_dhs6.md)
  : DHS sibling history variable map for DHS phase 6
- [`sibhist_varmap_dhs7`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_dhs7.md)
  : DHS sibling history variable map for DHS phase 7
- [`sibhist_varmap_dhs8`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_dhs8.md)
  : DHS sibling history variable map for DHS phase 8
- [`sibhist_varmap_mics4`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_mics4.md)
  : MICS sibling history variable map for MICS4
- [`sibhist_varmap_mics5`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_mics5.md)
  : MICS sibling history variable map for MICS5
- [`sibhist_varmap_mics6`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_mics6.md)
  : MICS sibling history variable map for MICS6
- [`sibhist_varmap_mics7`](http://dennisfeehan.org/siblingsurvival/reference/sibhist_varmap_mics7.md)
  : MICS sibling history variable map for MICS7
- [`sibling_estimator()`](http://dennisfeehan.org/siblingsurvival/reference/sibling_estimator.md)
  : Estimate death rates from sibling history data
- [`sibling_summ()`](http://dennisfeehan.org/siblingsurvival/reference/sibling_summ.md)
  : calculate summary statistics for siblings in a given time window
- [`warn_uninterviewed_sex()`](http://dennisfeehan.org/siblingsurvival/reference/warn_uninterviewed_sex.md)
  : warn when a sibling sex has no matching respondents
- [`window_intersect()`](http://dennisfeehan.org/siblingsurvival/reference/window_intersect.md)
  : window_intersect
