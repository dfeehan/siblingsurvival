#' Example ego-level dataset
#'
#' A dataset with one row per survey respondent (ego), for use in examples
#' and tests.
#'
#' @format A data frame with respondent-level variables.
#' @name ex.ego
NULL

#' Example sibling-level dataset
#'
#' A dataset with one row per reported sibling, for use in examples and tests.
#'
#' @format A data frame with sibling-level variables.
#' @name ex.sib
NULL

#' Model DHS dataset
#'
#' A simulated dataset structured to resemble DHS sibling history data,
#' used for demonstrations and testing.
#'
#' @format A data frame with DHS-style variables.
#' @name model_dhs_dat
NULL

#' DHS sibling history variable map for DHS phase 2
#'
#' A data frame mapping variable names used in DHS phase 2 sibling history
#' modules to the standardized names used by this package.
#'
#' @format A data frame with columns for original and standardized variable names.
#' @name sibhist_varmap_dhs2
NULL

#' DHS sibling history variable map for DHS phase 3
#'
#' A data frame mapping variable names used in DHS phase 3 sibling history
#' modules to the standardized names used by this package.
#'
#' @format A data frame with columns for original and standardized variable names.
#' @name sibhist_varmap_dhs3
NULL

#' DHS sibling history variable map for DHS phase 4
#'
#' A data frame mapping variable names used in DHS phase 4 sibling history
#' modules to the standardized names used by this package.
#'
#' @format A data frame with columns for original and standardized variable names.
#' @name sibhist_varmap_dhs4
NULL

#' DHS sibling history variable map for DHS phase 5
#'
#' A data frame mapping variable names used in DHS phase 5 sibling history
#' modules to the standardized names used by this package.
#'
#' @format A data frame with columns for original and standardized variable names.
#' @name sibhist_varmap_dhs5
NULL

#' DHS sibling history variable map for DHS phase 6
#'
#' A data frame mapping variable names used in DHS phase 6 sibling history
#' modules to the standardized names used by this package.
#'
#' @format A data frame with columns for original and standardized variable names.
#' @name sibhist_varmap_dhs6
NULL

#' DHS sibling history variable map for DHS phase 7
#'
#' A data frame mapping variable names used in DHS phase 7 sibling history
#' modules to the standardized names used by this package.
#'
#' @format A data frame with columns for original and standardized variable names.
#' @name sibhist_varmap_dhs7
NULL

#' DHS sibling history variable map for DHS phase 8
#'
#' A data frame mapping variable names used in DHS phase 8 sibling history
#' modules to the standardized names used by this package.
#'
#' @format A data frame with columns for original and standardized variable names.
#' @name sibhist_varmap_dhs8
NULL

#' MICS sibling history variable map for MICS4
#'
#' A data frame mapping variable names used in the MICS4 maternal mortality
#' module (`mm.sav`) to the standardized names used by this package.
#'
#' MICS4 and MICS5 share a roster numbering of `MM5`--`MM13`, which is different
#' from the `MM15`--`MM27` used from MICS6 onward. Neither MICS4 nor MICS5 asks
#' about violence or accidents, so they support **pregnancy-related**, not
#' strictly maternal, mortality -- the same limitation as DHS phases 2--6.
#'
#' Variable names are lowercase; see [prep_mics_sib_histories] and its
#' `lowercase` argument.
#'
#' @format A data frame with columns `orig.varname`, `new.varname`, `sibvar`,
#'   `description` and `comments`.
#' @name sibhist_varmap_mics4
NULL

#' MICS sibling history variable map for MICS5
#'
#' Identical to [sibhist_varmap_mics4]; MICS4 and MICS5 use the same roster
#' numbering.
#'
#' @format A data frame with columns `orig.varname`, `new.varname`, `sibvar`,
#'   `description` and `comments`.
#' @name sibhist_varmap_mics5
NULL

#' MICS sibling history variable map for MICS6
#'
#' A data frame mapping variable names used in the MICS6 maternal mortality
#' module (`mm.sav`) to the standardized names used by this package. This is the
#' default varmap for [prep_mics_sib_histories].
#'
#' MICS6 renumbered the roster to `MM15`--`MM27` and added `MM26` (violence) and
#' `MM27` (accident), so MICS6 supports **maternal** mortality as well as
#' pregnancy-related mortality.
#'
#' Note that `MM16` means different things in MICS and the DHS: in MICS6 it is
#' "Is (name) still alive?", while in DHS phase 7 and later `mm16` is "died of
#' violence or an accident". [prep_mics_sib_histories] refuses to run a varmap
#' that maps `mm16` to `sib.died.accident`.
#'
#' Variable names are lowercase; see [prep_mics_sib_histories] and its
#' `lowercase` argument.
#'
#' @format A data frame with columns `orig.varname`, `new.varname`, `sibvar`,
#'   `description` and `comments`.
#' @name sibhist_varmap_mics6
NULL

#' MICS sibling history variable map for MICS7
#'
#' Identical to [sibhist_varmap_mics6]; the MICS7 maternal mortality module is
#' unchanged from MICS6.
#'
#' @format A data frame with columns `orig.varname`, `new.varname`, `sibvar`,
#'   `description` and `comments`.
#' @name sibhist_varmap_mics7
NULL
