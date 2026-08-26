# Suppress R CMD check notes about non-standard evaluation variable bindings.
# These variables are used as column names in dplyr operations.
utils::globalVariables(c(
  ":=", ".", ".ego.id", ".ego.weight", ".misscount", ".sib.id", ".sib.in.F",
  ".sib.sex", "age", "age.cat", "agegrp_prop", "agelabel", "agg.est", "asdr.hat",
  "asdr.hat.agg", "asdr.hat.ind", "boot_idx", "caseid", "doi", "dummy", "ind_vis",
  "ind.est", "new.varname", "orig.varname", "ratio.agg.ind", "ratio.ind.agg", "sex",
  "sib.age", "sib.alive", "sib.death.date", "sib.died.accident", "sib.died.pregnant",
  "sib.dob", "sib.exp", "sib.maternal.death.date", "sib.occ",
  "sib.preg_related.death.date", "sib.sex", "sibhist_varmap_dhs6",
  "sibhist_varmap_mics6", "sibvar", "time.period", "total", "value", "wwgt", "y.F.bar"
))
