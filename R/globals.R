# Suppress R CMD check notes about non-standard evaluation variable bindings.
# These variables are used as column names in dplyr/data.table operations.
utils::globalVariables(c(
  ".", ":=",
  ".age.offset", ".agecat", ".ego.id", ".ego.weight", ".end.obs", ".event",
  ".id", ".ind_vis_weight", ".misscount", ".sib.id", ".sib.in.F", ".sib.sex",
  ".start.obs", ".time.offset", ".weight", ".y.F",
  "N.Falpha", "N.Fminusalpha",
  "adj.factor", "adj.factor.agespec", "adj.factor.allage", "adj.factor.meanagespec",
  "age", "age.cat", "agegroup", "agegrp_prop", "agelabel", "agg.est",
  "asdr.hat", "asdr.hat.agg", "asdr.hat.ind",
  "boot_idx", "cell", "denom.hat", "dummy",
  "ind.denom.ego", "ind.est", "ind.num.ego", "ind_vis",
  "normalized_diff", "num.hat", "occ", "qty",
  "ratio.agg.ind", "ratio.ind.agg", "rawqty",
  "sex", "sib.age", "sib.alive", "sib.death.date", "sib.died.accident",
  "sib.died.pregnant", "sib.exp", "sib.maternal.death.date", "sib.occ",
  "sib.preg_related.death.date", "sib.sex", "sib.size", "sib.time.delivery.death",
  "sibhist_varmap_dhs6", "sibvar",
  "time.period", "value",
  "wwgt", "y.Dcell", "y.Dcell.ind", "y.F",
  "y.Falpha.Fminusalpha", "y.Fminusalpha.Falpha",
  "y.NandFcell", "y.Ncell", "y.Ncell.ind"
))
