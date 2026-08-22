# MICS sibling history variable map for MICS6

A data frame mapping variable names used in the MICS6 maternal mortality
module (`mm.sav`) to the standardized names used by this package. This
is the default varmap for
[prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md).

## Format

A data frame with columns `orig.varname`, `new.varname`, `sibvar`,
`description` and `comments`.

## Details

MICS6 renumbered the roster to `MM15`–`MM27` and added `MM26` (violence)
and `MM27` (accident), so MICS6 supports **maternal** mortality as well
as pregnancy-related mortality.

Note that `MM16` means different things in MICS and the DHS: in MICS6 it
is "Is (name) still alive?", while in DHS phase 7 and later `mm16` is
"died of violence or an accident".
[prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md)
refuses to run a varmap that maps `mm16` to `sib.died.accident`.

Variable names are lowercase; see
[prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md)
and its `lowercase` argument.
