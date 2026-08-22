# MICS sibling history variable map for MICS4

A data frame mapping variable names used in the MICS4 maternal mortality
module (`mm.sav`) to the standardized names used by this package.

## Format

A data frame with columns `orig.varname`, `new.varname`, `sibvar`,
`description` and `comments`.

## Details

MICS4 and MICS5 share a roster numbering of `MM5`–`MM13`, which is
different from the `MM15`–`MM27` used from MICS6 onward. Neither MICS4
nor MICS5 asks about violence or accidents, so they support
**pregnancy-related**, not strictly maternal, mortality – the same
limitation as DHS phases 2–6.

Variable names are lowercase; see
[prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md)
and its `lowercase` argument.
