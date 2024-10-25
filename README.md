siblingsurvival
===============

An R package to help estimate adult death rates from sibling history data.

This package accompanies [our article](https://arxiv.org/abs/1906.12000).

<!--

TODO 2024 (mostly for maternal analysis)

- add DHS7 varmap
- ... and other DHS varmaps

- note from maternal analysis code: we should handle the case where some variables are optional (since not all countries ask all questions); example. literacy variable

- look at `helper_long.R` -> process_file, etc

- add vignette for maternal analysis

- also add a function that calculates individual-level visibilities
  - and maybe xi?

FNS TO ADD

- get ego age distn (for standardizing aggregate MM estimate)
- get visibility for each reported sib
- fn that adds sib maternal death date recoding, based on either
  maternal death or preg-related death (depending on DHS version, I think)
- calculate overall avg using age distn + age-specific rates

I WONDER IF WE COULD HAVE A 2-STEP PROCESS
  - 1st, get death, age, etc for all deaths
  - then, add an indicator for whether or not death was maternal



ALSO, FOR OTHER STUFF

- make sure code works fairly well w/ non-DHS data 
  - Brazil
  - Matlab
  - Socsim

  

-->

<!--
TODO

- handle discretizing exposure
- maybe handle including ego in reports?
- add unit tests

TODO (VIGNETTES)

- producing estimates (mostly done)
- making a sibling dataset from a survey dataset (mostly done)
- IC checks (now in 'producing estimates', but maybe make a separate vignette?)

WISH LIST

- it would be nice to have a Shiny widget to help conduct sensitivity analyses
- we could always use more unit tests
- additional varmaps (right now, we only have DHS6); an up-to-date list of DHS versions and recodes is available [here](https://www.dhsprogram.com/publications/publication-dhsg4-dhs-questionnaires-and-manuals.cfm)

-->
