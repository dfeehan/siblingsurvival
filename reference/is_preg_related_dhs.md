# is each sibling's death pregnancy-related, by DHS coding?

The DHS records this in one coded item, `sib.died.pregnant` (`mm9`):

## Usage

``` r
is_preg_related_dhs(sib_df, na.action = NULL, prmr.accident.recode = FALSE)
```

## Arguments

- sib_df:

  the prepped sibling dataset

- na.action:

  retained for symmetry with
  [is_maternal_dhs](http://dennisfeehan.org/siblingsurvival/reference/is_maternal_dhs.md)
  and ignored here. It used to decide how a missing `mm12` was treated;
  `mm12` is no longer consulted, so it has no effect on this column

- prmr.accident.recode:

  apply the documented-but-unexecuted 2016 PRMR rule, which drops a
  death during pregnancy (`mm9 = 2`) that is reported as violence or an
  accident. Default `FALSE`, matching the reference implementation. See
  Details

## Value

a logical vector, one entry per row of `sib_df`

## Details

|  |  |  |  |
|----|----|----|----|
| `mm9` | meaning | pregnancy-related | maternal |
| 2 | died while pregnant | yes | yes |
| 3 | died during delivery | yes | yes |
| 4 | since delivery – never assigned in practice | yes | yes |
| 5 | within six weeks of a delivery | yes | yes |
| 6 | between six weeks and two months of a delivery | **yes** | no |

Code 6 is what makes this the **two-month** quantity rather than a
42-day one, and it is exactly the line The DHS Program draws between
this and
[is_maternal_dhs](http://dennisfeehan.org/siblingsurvival/reference/is_maternal_dhs.md),
which stops at code 5.

No other condition is applied. In particular the time-since-delivery
band `mm12` is *not* used: the reference implementation
(`DHS-Indicators-Stata`, `Chap16_AM/AM_rates.do:725`) counts
`mm9 >= 2 & mm9 <= 6` and nothing else, and its header states plainly
that "mm12 is not needed" – it is dropped before the roster is reshaped.

## Changed in this version

This function previously required `mm9` to be 2, 3, 4 or 5 **and**
`mm12` to fall in the band `100`–`141` (0–41 days) or be `997`/`998`.
Both conditions were wrong:

- excluding code 6 dropped postpartum deaths that the function's own
  documentation described as in scope, and

- the `mm12` band imposed a 42-day cut on a quantity defined over two
  months, and applied a *postpartum* timing test even to deaths that
  occurred during pregnancy or delivery.

**This changes DHS results, in some surveys substantially.** Which code
a survey uses for postpartum deaths is a property of its questionnaire,
so the old behaviour lost anywhere from none to about 48% of
pregnancy-related deaths depending on the survey. On Rwanda 2010 it gave
51.2 deaths against a published 91; it now gives 90.7. See the DHS
validation plan in the package repository.

## The 2016 PRMR redefinition

`AM_rates.do:307-320` carries, **inside a comment block and therefore
never executed**, a rule introduced as "Important for redefinition of
Pregnancy Related Mortality Ratio (PRMR) in surveys from 2016 onwards":

    If mm9=2, and mm16=1 or 2, recode mm9 to 1

That is: a death *during pregnancy* that is reported as due to violence
or an accident stops counting as pregnancy-related. The DHS Program
documents the rule (see `blog.dhsprogram.com/mmr-prmr/`) but the code it
ships does not apply it, so published figures produced with that code do
not reflect it either.

`prmr.accident.recode = TRUE` applies it. The default is `FALSE`, which
is what the shipped reference code does and therefore what reproduces
published tables. It can only bite on the five surveys that carry `mm16`
at all.
