# MICS Maternal & Adult Mortality: Methods Reference

Compiled 2026-08-20. Every claim below is sourced; items that could not be verified are flagged
explicitly. Two official documents — the **MICS6 Tabulation Plan** and the **Standard SPSS
Syntax** — are Cloudflare-blocked and absent from the Wayback Machine, so the estimation
algorithm below is reconstructed from report methodology text, table footnotes, and the IUSSP
method chapter, then **verified arithmetically against published tables**. It has not been checked
against UNICEF's actual code.

---

## 1. Two different instruments, often confused

MICS has run *two structurally different* mortality modules under the same "MM" label.

| | **MICS2 / MICS3** (2000–2006) | **MICS4 / MICS5 / MICS6 / MICS7** (2010–) |
|---|---|---|
| Method | Indirect (summary) sisterhood | Direct sibling survival history |
| Questionnaire | **Household** questionnaire | **Individual Women** questionnaire |
| Respondents | **All adults 15+, both sexes**, proxy allowed | Women 15–49 |
| Data file | inside `hl.sav` | dedicated **`mm.sav`**, one row per sibling |
| Brothers enumerated? | **No** — sisters only | **Yes** — full both-sex roster |
| Time reference | ~10–14 years before survey | **7 years** before survey |
| Death definition | pregnancy-related, 6 weeks | 2 months (MICS4/5) → **42 days, excl. accidents/violence** (MICS6/7) |
| Supports adult mortality? | No | **Yes** — ₅m_x and ₃₅q₁₅ by sex |

The MICS4/5/6 file is a genuine both-sex sibling survival history — the same analytic surface as
DHS, usable for all-cause adult mortality by sex, not just maternal.

---

## 2. Module status: optional

MM is **not a core module**. In MICS7 it appears in the *Complementary Questionnaire Topics*
table (not the Base Questionnaire) and ships as a standalone add-on form. In MICS6 it is printed
inline in the model women's questionnaire at pp. 40–42, which is why it can look core if you only
read the template — but only a minority of countries fielded it.

**MICS6 women's questionnaire module order:** WM → WB → MT → CM → BH → DB → MN → PN → CP →
UN → FG → DV → VT → MA → AF → SB → HA → **MM (pp. 40–42)** → TA → LS.

The MICS6 "Questionnaires and Modules" document that would state core/optional in so many words
is Cloudflare-403 and not in Wayback. The above is inferred from the MICS7 topics table plus
empirical presence/absence of `mm.sav` across MICS6 surveys. In MICS3, Appendix Two sets core
modules in CAPITALS; Maternal Mortality is in mixed case under the Household Questionnaire.

---

## 3. Verbatim MICS6 MM module

Source of record: **Zimbabwe MICS 2019 Questionnaire for Individual Women**, Appendix E
pp. 495–500 ([PDF](https://mics.ipums.org/mics/resources/enum_materials_pdf/survey_form_zw2019a_wm.pdf)),
cross-checked against the IPUMS transcription of the MICS6 model questionnaire
([XML](https://mics.ipums.org/mics-action/source_documents/survey_form_mics6_wm.xml)) and the
official MICS7 standalone form
([.docx](https://mics.ipums.org/mics/resources/enum_materials_pdf/survey_form_maternal_mortality.docx)).
Italics are interviewer instructions; `→` is the skip column.

### Free-listing and completeness probes (asked once per woman)

> **MM1.** Now I would like to ask you some questions about your brothers and sisters born to your
> natural mother, including those who are living with you, those living elsewhere and those who
> have died. From our experience in prior surveys, we know it may sometimes be difficult to
> establish a complete list of all the children born to your natural mother. We will work together
> to draw the most complete list and work to recall all your siblings. Could you please now give me
> the names of all of your brothers and sisters born to your natural mother?
> *List all names on lines [A] to [H] below. Do not fill in the order number yet. If more than 8
> siblings, use additional questionnaires.*

> **MM2.** *Check MM1: How many siblings?* — NO SIBLINGS 1 → MM4 · ONE OR MORE 2

> **MM3.** *Read the names of the brothers and sisters to the respondent. After the last one, ask:*
> Are there any other brothers and sisters from the same mother that you have not mentioned?
> YES 1 → *Record in MM1* · NO 2

> **MM4.** Sometimes people forget to mention children born to their natural mother because they do
> not live with them or they do not see them very often. Are there any brothers or sisters who do
> not live with you that you have not mentioned? YES 1 → *Record in MM1* · NO 2

> **MM5.** Sometimes people forget to mention children born to their natural mother because they
> have died. Are there any brothers or sisters who died that you have not mentioned?
> YES 1 → *Record in MM1* · NO 2

> **MM6.** Some people have brothers or sisters from the same mother but a different father. Are
> there any brothers or sisters born to your natural mother, but who have a different natural
> father, that you have not mentioned? YES 1 → *Record in MM1* · NO 2

> **MM7.** *Count the number of siblings listed in MM1.* SUM __ __

> **MM8.** Just to make sure that I have this right: Your natural mother had (total in MM7) live
> births, excluding you, during her lifetime. Is that correct? YES 1 → MM10 · NO 2

> **MM9.** *Probe and check sum in MM7 and list in MM1. Make corrections as necessary until response
> in MM8 is 'Yes'.*

> **MM10.** *Check MM7: How many siblings?* — NO SIBLINGS 1 → End · ONE OR MORE 2

> **MM11.** Please tell me, which brother or sister was born first? And which was born next?
> *Record '01' for the order number in MM1 for the first-born, '02' for the second, and so on.*

> **MM12.** How many of these births did your mother have before you were born? __ __

> **MM13.** *Write down the names of the brothers and sisters in MM14 according to the order number
> in MM1. Ask MM15 to MM27 for one brother or sister at a time (vertically).*

### Per-sibling roster

Columns headed **[S1] FIRST-BORN … [S8] EIGTH** *(typo is in the original)*.

> **MM14.** *Copy name of individual siblings to individual columns.*
> **MM15.** Is (name) male or female? — MALE 1 · FEMALE 2
> **MM16.** Is (name) still alive? — YES 1 · NO 2 → MM18 · DK 8 → MM28
> **MM17.** How old is (name)? __ __ → MM28
> **MM18.** How many years ago did (name) die? __ __
> **MM19.** How old was (name) when (he/she) died? __ __
> **MM20.** *Check MM15: Was the sibling male?* — YES 1 → MM26 · NO 2
> **MM21.** *Check MM19: Did the sister die before age 12 years?* — YES 1 → MM26 · NO 2
> **MM22.** Was (name) pregnant when she died? — YES 1 → MM26 · NO 2
> **MM23.** Did (name) die during childbirth? — YES 1 → **MM28** · NO 2
> **MM24.** Did (name) die within two months after the end of a pregnancy or childbirth? — YES 1 · NO 2 → MM26
> **MM25.** How many days after the end of the pregnancy or childbirth did (name) die? __ __
> **MM26.** Was (name)'s death due to an act of violence? — YES 1 → MM28 · NO 2
> **MM27.** Was (name)'s death due to an accident? — YES 1 · NO 2
> **MM28.** *Check MM14: Is there a younger sibling?* — YES 1 → next column · NO 2 → End

### Skip logic, compactly

```
MM16 = DK  → jump to next sibling; NO age and NO date info at all
MM17       living siblings only        MM18/MM19  dead siblings only
MM20 male  → MM26   (not out of module)
MM21 <12   → MM26   (not out of module)
MM22 preg  → MM26
MM23 childbirth = YES → MM28   ← MM26/MM27 NEVER ASKED
MM24 = NO  → MM26 ;  MM24 = YES → MM25
```

Three gotchas:

1. **MM26/MM27 are asked of *all* deceased siblings — male, and sisters who died before 12.**
   Cause-of-death screening is not female-only.
2. **A childbirth death (MM23=1) skips MM26/MM27 entirely.** Your recode must not treat missing
   MM26/MM27 as grounds for exclusion in those cases.
3. **No DK code on MM17, MM18, MM19, MM25.** MM21 is keyed on MM19, so a sister with missing age at
   death cannot be routed correctly.

### MICS3 summary sisterhood (verbatim)

From MICS3 Appendix Two, p. A2.66
([Wayback copy of childinfo.org PDF](https://web.archive.org/web/20100622030948id_/http://www.childinfo.org/files/MICS3_Appendix_2_-_Questionnaires_060306.pdf)).
Administered in the **Household** questionnaire to each adult 15+, proxy allowed (MM3 flags proxy,
MM4 gives the proxy's line number).

> **MM5.** How many sisters (born to the same mother) have you ever had? *98 = DK*
> **MM6.** How many of these sisters ever reached age 15? *98 = DK*
> **MM7.** How many of these sisters (who are at least 15 years old) are alive now? *98 = DK*
> **MM8.** How many of these sisters who reached age 15 or more have died? *98 = DK*
> **MM9.** How many of these dead sisters died while pregnant, or during childbirth, or during the
> six weeks after the end of pregnancy? *98 = DK*

No age at death, no time since death, no cause probe, no live-birth count. Six-week window, not
two months. Sisters only.

### MICS4 / MICS5 (intermediate design)

Women's questionnaire, MM1–MM14, per-sibling roster: MM4 name, MM5 sex, MM6 alive, MM7 age,
MM8 years since death, MM9 age at death, MM10 pregnant when died, MM11 died during childbirth,
MM12 died within two months, **MM13 "How many live born children did (name) give birth to during
her lifetime?"** (dropped in MICS6). MICS5 adds check MM9A. MICS6 is a substantial redesign:
full free-listing with five completeness probes, birth-order reconstruction, violence/accident probes.

---

## 4. Variables in `mm.sav` and `wm.sav`

MICS6 splits the module across two files. Verified against Zimbabwe MICS 2019
([`mm.sav` dictionary](https://microdata.worldbank.org/index.php/catalog/4180/data-dictionary/F6?file_name=mm.sav))
and Pakistan Sindh MICS 2018-19
([catalog 4181](https://microdata.worldbank.org/index.php/catalog/4181/data-dictionary/F6)).

**`wm.sav` (woman-level):** `MM3`, `MM4`, `MM5`, `MM6`, `MM7` (number listed), `MM8`
(confirmation), `MM12` (preceding births). MM1, MM2, MM9, MM10, MM11, MM13 are not retained —
name fields, interviewer checks, or instructions.

**`mm.sav` (sibling-level; Zimbabwe 2019 = 47,835 rows × 48 vars):**

| Var | Content |
|---|---|
| `HH1`, `HH2`, `LN` | cluster / household / **respondent woman's** line no. |
| `MMLN` | sibling's roster position (the name is dropped) |
| `WM1`,`WM2`,`WM3`,`WMINT` | link keys to `wm.sav` |
| `MM15` | sibling's sex |
| `MM16` | still alive |
| `MM17` | current age (living) |
| `MM18` | years since death |
| `MM19` | age at death |
| `MM22`–`MM25` | pregnant / childbirth / within 2 months / days after |
| `MM26`, `MM27` | violence / accident |
| `MM17C` | **imputed CMC date of birth** (constructed) |
| `MM18C` | **imputed CMC date of death** (constructed) |
| `WDOI`, `WDOB` | CMC interview date, CMC woman's DOB |
| `wmweight`, `psu`, `stratum` | design |
| background | `HH3`,`HH4`,`HH6`,`HH7`, `welevel`, `disability`, `insurance`, `MSTATUS`, `CEB`, `CSURV`, `CDEAD`, `wscore`/`windex5`/`windex10`, `religion`, `dvhweight` |

**`MM20` and `MM21` are not in the data** — interviewer check items. Country customisation exists:
Pakistan Sindh adds `MM22A` "Was deceased sister ever married" and drops some background vars
(42 vars total); Pakistan's file does **not** carry `MM17C`/`MM18C`.

---

## 5. Published indicators

**`TM.21` — "Maternal mortality ratio"** is the only numbered maternal mortality indicator in
MICS6/MICS7. Verbatim from *MICS7 Indicators and Definitions v7.3*
([Wayback](https://web.archive.org/web/20250828215542id_/https://mics.unicef.org/sites/mics/files/2025-05/MICS7%20Indicators%20and%20Definitions%20v7.3.docx)):

> `TM.21 | Maternal mortality ratio | 3.1.1 | MM | Deaths during pregnancy, childbirth, or within
> 42 days of termination excluding accidents and acts of violence, per 100,000 live births | TM.9.3`

There are **no companion numbered indicators** — the only other mortality indicators in the whole
MICS7 list are `CS.1`–`CS.5` (neonatal through under-five). But the tables publish more:

| Quantity | Table | Indicator no. |
|---|---|---|
| Maternal mortality **ratio** (/100,000 live births) | TM.9.3 | **TM.21** |
| Maternal mortality **rate** (/1,000 woman-years) | TM.9.3 | — |
| **PM / PMDF** (% of female deaths that are maternal) | TM.9.3 | — |
| General fertility rate (7-year) | TM.9.3 | — |
| Lifetime risk of maternal death | TM.9.3 | — |
| Adult mortality rates ₅m_x, **by sex** | TM.9.1 | — |
| Adult mortality probability ₃₅q₁₅, **by sex** | TM.9.2 | — |

**Units inconsistency inside MICS6 reports:** a footnote defines the maternal mortality rate "per
100,000 women age 15–49", but table footnote B says the printed column is "per **1,000** woman-years
of exposure". The printed numbers are per 1,000 — verified arithmetically in three countries.

**Older numbering.** MICS5: Zimbabwe 2014 labels it "MICS indicator 5.13; MDG indicator 5.1" in the
body but "1.6" in its own Appendix G — an internal contradiction; cite MICS5 numbers carefully.
MICS3: "MICS indicator 3; MDG indicator 16".

---

## 6. The estimator

### Direct method (MICS4/5/6/7)

Let `x` index 5-year age groups 15–19 … 45–49 over the 7-year window:

```
5MDx   = maternal deaths of sisters                (numerator, see §7)
5Dx^s  = all deaths of sisters
5PYx^s = sister person-years of exposure (living AND dead siblings)
5Nx^f  = age-standard weights (see caveat)
5fx    = ASFR from the birth history, SAME 7-year window

rate_x = 5MDx / 5PYx^s                                    (printed per 1,000)
PM_x   = 5MDx / 5Dx^s                                     (printed as %)

MMRate = Σ (rate_x · 5Nx^f) / Σ 5Nx^f                     age-standardised
GFR    = Σ (5fx  · 5Nx^f) / Σ 5Nx^f                       age-standardised
MMR    = 100,000 × MMRate / GFR                           ← footnote E
LTR    = 1 − (1 − MMR)^TFR                                ← footnote F, MMR as proportion
```

**The live-births denominator is a GFR from the women's birth history over the same 7-year window,
applied to the same age standard** — not a raw birth count and not the sibling person-years.
Classic Rutenberg–Sullivan / IUSSP construction.

Numerically verified:

- Zimbabwe 2019: `0.59/1000 ÷ 128/1000 × 100,000 = 461` vs published **462**
- Iraq 2018: `0.13 ÷ 128 × 100,000 = 102` vs published **104**
- Punjab 2017-18: `0.23 ÷ 126 × 100,000 = 183` vs published **180**

The TFR in the lifetime-risk formula is the **7-year** TFR, not the headline 3-year TFR
(back-solving gives ≈4.14 for Zimbabwe vs headline 3.86). Medium-high confidence.

**Age-standard weights — unresolved ambiguity.** MICS text says the rate is "standardised by the
age distribution of *the survey respondents*". The IUSSP chapter MICS is implementing says weight by
"the number of women aged 15–49 *in the households surveyed*". MMEIG says "the female population of
*respondent households*". These differ (interviewed women vs. all household women 15–49). **Try both
when replicating.** Medium confidence that MICS uses interviewed women, because that is what the
text literally says.

**No sibship-size reweighting.** MICS follows the DHS convention: reported siblings only,
respondent excluded, weighted only by the respondent's sample weight. No Gakidou–King correction,
no adjustment for sibships with zero surviving members. Age-standardisation is used specifically
"to remove the effect of truncation bias (the upper boundary for eligibility is 49 years)".

### Indirect method (MICS2/MICS3)

```
sister units of exposure = Σ_a (sisters reaching 15, reported by respondents aged a) × AF_a
LTR = maternal deaths / sister units of exposure
MMR = 1 − (1 − LTR)^(1/TFR)
```

Graham–Brass–Snow adjustment factors, as printed in Sierra Leone 2005 and Somalia 2006 (identical):

| Age | 15–19 | 20–24 | 25–29 | 30–34 | 35–39 | 40–44 | 45–49 | 50–54 | 55–59 | 60+ |
|---|---|---|---|---|---|---|---|---|---|---|
| AF | .107 | .206 | .343 | .503 | .664 | .802 | .900 | .958 | .986 | 1.000 |

Verified: Sierra Leone 2005 — `1066/36,335 = 0.02934`; `1−(1−0.02934)^(1/6.50) = 457.1` = published
**457**. Somalia 2006 — `1629.3/24,715.1 = 0.06592` → **1044.5** = published **1044**.

Note the AF rows above 49 — the module was administered to all adults 15+, so the effective
time-location is pushed *further* back than the textbook ~12 years for a 15–49 respondent sample.
TFR is an **externally supplied scalar**, not estimated from the survey (Somalia: "A total fertility
rate of 6.5 was used"; Sierra Leone column header: "Total fertility rate **10–14 years ago**").

**⚠️ Unexplained step.** Both reports include a column "Sisters who reached age 15 *(adjusted)*"
in which, **for respondents under 30 only**, the figure is inflated ~2.6× (Sierra Leone: 4,818 →
11,655 at ages 15–19). No retrieved source documents this adjustment. Low confidence on its basis —
you will need to reverse-engineer it.

---

## 7. Reference period

**Direct method: 7 years, uniformly.** Verbatim (Zimbabwe 2019; identical in Iraq 2018 and Punjab):

> "Table TM.9.3 presents direct estimates of maternal mortality for the **seven-year period prior to
> the survey**. This period of time was chosen **to reduce possible heaping of reported years since
> death on five-year intervals**."

Adult mortality tables TM.9.1/TM.9.2 use the same window.

**Boundary undocumented.** No MICS document states whether "7 years" means years-since-death 0–6 or
1–7. The IUSSP chapter says DHS uses "seven (0 to 6) years"; Zimbabwe 2014 glosses its window as
"roughly between 2007 and 2014" for a mid-2014 survey, consistent with 0–6. Medium confidence it is
**0–6 completed years** (MM18 ∈ {0…6}, or CMC exposure in the 84 months before interview). The
presence of `MM17C`/`MM18C`/`WDOI` implies CMC-based exposure allocation rather than integer-year
binning — test both.

**MICS5 also published a 5-year table.** Zimbabwe 2014: RH.20A (7 yr) = **614**, RH.20B (5 yr) =
**581**, with 614 designated the indicator. MICS6 dropped the 5-year table. (Zimbabwe 2014's
Appendix G nonetheless defines the indicator over "the 5-year period" — another internal
contradiction.)

**Indirect method: ~10–14 years back.** No MICS3 report states a reference period in prose, but
Sierra Leone MICS3's table column is headed "Total fertility rate **10–14 years ago**", so MICS3
deliberately paired the LTR with a decade-lagged fertility level. The "~10–12 years" figure is the
standard sisterhood-literature result (Graham, Brass & Snow 1989, cited in the Somalia table), not
a MICS assertion.

---

## 8. Maternal vs. pregnancy-related — MICS6 broke with DHS

**MICS6/7 report TRUE MATERNAL DEATHS. This is a deliberate departure from DHS and from MICS5.**

Table TM.9.3 footnote A, verbatim (identical across Zimbabwe 2019, Iraq 2018, Punjab 2017-18):

> "A maternal death is defined as the death of a woman while pregnant or within 42 days of
> termination of pregnancy, **from any cause except accidents or violence**"

The transition is documented in a MICS6 footnote:

> "Please note that **42 days is a measure recently adopted as per the SDG indicator definition.
> Previously, the indicator of maternal mortality ratio was defined as any death during pregnancy or
> within two months of delivery or termination. This previously employed measure is now labelled
> 'Pregnancy-related maternal mortality ratio' and can be calculated for direct comparison.**"

Contrast MICS5 (Zimbabwe 2014): "Pregnancy-related deaths are defined as any death that occurred
during pregnancy, childbirth, or within two months after the birth or termination of a pregnancy…
**even if the death is due to causes that are accidental or incidental**."

### Numerator recode

```r
maternal_death <- female &
  MM19 >= 12 &
  ( MM22 == 1 |                      # pregnant when died
    MM23 == 1 |                      # died during childbirth
    (MM24 == 1 & MM25 <= 42) ) &     # within 42 days postpartum
  !(MM26 %in% 1 | MM27 %in% 1)       # NOT violence / accident
```

Two subtleties:

1. **MM23 = 1 skips to MM28 — MM26/MM27 are never asked for childbirth deaths.** A childbirth death
   is unconditionally maternal. Do not let `NA` on MM26/MM27 exclude it. (`%in%` above handles this;
   `==` would propagate NA.)
2. **MM25 is only asked when MM24 = 1**, so the 42-day cut bites only on that branch. Deaths with
   MM24=1 and MM25 in 43–60 days *are collected* and excluded under MICS6 — which is exactly what
   lets you reconstruct the old 2-month "pregnancy-related" series for backward comparability.

**⚠️ Residual sloppiness:** the narrative text in MICS6 reports still says "Age-specific mortality
rates are calculated by dividing the number of **pregnancy-related deaths** by years of exposure"
(Zimbabwe 2019 *and* Iraq 2018) while the table footnote says accidents and violence are excluded.
The **table footnote and indicator definition are authoritative**; the sentence is un-updated MICS5
boilerplate.

**WHO's own metadata is out of date on this.** GHO indicator IMR 26 still asserts "the sisterhood
method used in DHS and MICS results in *pregnancy-related mortality*: regardless of cause of death,
all deaths occurring during pregnancy, birth, or the six weeks following…" That is no longer true
for MICS6+, and is worth flagging in any paper.

---

## 9. Confidence intervals

Verbatim from the sampling-error appendix (same wording MICS5 and MICS6):

> "For survey indicators that are means, proportions or ratios, the **Taylor series linearization**
> method is used… For more complex statistics, such as fertility and mortality rates, the
> **Jackknife repeated replication method** is used."
> "For MICS results **95% confidence intervals** are used… plus or minus **two times the standard
> error** (r + 2·se or r − 2·se)."
> "programs developed in **CSPro Version 6.3 and SPSS Version 24 Complex Samples** module."

So: jackknife for MMR, ±2·se (not ±1.96), computed in CSPro. `deff`/`deft` print as "na" for MMR,
consistent with jackknife. The replicate-dropping unit is not spelled out; medium confidence it is
cluster-level delete-one.

| Survey | MMR | se | CV | 95% CI |
|---|---|---|---|---|
| Zimbabwe 2019 (SE.1) | 413.637 ⚠️ | 63.599 | 0.154 | 288.98 – 538.29 |
| Zimbabwe 2014 (MICS5) | 614 | 54 | 0.088 | 506 – 722 |
| Iraq 2018 (SE.1) | 104 | 13.684 | 0.132 | 77 – 131 |
| Punjab 2017-18 | — | — | — | **none published** |

**Two Zimbabwe 2019 anomalies, flagged not resolved:**

1. The SE table's MMR (413.637) **does not match the report's headline MMR (462)** from TM.9.3 and
   the indicator list. Iraq's SE table *does* match its TM.9.3, so this is Zimbabwe-specific.
2. The printed CI is not `r ± 2·se` with the printed se: `413.637 ± 2(63.599) = 286.44–541.24`, but
   the table prints 288.98–538.29, implying se = 62.327.

**Punjab publishes a point estimate with no CI** — do not assume MICS6 always supplies uncertainty.

---

## 10. Adult mortality — yes, MICS publishes it

**Table TM.9.1 "Adult mortality rates"** — deaths, exposure years, and ₅m_x per 1,000 for **females
and males separately**, ages 15–19 … 45–49, plus an age-standardised 15–49 total, same 7-year
window. Verbatim: "Reported ages at death and years since death of the respondents' **brothers and
sisters** are used to construct the numerators."

**Table TM.9.2 "Adult mortality probabilities"** — ₃₅q₁₅ per 1,000 by sex:

> "nqx = (n · nmx) / (1 + (n − nax)·nmx)) … **nax is assumed to be 2.5 years for all 5-year age
> groups**. 35q15 = 1 − ((1 − 5q15)·(1 − 5q20)· … ·(1 − 5q45))"

Published ₃₅q₁₅: Zimbabwe 2019 **224 (W) / 219 (M)**; Iraq 2018 **49 / 86**; Punjab 2017-18
**75 / 85**. MICS5 had the same pair as RH.18/RH.19. Neither table carries an indicator number and
**neither appears in the sampling-error appendix — no CIs for adult mortality.**

---

## 11. Worked example: Zimbabwe MICS 2019

Table TM.9.3, direct estimates, 7 years preceding the survey:

| Age | % of female deaths maternal | Maternal deaths | Exposure (yrs) | Rate /1,000 |
|---|---|---|---|---|
| **15–49 (age-adj.)** | **10.0** | **68** | **108,985** | **0.59** |
| 15–19 | 10.0 | 3 | 15,313 | 0.23 |
| 20–24 | 20.8 | 10 | 18,198 | 0.56 |
| 25–29 | 11.3 | 9 | 21,240 | 0.41 |
| 30–34 | 12.9 | 19 | 21,434 | 0.87 |
| 35–39 | 9.1 | 15 | 16,044 | 0.91 |
| 40–44 | 8.4 | 10 | 10,479 | 0.96 |
| 45–49 | 2.4 | 2 | 6,277 | 0.30 |

GFR (age-adj., 7-yr) = **128** · **MMR (TM.21) = 462** per 100,000 live births · LTR = **0.019**
95% CI **288.98 – 538.29**, se 63.599, jackknife (with the caveats in §9).

**68 maternal deaths total**, so age-specific cells of 2–19 deaths — the age pattern is essentially
noise. IUSSP offers a diagnostic here: "the age pattern of the proportion of deaths that are
pregnancy-related should resemble the age distribution of age-specific fertility." Zimbabwe's PM by
age (10.0, 20.8, 11.3, 12.9, 9.1, 8.4, 2.4) is noisier than an ASFR schedule; Punjab's
(4.1, 22.8, 25.0, 16.1, 18.6, 8.5, 1.3) has an implausibly low 15–19 value.

**⚠️ Do not compare Zimbabwe 2014 (614) with 2019 (462).** 2014 is *pregnancy-related, 2-month
window, no cause exclusion*; 2019 is *maternal, 42 days, excluding violence/accidents*. Every press
comparison of "614 → 462" conflates the two.

**Data-quality tables:** DQ.7.1 — siblings 82.8% living / 17.1% dead / 0.1% missing; among dead
siblings **97.4% have both age at death and years since death**. DQ.7.2 — mean sibship size 4.9,
sex ratio at birth 0.99.

---

## 12. MMEIG use and critiques

**MMEIG uses MICS and re-derives from microdata.** *Trends in maternal mortality 2000 to 2023*:
"DHS and MICS surveys were searched and **microdata downloaded** from their respective websites."

Eligibility: "(i) sufficient methodological information … such as **standard errors**; and (ii)
information on the **age distribution of females residing in survey households**." (Punjab-style
reports with no published MMR se would fail (i) as published, though MMEIG works from microdata.)

Processing chain:

- Survey MMR → **converted to PM** using WPP 2024 live births and all-cause female deaths 15–49,
  "because it is less affected by unregistered deaths."
- PM "standardized according to the age distribution of the female population of **respondent
  households** … because the age distribution found when using the sisterhood method is different
  from that of the general population" (citing Wilmoth et al. 2012).
- **+10% upward adjustment** to all non-CRVS observations "to account for deaths early in pregnancy
  that might not have been captured."
- If the source uses the **pregnancy-related** definition, a **downward** adjustment: incidental/
  accidental deaths assumed to be "**10% of pregnancy-related deaths (excluding HIV-related deaths)
  in sub-Saharan African countries, and 15% in other low- and middle-income countries.**"
- Feeds **BMat** (covariates GDP PPP, GFR, SBA), with **BMis** for CRVS misclassification.

**⚠️ Unresolved:** whether MMEIG classifies MICS6-era observations as "maternal" (skipping the
10%/15% haircut) or still as "pregnancy-related". The report distinguishes the definitions but does
not tabulate which surveys fall in which bucket. **Applying the 10–15% deduction to a MICS6 MMR
would double-count an exclusion MICS already performs at the microdata level via MM26/MM27.**

**Method critiques** (from [IUSSP Tools for Demographic Estimation](https://demographicestimation.iussp.org/content/estimation-pregnancy-related-mortality-survival-siblings)):

> "It is widely believed that **sibling histories tend to under-report mortality, particularly deaths
> further in the past**. One should thus **not attempt to interpret trends over time in
> pregnancy-related mortality from a single data set**."

> "**No formal methods exist for carrying out such assessments**" of the proportion pregnancy-related.

On sibship-size reweighting, IUSSP surveys the Gakidou–King (2006) vs. Trussell–Rodriguez (1990) vs.
Obermeyer et al. (2010) vs. Masquelier (2012) dispute: Obermeyer et al. estimated the unadjusted
(DHS/MICS) approach can bias overall mortality **downward by ~20%**; Masquelier argues that is
exaggerated and recommends the DHS/MICS calculation. **MICS uses the unadjusted approach.**

**No publication specifically critiquing MICS (as distinct from DHS) maternal mortality estimation
was found.** Leads not read: DHS Methodological Report 13 (transfers, since the module is
near-identical); two Zimbabwe RAMOS triangulation papers (PMC9358939; BMC Public Health
10.1186/s12889-022-13321-7) — both reCAPTCHA-gated.

---

## 13. Sources

- Zimbabwe 2019 MICS Survey Findings Report — [washdata.org mirror](https://washdata.org/sites/default/files/documents/reports/2020-02/Zimbabwe%202019%20MICS%20Report.pdf) · [UNICEF copy](https://www.unicef.org/zimbabwe/media/2536/file/Zimbabwe%202019%20MICS%20Survey%20Findings%20Report-31012020_English.pdf)
- [Iraq 2018 MICS Survey Findings Report](https://washdata.org/sites/default/files/documents/reports/2019-05/Iraq%20MICS%202018%20SFR.pdf)
- [Pakistan Punjab MICS 2017-18 (World Bank Microdata cat. 3559)](https://microdata.worldbank.org/index.php/catalog/3559)
- [Zimbabwe 2014 MICS Final Report (MICS5), Wayback](https://web.archive.org/web/20170113043555id_/https://mics-surveys-prod.s3.amazonaws.com/MICS5/Eastern%20and%20Southern%20Africa/Zimbabwe/2014/Final/Zimbabwe%202014%20MICS_English.pdf)
- [MICS7 Indicators and Definitions v7.3, Wayback](https://web.archive.org/web/20250828215542id_/https://mics.unicef.org/sites/mics/files/2025-05/MICS7%20Indicators%20and%20Definitions%20v7.3.docx)
- [MICS7 Questionnaire Topics v7.3.2, Wayback](https://web.archive.org/web/20250922042144id_/https://mics.unicef.org/sites/mics/files/2025-09/MICS7%20Questionnaire%20Topics%20v7.3.2.docx)
- [MICS6 model women's questionnaire, IPUMS transcription](https://mics.ipums.org/mics-action/source_documents/survey_form_mics6_wm.xml)
- [Zimbabwe MICS 2019 women's questionnaire PDF](https://mics.ipums.org/mics/resources/enum_materials_pdf/survey_form_zw2019a_wm.pdf)
- [MICS7 standalone Maternal Mortality form (.docx)](https://mics.ipums.org/mics/resources/enum_materials_pdf/survey_form_maternal_mortality.docx)
- [MICS3 Appendix Two — Questionnaires, Wayback of childinfo.org](https://web.archive.org/web/20100622030948id_/http://www.childinfo.org/files/MICS3_Appendix_2_-_Questionnaires_060306.pdf)
- [Zimbabwe MICS 2019 `mm.sav` data dictionary](https://microdata.worldbank.org/index.php/catalog/4180/data-dictionary/F6?file_name=mm.sav)
- [Sierra Leone MICS3 2005 (WB cat. 50)](https://microdata.worldbank.org/index.php/catalog/50) · [Somalia MICS3 2006 (WB cat. 51)](https://microdata.worldbank.org/index.php/catalog/51)
- [Trends in maternal mortality 2000 to 2023 (MMEIG)](https://www.unfpa.org/sites/default/files/pub-pdf/9789240108462-eng.pdf)
- [WHO GHO indicator metadata: MMR (IMR 26)](https://www.who.int/data/gho/indicator-metadata-registry/imr-details/26)
- [IUSSP: Estimation of pregnancy-related mortality from survival of siblings](https://demographicestimation.iussp.org/content/estimation-pregnancy-related-mortality-survival-siblings)
- [DHS Methodological Report 13](https://dhsprogram.com/pubs/pdf/MR13/MR13.pdf) *(lead, not read)*
- [MICS tools page](https://mics.unicef.org/tools) *(Tabulation Plan and SPSS Syntax downloads 403)*

### Could not retrieve

`mics.unicef.org` file downloads are Cloudflare-403 from a non-browser client, and
`mics-surveys-prod.s3.amazonaws.com` returns AccessDenied. Specifically **not obtained**:
MICS6 Tabulation Plan (18 Mar 2019), MICS6 Indicators and Definitions (11 Mar 2019), MICS6/7
Standard SPSS Syntax `.zip` (Wayback replay returns HTTP 500), and
`MICS6_Questionnaires_Modules_20180604.docx`. Retrieving these would require driving a real
browser session — worth doing before you finalise a replication, since the SPSS syntax is the only
authoritative statement of the estimation algorithm.
