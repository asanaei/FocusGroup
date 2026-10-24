# Building `anes_2024_personas`

This folder builds the dataset `anes_2024_personas` that FocusGroup and
LLMRpanel ship: 100 respondents to the ANES 2024 Time Series Study, each
described by 17 demographic fields, 105 attitudes written as text, and an
`ideology_score`. One run writes identical copies into both packages. The
folder is excluded from the FocusGroup tarball (`^data-raw$` in
`.Rbuildignore`).

| File | Content |
|---|---|
| `make_personas.R` | The build: decoding, checks, selection, IRT model, output |
| `persona_spec.R` | One row per field (handle, variable, phrase, block, type, level, category order, off-scale codes, IRT flag); the employment recode table; the refusal rule; the fields dropped or replaced |
| `codebook.R` | Reads the codebook PDF through `pdftotext -raw` and finds each variable's block |
| `label_overrides.csv` | Full text of the six value labels the Stata file cuts at 80 characters, with codebook pages |
| `wording_overrides.csv` | Question wording for the 52 fields whose wording is not simply their own variable's question |

## Running

From the FocusGroup package root:

```sh
ANES_DTA=/path/anes_timeseries_2024_stata_20260519.dta \
ANES_CODEBOOK=/path/anes_timeseries_2024_userguidecodebook_20260519.pdf \
PANEL_ROOT=/path/to/LLMRpanel \
  Rscript data-raw/anes_2024_personas/make_personas.R
```

The build needs haven, mirt, dplyr, and pkgload, and the `pdftotext` program
from poppler (on the search path, or named by `PDFTOTEXT`). It loads
FocusGroup from the working tree to use `fg_default_na_strings()` and the
scale-endpoint rule of `create_agents_from_survey()`. It stops on the first
failed check and writes nothing in that case. A successful run writes
`data/anes_2024_personas.rda` (xz, version 3) into both packages, reloads both
and checks that they are identical, and writes the fingerprint
`tests/testthat/fixtures/anes_2024_personas.md5` into both test folders. The
build is deterministic; two runs gave the same file.

Inputs of the shipped build (the Full Release of May 19, 2026):

| File | md5 |
|---|---|
| `anes_timeseries_2024_stata_20260519.dta` | `bb46cf5a94e984e87e53fcef19fe691b` |
| `anes_timeseries_2024_userguidecodebook_20260519.pdf` | `0bdf8a553085dbb5610e7008fc910a0e` |

Outputs: `anes_2024_personas.rda` of 24,980 bytes (md5
`8b5afa1db446dd487354748dc343a633` under R 4.4.3), and the fingerprint
`23e4f84f05277bdb13271d1c95b6350e`, the md5 of a plain-text dump of the data,
dictionary, and attributes other than `build`. The tests check the
fingerprint, which does not depend on how R serializes the file.

Citation, as printed on p. 5 of the User Guide and stored in both
`attr(x, "source")` and `attr(x, "citation")`:

> American National Election Studies. 2026. ANES 2024 Time Series Study Full
> Release [dataset and documentation]. May 19, 2026 version.
> www.electionstudies.org

## How values are decoded

- **Missing by code.** A negative code is `NA`; a thermometer rating outside
  0-100 is `NA` (998 "Don't know (where to rate)", 999 "Don't recognize", and
  one rating of 200 on the police thermometer). The build checks that every
  negative-code label of the 123 variables read matches
  `fg_default_na_strings()` and that no positive label does, the thermometers'
  codes above 100 aside. No list of label patterns decides what is missing.
- **"Haven't thought much about this"** (code 99) is an answer and is shown;
  it is left out of the distance and the IRT model.
- **Labels.** The code prefix (`^\s*-?[0-9]+\.?\s+`) and the notes "{VOL}"
  and "{SPECIFY}" are removed; parentheticals are kept, so the nine
  categories of V241075x and V241076x ("Democratic candidate selected
  (intent to vote)") stay distinct. Value labels are transliterated to ASCII.
- **80-character labels.** Stata stores at most 80 characters of a value
  label. Every positive-code label of exactly 80 characters needs a row in
  `label_overrides.csv`, and the build checks each row: the label in the file
  is a prefix of the full text, and "N. full text" occurs in that variable's
  codebook block after its lines are joined and whitespace is normalized, on
  the page given. The six verified labels:
  - V241386, p. 164: 1 "Make all unauthorized immigrants felons and send
    them back to their home country"; 2 "Have guest worker program that allows
    unauthorized immigrants to remain in the US to work but only for a limited
    amount of time"; 3 "Allow unauthorized immigrants to remain in US &
    qualify for citizenship if they meet certain requirements"; 4 "Allow
    unauthorized immigrants to remain in US & qualify for citizenship without
    penalties".
  - V241488x, p. 205: 1 "R working now (if also retired, disabled, homemaker
    or student, working 20 or more hrs/wk)"; 7 "R homemaker (if also working,
    working <20 hrs/wk/incl nonworkg rs both homemaker and student)".
- **Employment status** (V241488x) is written from the recode table in
  `persona_spec.R`, whose `anes_label` column the build compares with the
  full labels. The shown text drops the "R " prefix; for code 7 it also
  spells out "incl nonworkg rs" as "includes nonworking respondents who are".
- **Endpoint-only scales.** V241239, V241258, V241245, V241248, V241255, and
  V241242 label only 1 and 7. Every value 1-7 is written as "5 on a scale from
  1 (Government should provide many fewer services) to 7 (Government should
  provide many more services)", endpoints included. The endpoints are found by
  FocusGroup's `.fg_scale_endpoints()`, and the build stops unless they are 1
  and 7 for every middle value.
- **Thermometers** are written as "70 on a 0-100 feeling thermometer".
- **Age** (V241458x) in six bands, 18-24 to 65+ (80 means 80 or older);
  **children in household** (V241521) as Yes or No.
- **Religious attendance.** V241440 is asked only of people who attend
  (universe "IF R ATTENDS RELIGIOUS SERVICES:", p. 185). Respondents with
  V241439 = 2 (they do not attend) are written as "Never", V241440's own label
  for code 5; a missing code in V241439, or in V241440 for an attender, is
  `NA`.
- Every positive code found in the data must have a label (or lie on a scale
  or thermometer), and every `NA` cell must be a missing-data code; the build
  stops otherwise. `attr(x, "build_audit")` records, per column, the `NA`
  cells and the raw missing codes of the same respondents.

## Fields changed from the copy in LLMR

The build refuses any variable whose label begins, after an optional "PRE:",
"POST:", or "PRE-POST:", with SPS (spouse/partner survey), PAPER (paper-only
item), GSS (asked only of the GSS re-interview sample), or RESTRICTED. The
User Guide describes the first and third: "Another component of the study
involved a brief interview with the spouses or partners of respondents in the
fresh sample groups who were in spousal or cohabiting relationships" (p. 6),
and "GSS re-interview data are not included in this data release and will be
made available as a supplemental file" (p. 6). The file holds sample types 1-3
only (2,171 panel, 2,308 web, 1,042 face-to-face).

Each substitute was checked in the codebook before use; a printed universe
means only some respondents were asked, and no universe means all were ("note
that universes are not included for most summary variables", p. 19).

| Handle | Was | Now | Codebook (label; question; universe) |
|---|---|---|---|
| `att_aff_feeling_toward_the_maga_move` | V241168 "PRE: PAPER: FEELING THERMOMETER: MAGA MOVEMENT" (p. 83), paper only | V242148 | p. 401: "POST: FEELING THERMOMETER: MAKE AMERICA GREAT AGAIN (MAGA)"; "(How would you rate:) MAGA movement"; no universe; survey questions "THERMGR_THGRMAGA, PAPI POST Q55", so asked in every mode |
| `demo_rural_urban_identity` (new handle; `demo_community_type` is gone) | V242340 "POST: PAPER: DOES R LIVE IN RURAL AREA, SMALL TOWN, SUBURB, OR CITY" (p. 494), paper only | V242341 | p. 495: "POST: DOES R USUALLY THINK OF SELF AS A RURAL OR URBAN PERSON"; "Regardless of where you currently live, do you usually think of yourself as a city person, a suburban person, a small-town person, a country or rural person, or something else?"; no universe |
| `att_val_trust_in_the_judiciary` (new) | V242617 "POST: GSS: R'S CONFIDENCE IN US SUPREME COURT" (p. 598), universe "IF R IS IN GSS SAMPLE:" | V242419 | p. 521: "POST: CSES6-Q07C: TRUST JUDICIARY"; "(How about:) The judiciary? (Do you trust it a lot, trust it somewhat, do not trust it very much, or do not trust it at all?)"; no universe |
| `att_val_trust_in_scientists` (new) | V242620 "POST: GSS: R'S CONFIDENCE IN SCIENTIFIC COMMUNITY" (p. 599), GSS sample only | V242420 | p. 522: "POST: CSES6-Q07D: TRUST SCIENTISTS"; "(How about:) Scientists? (Do you trust them a lot, trust them somewhat, do not trust them very much, or do not trust them at all?)"; no universe |
| `att_val_trust_in_traditional_media` (new) | V242619 "POST: GSS: R'S CONFIDENCE IN THE PRESS" (pp. 598-599), GSS sample only | V242422 | p. 523: "POST: CSES6-Q07F: TRUST TRADITIONAL MEDIA"; "(How about:) Traditional news media, such as newspapers, television or radio? (Do you trust them a lot, trust them somewhat, do not trust them very much, or do not trust them at all?)"; no universe |
| `att_val_satisfaction_with_democracy` | V241711 "SPS: HOW SATISFIED WITH THE WAY DEMOCRACY WORKS IN THE UNITED STATES?" (p. 314), spouse/partner survey | V242439 | p. 531: "POST: CSES6-Q21: SATISFACTION WITH DEMOCRATIC PROCESS"; "On the whole, are you [very satisfied, fairly satisfied, not very satisfied, or not at all satisfied / not at all satisfied, not very satisfied, fairly satisfied, or very satisfied] with the way democracy works in the United States?"; no universe; codes 1, 2, 4, 5 |
| `demo_home_ownership` | V241531 "PRE: HOME TENURE CPS OWNERSHIP", universe "IF R'S HOME OWNED OR BEING BOUGHT BY HOUSEHOLD MEMBER:" (p. 230) | V241530 | pp. 229-230: "PRE: HOME TENURE CPS"; "Are your living quarters owned or being bought by a household member, rented for cash, or occupied without payment of cash rent?"; no universe |
| `demo_religious_attendance` | V241440 alone | V241440 with V241439 | p. 184: V241439 "PRE: EVER ATTEND CHURCH OR RELIGIOUS SERVICES"; "Lots of things come up that keep people from attending religious services even if they want to. Thinking about your life these days, do you ever attend religious services, apart from occasional weddings, baptisms or funerals?"; no universe |

Dropped without a substitute:

- `att_val_general_happiness`: V242614 "POST: GSS: TAKEN ALL TOGETHER HOW HAPPY IS R
  THESE DAYS", universe "IF R IS IN GSS SAMPLE:" (p. 596).
- `att_val_importance_of_remaining_a_de`: V241731 "SPS: HOW IMPORTANT IS IT
  FOR THE POLITICAL SYSTEM IN THE U.S. TO REMAIN A DEMOCRACY" (pp. 319-320),
  spouse/partner survey.

Every substitute matched what the plan assumed, so none was dropped instead.

## Question wording

`attr(x, "dictionary")` gains `wording`, `wording_variable`, `wording_page`,
and `wording_status`. Wordings are taken from the codebook's "Question"
section, transliterated to ASCII with whitespace normalized; the build checks
that each is part of the question printed for `wording_variable` and records
the page where it begins.

| Status | Fields | Meaning |
|---|---|---|
| `verbatim` | 71 | The question printed for the field's own variable |
| `from_source_item` | 37 | The variable is a summary without a question; the wording is that of the item it summarizes (party ID from V241221, approval of the President from V241134, each favor/oppose summary from its direction item) |
| `stem_only` | 7 | The codebook prints only "Where would you place yourself on this scale, or haven't you thought much about this?" for the 7-point placements; the subject is in the end labels |
| `none` | 7 | No single question: age, race/ethnicity, religion, census region, employment status, and the party of the presidential and House vote/intent/preference |

`from_source_item` is used where a summary combines one question with its
follow-ups (strength, lean, a bracketed amount). Where it combines distinct
questions (Hispanic origin and race; vote, intent, and preference; birth date
into age) the status is `none`. The income wording comes from V241555, a
restricted variable whose data are not read; only its printed question is
used. The V241237 wording stops before the interviewer instruction "{READ IF
NECESSARY}"; the response options read after it are omitted.

## Eligibility, selection, and `ideology_score`

Eligible respondents completed both interviews (V240002c = 2), neither of them
on paper (V240002a and V240002b not 3), and answered at least 75 percent of
the 122 fields ("Haven't thought much about this" counts as an answer). Of the
5,521 respondents, 4,964 completed both interviews, 4,764 of them not on
paper, and 4,710 answered enough.

Selection works on positions computed from codes rather than from sorted label text:
ordinal fields by the order of their ANES codes (or the explicit `order` of
the spec), scales by their value, thermometers by their rating, and nominal
fields by match or mismatch. The distance is a Gower distance over the
columns both respondents answered; the blocks weigh 0.25 (demographics), 0.25
(identity), 0.20 (thermometers), 0.15 (evaluations), 0.15 (issues), and 0.15
(values), each weight split equally among the block's columns. After
`set.seed(110)`, one respondent is drawn at random for each code of party
identification (1-7), race/ethnicity (1-6), and ideology (1-7) that the
respondents already drawn do not cover, which gave 11 seeds; greedy maximin
then adds the respondent farthest from those chosen until there are 100.

`ideology_score` comes from a one-dimensional graded-response model (mirt
1.44.0, `set.seed(110)` before the fit) on the 4,710 eligible respondents
and 89 items: every ordinal or numeric attitude except those that measure
salience, participation, or personal well-being (importance of party
identity, likelihood of voting, personal finances, financial worry, hope and
anger about the country, importance of abortion and of what schools teach,
whether harassment would affect the vote, trust in people, life
satisfaction). Thermometers enter in five bands (0-15, 16-40, 41-60, 61-85,
86-100), scales by their 1-7 value, and off-scale answers as missing. Items
are reversed when they correlate negatively with the anchor, the mean of
standardized party identification and ideology (32 were reversed). The model
converged in 265 EM cycles (log-likelihood -507,675.5, AIC 1,016,294.9, BIC
1,019,342.8). Absolute loadings have median 0.62 (quartiles 0.43 and 0.71,
range 0.03 to 0.90); 16 items load below 0.3, among them the thermometers
toward Congress and Jews, approval of Congress, the China and Russia threat
items, trust in the judiciary, and satisfaction with democracy. EAP scores
are standardized over the eligible pool and oriented so that high is
conservative. They correlate 0.890 with the anchor in the pool, 0.582 among
the 100, and 0.992 with the first principal component.

Maximin selection favors unusual combinations of answers. Among the 100,
ideology_score has a standard deviation of 0.55, and the mean score by party
identification (strong Democrat to strong Republican) is -0.40, -0.13, -0.01,
-0.03, 0.43, 0.22, 0.60, against -1.05, -0.56, -0.59, 0.08, 0.66, 0.57, 1.21
in the pool: the partisans chosen are those whose other answers depart from
their party's. Thirty-eight of the 100 have no 2024 presidential vote
recorded, against 18.5 percent of the eligible pool and 26.6 percent of the
file.

## Missing cells

The 100 rows hold 246 `NA` cells in 54 of the 122 fields, each a missing-data
code of the raw file:

- Items that do not apply to everyone (-1, or -2 in summaries): 2024
  presidential vote 38 (-2, no vote data: did not vote, or DK/RF/INAP),
  presidential vote intent 39 (-1), party of the House vote/intent 28 (-2),
  likelihood of voting 16 (-1), party of the presidential vote/intent 14
  (-2), importance of party identity 7 (-1).
- Refusals (-9) on demographics (sex 1, education 1, income 3) and on single
  items, most of them thermometers (1 to 6 per thermometer).
- Don't know or refused in a summary's source item (-2 in approvals,
  spending, immigration, and favor/oppose summaries; 1 to 4 each).
- Don't know (-8) on the millionaire tax and the China and Russia threat
  items, 1 each; a breakoff (-5) on life satisfaction, 1; a missing birth
  date (-2) on age, 2.
- Thermometer ratings outside 0-100: 998 or 999 on the MAGA (2), Black Lives
  Matter (2), and Planned Parenthood (1) thermometers.

The build log lists every column with its codes.

## Comparison with the copy in LLMR

Matching the 100 profiles of LLMR's copy to the file, on every field except
the four variables corrected or relabeled in the 2026 release (V242234x,
V242245x, V241445x, V241004), identifies all 100 uniquely; 26 of them are
among the new 100. All 100 completed both interviews, neither on paper.

## Judgment calls

- Block weights are split among a block's columns. LLMR's build gave every
  column its block's full weight, so blocks with more columns (47 issues)
  outweighed the stated shares.
- Seeds cover every code of party identification, race/ethnicity, and
  ideology, drawn at random under the build seed; LLMR's build took the first
  match of label patterns, one of which matched nothing.
- Five items whose codes are not in scale order get an explicit order, with
  the middle option (code 3) between codes 1 and 2: the millionaire tax
  (V242316a), gun purchases (V242325), the income gap (V241360), and the two
  child-rearing items (V242260, V242262).
- Off-scale answers are shown but left out of the distance and the model:
  "Haven't thought much about this" (99), "Side with neither" (V241409x, 8),
  "Neither" (V242260 and V242262, 4), "Something else" (V242341, 5).
- Rural-urban self-identification (V242341) is ordinal from city to country
  person; home ownership, religion, race/ethnicity, marital status, region,
  employment, military service, sex, union household, and children are
  nominal.
- The vote and candidate-preference items are nominal and stay out of the
  IRT model; the thermometers carry the candidate evaluations.
- The thermometer stems ("(How would you rate:) Kamala Harris") are
  `verbatim`, since the printed question names the object rated.
- `source` and `citation` hold the same citation string.
- The fingerprint leaves out `attr(x, "build")`, which records software
  versions, so a rebuild under another R version with identical content still
  matches.
- Cells of 76, 77, or 80 characters, the lengths of a label cut at 80 with or
  without its code prefix, must be on an allowlist: the full V241386 code 1
  label, which is itself 80 characters long, and the seven sentences of the
  medical insurance scale, which are 76.

## Tests

`tests/testthat/test-anes-personas.R`, identical in both packages, checks the
shape, handles, and attributes; the dropped fields and their substitutes;
that no cell holds missing-data vocabulary, a code prefix, braces, an "R "
prefix, or 998/999; cut-label lengths against the allowlist and the full
V241386 labels; the scale and thermometer forms, with middle scale values
present; `build_audit` (`NA` cells equal raw missing codes); the wording
rules; the fingerprint; source and citation; and ASCII. With `ANES_DTA` and
`ANES_CODEBOOK` set, it also re-verifies every wording and every label
override, with its page, against the raw files, using its own reading of the
codebook rather than `codebook.R`.
