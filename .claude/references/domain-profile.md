# Domain Profile

## Field

**Primary:** Political Science — Public Opinion / Foreign Policy Preferences
**Adjacent subfields:** International Relations, Public Policy, Survey Methodology, US-China Relations

---

## Target Journals (ranked by tier)

| Tier | Journals |
|------|----------|
| Top general | APSR, AJPS, JOP |
| Top field | Public Opinion Quarterly (POQ), International Organization (IO), International Security |
| Strong field | Journal of Conflict Resolution (JCR), Journal of Politics (JOP), Foreign Policy Analysis |
| Area studies | Journal of Contemporary China (JCC), Chinese Journal of International Politics (CJIP) |
| Policy | Foreign Affairs, Brookings Papers, JPAM |

---

## Common Data Sources

| Dataset | Type | Access | Notes |
|---------|------|--------|-------|
| 2024 YouGov wave | Cross-sectional survey (n=3,237 interviewed → 3,000 matched) | Proprietary (raw gitignored) | MoE ±1.94%; fielded Mar 12–19, 2024 |
| 2025 YouGov wave | Cross-sectional survey (n=2,273 interviewed → 2,000 matched) | Proprietary (raw gitignored) | MoE ±2.4%; fielded Apr 15–25, 2025 |

---

## Analytical Approaches Used in This Paper

| Approach | Application | Key Assumption to Defend |
|----------|-------------|------------------------|
| Survey-weighted OLS | Posture index ~ warmth × blame + controls + year FE | Model-based SEs; associational, not causal |
| Multinomial logit (survey-weighted) | 4-category tool configuration (Neither/S-only/A-only/S+A) | IIA; survey weights as frequency weights |
| Stacked OLS with respondent clustering | 8-probe falsification test — scenario-specific S+A vs S-only contrasts | Boundary probes are valid negative controls |
| TOST equivalence test | Confirm non-target probe effects are negligibly small (ε = 0.10 × SD) | Equivalence bound choice |
| Cross-wave pooling | Pooled models with year FE; wave-specific replication | Comparable samples and question wording across waves |
| Acquiescence controls | Endorsement-count measure to adjust for response style | Endorsement count captures acquiescence tendency |

---

## Field Conventions

- Always report weighted estimates and unweighted sample sizes
- Report margins of error (MoE) for key estimates — typically 95% CI
- Demographic breakdowns are expected: party ID, age, race/ethnicity, education, gender, region
- Likert-scale items: report full distribution and/or collapsed agree/disagree shares
- Avoid causal language for observational survey associations — use "associated with," "predicts," not "causes" or "effects"
- Question wording must be reproduced verbatim in appendix or methods section
- Report survey mode (online panel, phone, etc.), fielding dates, and response rate
- Use `survey` package in R for design-based inference (weights, stratification, clustering)

---

## Notation Conventions

| Symbol | Meaning | Anti-pattern |
|--------|---------|-------------|
| `warmth_z` | Standardized 7-pt warmth toward China (pooled-wave moments) | Don't use raw unstandardized warmth |
| `posture_z` | Standardized 4-item foreign-policy posture index | Don't use individual items without noting |
| `blame_china` | Binary: attributes fentanyl responsibility to China (multi-select) | Don't confuse with `blame_china_alt` (single-choice) |
| S+A | Sanctions + Action (requiring Chinese govt action) | Don't call it "cooperative" alone — it's mixed |
| $\Delta_s$ | Scenario-specific S+A vs S-only contrast | Don't use without specifying the scenario |
| $n$ | Unweighted sample size | Don't use $N$ for both weighted and unweighted |
| MoE | Margin of error at 95% confidence | Don't omit confidence level |

---

## Seminal References

| Paper | Why It Matters |
|-------|---------------|
| Zaller (1992) *The Nature and Origins of Mass Opinion* | RAS model — elite cues and opinion formation |
| Page & Shapiro (1992) *The Rational Public* | Aggregate opinion stability and multidimensionality |
| Herrmann, Tetlock & Visser (1999) | Image theory — adversary schemata and mass war support |
| Kertzer & Zeitzoff (2017) | Bottom-up theory of foreign policy attitudes |
| Jentleson (1992) *Pretty Prudent Public* | PPO framework — objective-sensitivity in force support |
| Heinrich et al. (2017) | Public support for sanctions — instrumental reasoning |
| Krosnick (1990) *Government Policy and Citizen Passion* | Issue publics framework |
| Rossiter (2026) | Pivotal-circumstance theory of issue public formation |
| Gries (2010, 2014) | Ideology/partisanship cleavages in China attitudes |
| Li (2021) *More Than Favorable* | Multidimensionality of US-China public attitudes |
| Smeltz et al. (Chicago Council surveys) | Annual US foreign policy attitude surveys — closest comparator |

---

## Field-Specific Referee Concerns

- "How representative is the sample?" — YouGov matched samples; must document weighting strategy clearly
- "Acquiescence / response style" — S+A could be checkbox-ticking; addressed via endorsement-count controls (~38% attenuation)
- "Causality" — Cross-sectional design; cannot establish blame → preferences causally. Paper is explicit about this.
- "Domain-specific social desirability" — Falsification test rules out generalized acquiescence but not domain-specific framing effects
- "Placebo concern" — Sanctions × international cooperation placebo shows comparable shift; tool-specificity is weaker than domain-specificity
- "Post-treatment bias" — Conditioning on posture_z in Step 2 is post-treatment; paper acknowledges within-posture interpretation
- "Partisan heterogeneity" — Lacks power for 3-way interactions (party × warmth × blame); acknowledged limitation
- "Why not use ANES/Pew/Chicago Council?" — Original data needed for fentanyl-specific tool configuration and 8-probe battery
- "External validity" — Single bilateral case (US-China); 2024-2025 is a specific political moment

---

## Quality Tolerance Thresholds

| Quantity | Tolerance | Rationale |
|----------|-----------|-----------|
| Point estimates (proportions) | ± 0.001 | Rounding to 3 decimal places for tables |
| Margins of error | ± 0.5 pp | Half a percentage point precision standard |
| Weighted N | Report to nearest integer | Effective sample size after weighting |
| Subgroup estimates | Flag if cell size < 100 | Unreliable estimates in small subgroups |
