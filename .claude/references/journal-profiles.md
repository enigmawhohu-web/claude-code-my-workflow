# Journal Profiles

<!--
These profiles calibrate the domain-referee and methods-referee when reviewing
for a specific journal. Each profile describes the journal's review culture
in plain language — the LLM adapts its priorities accordingly.

Used by: domain-referee.md, methods-referee.md (via /review --peer [journal])
-->

## How This Works

When `/review --peer [journal]` is invoked:

1. **Profile found below** → referees calibrate using the full profile
2. **Profile NOT found** → referees use the journal name + domain-profile.md to adapt (still better than generic)
3. **No journal specified** → generic top-field referee behavior

---

## Top-5 General Interest

### American Economic Review (AER)
**Focus:** All fields of economics — the broadest audience
**Bar:** Must interest economists outside your subfield. Big question, clean execution, clear contribution.
**Domain referee adjusts:** "Would a labor economist care about this health paper?" Contribution must be broad. Literature positioning against the *general* frontier, not just subfield. Policy implications welcome but not required — insight is enough.
**Methods referee adjusts:** Identification must be convincing to non-specialists. Clean, transparent design preferred over technically complex one. Standard errors and robustness should be thorough but not excessive.
**Typical concerns:** "Why should economists outside this field care?" "Is the contribution big enough for AER?" "Is this too narrow/specialized?"

### Econometrica (ECMA)
**Focus:** Theoretical and empirical economics with formal rigor
**Bar:** Methodological innovation or empirical work with exceptional identification and formal results.
**Domain referee adjusts:** Theoretical contribution valued highly. If empirical, the design must be near-airtight. Formal welfare analysis expected. Less emphasis on policy narrative, more on economic theory and mechanisms.
**Methods referee adjusts:** Formal proofs or near-formal arguments expected for key results. Asymptotic properties discussed. Novel estimators should have theoretical justification. Simulation evidence for finite-sample properties.
**Typical concerns:** "Where's the formal result?" "What are the asymptotic properties?" "Is this a methods contribution or an applied contribution?"

### Journal of Political Economy (JPE)
**Focus:** All fields — strong emphasis on economic mechanisms and structural thinking
**Bar:** Deep economic insight. JPE values understanding *why* something happens, not just *that* it happens.
**Domain referee adjusts:** Mechanism is king. Reduced-form results alone insufficient — need to explain the economics. Structural models or mechanism tests expected. Theoretical framework (even informal) valued.
**Methods referee adjusts:** Identification strong, but mechanism evidence equally important. Heterogeneity that illuminates the mechanism. Willing to accept some identification imperfection if the economic insight is deep enough.
**Typical concerns:** "What's the mechanism?" "Can you decompose the effect?" "What does this tell us about economic behavior?"

### Quarterly Journal of Economics (QJE)
**Focus:** All fields — prizes compelling narrative and important questions
**Bar:** The question must be important and the answer must surprise. QJE loves papers that change how you think about something.
**Domain referee adjusts:** Narrative matters enormously. The paper should read like a story with a punchline. Broad implications. Creative use of data or setting. "Clever" identification valued.
**Methods referee adjusts:** Identification must be clean and intuitive — not just technically correct, but easy to explain. Transparency and simplicity over complexity. Visual evidence (event studies, RD plots) highly valued.
**Typical concerns:** "Is this surprising?" "Does this change how we think about X?" "Can you explain the identification in one sentence?"

### Review of Economic Studies (REStud)
**Focus:** All fields — technically excellent empirical and theoretical work
**Bar:** Technical quality must be top-tier. Values precision and completeness over narrative.
**Domain referee adjusts:** Thoroughness expected — address every possible objection. Complete set of robustness checks. Careful literature review. Less emphasis on storytelling than QJE, more on completeness.
**Methods referee adjusts:** Every specification must be justified. Full battery of robustness checks expected. Sensitivity analysis (Oster bounds, etc.). Careful treatment of inference. Multiple testing corrections if applicable.
**Typical concerns:** "Have you checked robustness to X?" "What about specification Y?" "The inference needs more care."

---

## Top Field Journals

### American Economic Journal: Applied Economics (AEJ:Applied)
**Focus:** Empirical microeconomics — labor, health, education, development, public
**Bar:** Clean applied micro paper with credible identification and clear results. Slightly below top-5 bar but same rigor expectations.
**Domain referee adjusts:** Contribution should be meaningful to the subfield. Practical policy relevance appreciated. Literature positioning within the subfield, not the general field.
**Methods referee adjusts:** Same identification standards as top-5. Modern estimators expected (no naive TWFE for staggered). Replication package expected.
**Typical concerns:** "Is this incremental relative to [closely related paper]?" "Would this be better in a field journal?"

### American Economic Journal: Economic Policy (AEJ:Policy)
**Focus:** Policy evaluation and design — how policies affect outcomes
**Bar:** Must have direct policy relevance. Natural experiments from actual policy changes preferred.
**Domain referee adjusts:** Policy implications front and center — not an afterthought. Cost-benefit or welfare discussion expected. Institutional details of the policy must be well-documented. Generalizability to other policy contexts.
**Methods referee adjusts:** Identification from actual policy variation (not cross-sectional). Pre-trends must be clean. Heterogeneity by policy-relevant subgroups expected. Back-of-envelope welfare calculations.
**Typical concerns:** "What should policymakers do with this?" "Does this generalize to other states/countries?" "What's the cost-benefit?"

### Journal of Human Resources (JHR)
**Focus:** Labor economics, education, health, demography
**Bar:** Strong empirical contribution with clear policy relevance and careful identification.
**Domain referee adjusts:** Policy relevance matters more than theoretical novelty. External validity — can results inform actual policy? Sample representativeness. Heterogeneity analysis by policy-relevant subgroups expected. Institutional knowledge of labor markets/education systems/health care valued.
**Methods referee adjusts:** Clean identification is non-negotiable. Modern staggered DiD estimators required if applicable. Robustness to functional form. Pre-trends must be clean and shown. Replication package expected at acceptance.
**Typical concerns:** "What's the policy implication?" "Does this generalize beyond your sample?" "Have you considered heterogeneity by [race/gender/income]?"

### Journal of Health Economics (JHE)
**Focus:** Health economics — insurance, utilization, provider behavior, public health
**Bar:** Sound health economics with credible identification. Institutional knowledge of health care systems expected.
**Domain referee adjusts:** Must demonstrate deep understanding of health care institutions. Moral hazard vs. adverse selection distinction matters. Welfare implications expected. Connection to health policy debates. Knowledge of CMS data, insurance markets, provider incentives.
**Methods referee adjusts:** Health-specific threats: selection into insurance, Ashenfelter dip in health utilization, moral hazard confounding. IV exclusion restrictions scrutinized heavily in health contexts. GLM for cost outcomes expected alongside OLS.
**Typical concerns:** "Is this moral hazard or adverse selection?" "Have you addressed selection into treatment?" "What about the Medicaid population specifically?"

### RAND Journal of Economics (RAND)
**Focus:** Industrial organization, regulation, antitrust, health care markets
**Bar:** IO-flavored analysis with market structure or firm behavior component. Structural or quasi-experimental.
**Domain referee adjusts:** Market structure and competition implications. Firm behavior and strategic incentives. Regulatory implications. Welfare analysis (consumer surplus, total surplus) expected.
**Methods referee adjusts:** Structural models valued alongside reduced-form. Demand estimation methods (BLP, discrete choice). Entry/exit models. Merger simulation if relevant. Reduced-form papers need very clean identification.
**Typical concerns:** "What does this imply for market structure?" "Consumer welfare impact?" "Can you do a structural analysis?"

### Journal of Public Economics (JPubE)
**Focus:** Tax policy, public goods, redistribution, government programs
**Bar:** Public finance question with clean identification. Understanding of tax/transfer system mechanics.
**Domain referee adjusts:** Tax incidence, deadweight loss, behavioral responses to taxation. Program evaluation of government interventions. Fiscal federalism. Redistribution and inequality. Knowledge of tax code and transfer programs.
**Methods referee adjusts:** Bunching estimators for tax kinks/notches. RDD at eligibility thresholds. DiD around policy changes. Structural models of labor supply response. Extensive vs. intensive margin effects.
**Typical concerns:** "What's the elasticity?" "Extensive or intensive margin?" "Welfare implications of the tax/transfer change?"

### Journal of Labor Economics (JLE)
**Focus:** Labor markets — wages, employment, human capital, discrimination, immigration
**Bar:** Clean labor economics with careful identification. Understanding of labor market institutions.
**Domain referee adjusts:** Wage determination, employment effects, human capital returns, discrimination, unions, immigration. Mincer equations and labor supply models. Firm-worker matched data valued. Monopsony and market power in labor markets.
**Methods referee adjusts:** Selection correction (Heckman, Lee bounds) when relevant. Decomposition methods for wage gaps. Clean identification of causal effects on wages/employment. Event study designs around job transitions or policy changes.
**Typical concerns:** "Is this a supply or demand effect?" "Selection into employment?" "What about general equilibrium effects?"

### Journal of Development Economics (JDE)
**Focus:** Development economics — poverty, institutions, agriculture, trade in developing countries
**Bar:** Credible empirical evidence on development questions. RCTs or strong quasi-experimental designs. Field knowledge.
**Domain referee adjusts:** Context matters enormously — deep knowledge of the country/region expected. External validity to other developing country settings. Implementation details for interventions. Cost-effectiveness. Sustainability of effects. Gender and equity dimensions.
**Methods referee adjusts:** RCTs: randomization checks, attrition, compliance, spillovers, pre-analysis plan. Quasi-experimental: strong first stage for IV, clean RD, credible parallel trends. Power calculations. Clustered standard errors at appropriate level.
**Typical concerns:** "Does this generalize beyond this specific context?" "What about attrition?" "Cost-effectiveness?" "Long-run effects?"

---

## Strong Field Journals

### Review of Economics and Statistics (RESTAT)
**Focus:** Empirical economics — all fields, emphasis on careful measurement and methods
**Bar:** Technically excellent empirical work. Values careful econometrics and measurement.
**Domain referee adjusts:** Measurement quality is paramount. Novel data or measurement approaches valued. Less emphasis on big-picture narrative than QJE, more on getting the econometrics exactly right. Replication studies welcome.
**Methods referee adjusts:** Highest econometric standards short of Econometrica. Every assumption must be tested or bounded. Sensitivity analysis expected. Careful treatment of standard errors. Pre-registration or pre-analysis plans viewed favorably.
**Typical concerns:** "Is the measurement precise enough?" "Have you tested every assumption?" "What about measurement error in [variable]?"

### AER: Insights
**Focus:** Same breadth as AER but shorter format — important results that can be communicated concisely
**Bar:** AER-quality insight in a shorter paper. Must be self-contained and punchy.
**Domain referee adjusts:** Brevity is a feature, not a limitation. One clean result is enough. No need for 15 robustness checks — the core result must be compelling on its own. Well-suited for striking findings or clever identification.
**Methods referee adjusts:** Core identification must be clean. Fewer robustness checks acceptable given format, but the main result must be robust. Transparency and visual evidence valued.
**Typical concerns:** "Can this be communicated in 10 pages?" "Is the single result compelling enough?" "Does this need a longer format to be convincing?"

---

## Political Science — Top General

### American Political Science Review (APSR)
**Focus:** All subfields of political science — the discipline's flagship journal
**Bar:** Must make a significant contribution to political science broadly. Big question, rigorous execution, theoretical or empirical innovation.
**Domain referee adjusts:** "Does this advance our understanding of politics?" Contribution must transcend the subfield. Strong theoretical grounding expected — even empirical papers need a clear theoretical framework. Comparative and historical breadth valued.
**Methods referee adjusts:** Appropriate methods for the question. Survey research must demonstrate careful design, weighting, and sampling. Experiments need pre-registration or strong justification. Observational claims need careful attention to identification.
**Typical concerns:** "Is this political science or just policy analysis?" "What's the theoretical contribution beyond the empirical finding?" "Does this speak to the discipline broadly?"

### American Journal of Political Science (AJPS)
**Focus:** All subfields — particularly strong in American politics, political behavior, and methodology
**Bar:** Rigorous empirical or theoretical contribution. Slightly more methods-forward than APSR. Values innovation in measurement or design.
**Domain referee adjusts:** Methodological sophistication valued highly. Novel data or measurement approaches get attention. Clear causal reasoning expected even in observational work. American politics and behavior papers are well-represented.
**Methods referee adjusts:** State-of-the-art methods expected. Survey experiments should follow best practices (pre-registration, manipulation checks, attention checks). Bayesian and ML approaches welcome if well-motivated. Robustness to specification choices.
**Typical concerns:** "Is the method appropriate and state-of-the-art?" "Can you rule out alternative explanations?" "What's the mechanism?"

### Journal of Politics (JOP)
**Focus:** All subfields of political science — strong in American politics, comparative, IR
**Bar:** Solid empirical contribution with clear political science relevance. Slightly more accessible bar than APSR/AJPS.
**Domain referee adjusts:** Clear research question and clean execution matter most. Policy relevance appreciated but not required. Good fit for well-executed studies that advance a specific literature. Comparative extensions valued.
**Methods referee adjusts:** Sound methods appropriate to the question. Don't need cutting-edge methods if the design is clean. Standard errors and inference must be careful. Replication materials expected.
**Typical concerns:** "Is this incremental?" "How does this change what we know?" "Is the scope too narrow?"

---

## Political Science — Top Field

### Public Opinion Quarterly (POQ)
**Focus:** Public opinion measurement, survey methodology, political attitudes, media effects
**Bar:** Must contribute to understanding of opinion formation, measurement, or survey methodology. The premier outlet for public opinion research.
**Domain referee adjusts:** Survey methodology must be impeccable — this is the audience that will scrutinize every design choice. Question wording, mode effects, response rates all matter. Theoretical grounding in opinion formation (Zaller, dual-process models) expected. Historical context of the opinion trend valued.
**Methods referee adjusts:** Survey design is the method. Weighting strategy must be transparent and justified. Margins of error reported. Question wording effects addressed. If experimental, full design details and manipulation checks. Split-ballot experiments valued.
**Typical concerns:** "Is the sample representative?" "How sensitive are results to question wording?" "What does this add beyond Pew/Gallup/ANES?" "Have you addressed social desirability?" "Mode effects?"

### International Organization (IO)
**Focus:** International relations theory and empirical IR — institutions, cooperation, conflict, political economy
**Bar:** Must advance IR theory. Empirical papers need strong theoretical motivation. Among the most prestigious IR journals.
**Domain referee adjusts:** Theory-driven research valued above all. Public opinion papers must connect to IR theory (audience costs, two-level games, rally effects, threat perception). Institutional and structural arguments expected. Comparative/cross-national scope preferred over single-country.
**Methods referee adjusts:** Causal identification important but theory comes first. Survey experiments must speak to theoretical mechanisms. Observational work needs careful identification. Large-N cross-national analysis common.
**Typical concerns:** "What's the IR theory?" "How does public opinion matter for international outcomes?" "Is this comparative or US-only?" "Does this generalize beyond the current political moment?"

### International Security
**Focus:** Security studies — military affairs, defense policy, nuclear strategy, terrorism, emerging threats
**Bar:** Must engage security studies debates directly. Strong policy relevance expected. Accessible writing valued.
**Domain referee adjusts:** Policy relevance is paramount — what should decision-makers learn? Fentanyl as a security issue (transnational threats, non-traditional security) needs clear framing. China-specific security implications must be explicit. Historical cases and analogies valued.
**Methods referee adjusts:** Qualitative and mixed-methods work is common and welcome. Survey research must be clearly presented to a non-methods audience. Policy implications must be concrete, not abstract. Practitioner accessibility matters.
**Typical concerns:** "What are the security implications?" "Is this a security issue or a public health issue?" "What should policymakers do?" "How does this connect to broader US-China competition?"

### Journal of Conflict Resolution (JCR)
**Focus:** Conflict, peace, cooperation — quantitative and formal approaches to IR
**Bar:** Rigorous quantitative or formal contribution to understanding conflict and cooperation. Methods-forward IR.
**Domain referee adjusts:** Quantitative rigor expected. Cooperation vs. punishment framing fits well. Game-theoretic or bargaining frameworks valued. Cross-national or experimental designs preferred. Must connect to conflict/cooperation literature explicitly.
**Methods referee adjusts:** Sophisticated quantitative methods expected. Survey experiments should follow best practices. Formal models welcome. Careful inference and robustness. Large-N analysis or experimental designs.
**Typical concerns:** "How does this relate to the cooperation/conflict literature?" "Is the formal framework clear?" "What's the theoretical mechanism?" "External validity beyond the US case?"

---

## Area Studies

### Journal of Contemporary China (JCC)
**Focus:** All aspects of contemporary China — politics, economics, society, foreign relations
**Bar:** Must demonstrate deep knowledge of China and contribute to understanding of Chinese politics, society, or foreign relations. Area expertise essential.
**Domain referee adjusts:** China-specific knowledge is non-negotiable. US public opinion about China must be contextualized within the bilateral relationship. Knowledge of Chinese domestic politics, foreign policy discourse, and elite debates expected. Historical context of US-China relations valued.
**Methods referee adjusts:** Mixed methods common. Survey research welcome but must be grounded in area knowledge. Qualitative evidence from Chinese sources valued alongside quantitative analysis. Translation and cultural context matter.
**Typical concerns:** "Does the author understand China?" "How is this situated in the US-China relationship literature?" "What Chinese sources are used?" "Is this US-centric or genuinely about China?"

### Chinese Journal of International Politics (CJIP)
**Focus:** International politics with emphasis on China's role — theory, empirical analysis, and policy
**Bar:** Must engage with debates about China's international role. Theoretical ambition valued. Published by Oxford UP, increasingly influential.
**Domain referee adjusts:** China's rise and international order debates are central. Public opinion research must connect to foreign policy output or international audience costs. Constructivist and identity-based arguments about threat perception fit well. US-China strategic competition framing.
**Methods referee adjusts:** Pluralistic methods — quantitative, qualitative, and theoretical all welcome. Survey data valued if it illuminates public constraints on foreign policy. Must engage with both Western and Chinese IR scholarship.
**Typical concerns:** "How does this contribute to understanding China's international role?" "What are the implications for US-China relations?" "Does this engage Chinese IR scholarship?"

---

## Add Your Own Journal

Copy this template and add it above this section:

```markdown
### [Journal Name] ([Abbreviation])
**Focus:** [fields and topics covered]
**Bar:** [what it takes to publish here]
**Domain referee adjusts:** [what matters most to domain reviewers at this journal]
**Methods referee adjusts:** [rigor expectations, preferred methods, required checks]
**Typical concerns:** [common referee questions at this journal]
```
