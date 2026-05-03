# Theory Review: prism/manuscript.tex (§2 and §5)
**Date:** 2026-04-22
**Reviewer:** theorist-critic
**Calibration note:** Calibrated to **applied political-science conceptual framework**, NOT formal econometric theory. The manuscript contains no theorems, proofs, identification results, or asymptotic distribution claims. The "theory" under review is a **verbal conceptual framework** (Issue-Bounded Conditionality, IBC) with two scope conditions and two observable expectations. Phase 2 logic checks therefore translate to "do the OEs follow from the verbal premises?" rather than "do the proofs hold?" Phase 2B–2E (measurability, expansions, asymptotic distributions) are not applicable. The deduction rubric is applied analogically: "logic gap" = OE does not follow from premises; "circular reasoning" = premise restates conclusion; "over-claim" = verbal claim exceeds what the framework supports.

---

## Phase 1: Claim Identification

- **Object type:** Conceptual framework with observable expectations (no formal theorem). Closer in genre to Jentleson's PPO or Krosnick's issue-publics framework than to a formal model.
- **Target "parameter":** A pattern of preference configurations — the joint distribution of (warmth, blame) over (general posture, tool configuration, domain probes).
- **Two structural claims (§2.2, line 87):**
  1. **Two-level claim** — generalized bilateral orientation and issue-level instrument preference can decouple.
  2. **Domain-bounded claim** — decoupling is tethered to a specific issue domain, not free-floating.
- **Mechanism (§2.2 ¶6b, line 94):** Bounded instrumental logic ("leverage") as working hypothesis; normative-cueing alternative explicitly named and not adjudicated by cross-sectional design.
- **Scope conditions (§2.2 ¶7, line 97):**
  - **SC1: Problem structure** — domestic salience + traceable attribution + compliance-dependent resolution.
  - **SC2: Tool structure** — bounded, reversible, issue-specific coercive tools (not relationship-ending).
- **Observable expectations (§2.2 ¶8, line 100):**
  - **OE1:** Warmth × blame interaction on posture (blame amplifies warmth–posture gradient, not collapses it).
  - **OE2:** Domain-concentration of S+A preference on the focal (fentanyl) probe; primary falsifiable implication.
- **Antecedent literatures cited:** image theory (Herrmann); affect/threat (Marcus, Lerner, Brader, Huddy); structured opinion (Page–Shapiro, Hurwitz–Peffley, Kertzer); PPO (Jentleson, Carnegie); instrument-level work (Tomz, Heinrich, McLean, Frye, Levin); issue publics (Krosnick; Rossiter 2026); China-specific (Gries, Li, Jin, Wick).
- **Paper type fit:** Appropriate. This is a public-opinion paper at POQ/IO/JCC tier. A formal-model treatment would be out of place; a verbal framework with named scope conditions and an OE2 that is *designed to be falsifiable* is the right register.

---

## Phase 2: Logical Validity (analogue to "Proof Validity")

**Assessment: SOUND with two MINOR logic-tightening opportunities.**

### Issue 2.1 — OE1's discriminating power is conceded but the §2.2 framing is a touch too strong (MINOR)
- **Location:** §2.2 ¶8 (line 100) vs. commented-out alternative (line 102, the `%`-prefixed paragraph) and §5 ¶1 (line 320).
- **Problem:** The active line 100 says OE1 follows from the IBC account: "Warmth and blame attribution should jointly structure foreign-policy orientation, such that blame amplifies rather than eliminates the warmth-posture gradient." But the commented-out alternative paragraph (102) more honestly notes that "Simpler affect-amplification accounts could also produce this pattern, so OE1 alone cannot distinguish issue-bounded conditionality from alternative mechanisms." The active text loses this caveat. Strictly, OE1 is necessary-not-sufficient: a warmth × blame interaction is consistent with IBC but also with simple amplification. Without the caveat, an unkind reader could read OE1 as overstated.
- **Severity:** MINOR (–3, exposition / under-acknowledged limitation). Not a logic gap because §2.2 line 100 does say "OE2 is the framework's primary falsifiable implication" — implying OE1 is precondition rather than discriminating. But the relationship between OE1 and OE2 should be one sentence more explicit.
- **Suggested fix:** Restore the spirit of the commented-out passage in 1–2 sentences: "OE1 establishes a precondition (rules out collapse to uniform confrontation) but does not by itself discriminate IBC from affect-amplification accounts; OE2 is the discriminating test."

### Issue 2.2 — Falsifiability claim for OE2 is logically sound but slightly overstated by the "primary falsifiable implication" wording (MINOR)
- **Location:** §2.2 line 100; mirrored in §5 ¶2 (line 323).
- **Problem:** "OE2 is the framework's primary falsifiable implication" is *almost* right. Strictly, what OE2 falsifies is the conjunction (IBC ∧ scope conditions met for fentanyl). A null result on the seven boundary probes plus a positive target probe falsifies generalized endorsement; *equal* effects across all eight probes would falsify domain-boundedness. This nuance is correctly handled in the §3 footnote distinguishing "specificity test" from "falsification test" (line 147). The §2.2 statement could mirror that footnote's care. Not a logic gap — the framework genuinely is falsifiable in the way claimed — but the §2.2 passage would benefit from one sentence acknowledging that OE2 jointly tests IBC + the scope-condition assignment to the fentanyl case (not IBC in isolation).
- **Severity:** MINOR (–3, statement quality, akin to "interpretation gloss missing").
- **Suggested fix:** Append: "More precisely, OE2 jointly tests IBC together with the claim that scope conditions are met for the fentanyl case; rejection therefore implicates either IBC or the scope-condition assignment."

### No circular reasoning detected
The premises ("citizens differentiate by objective, instrument, domain") and the conclusion ("two-level structure can decouple under SC1 ∧ SC2") are logically distinct. The mechanism (leverage) is offered as a *hypothesis* not as definitional, and the normative alternative is explicitly preserved (line 94). No premise restates the conclusion. **No deduction here.**

### Phase 2 total: –6

---

## Phase 3: Assumptions and Statements (Scope Conditions)

**Assessment: GOOD, with two issues.**

### Issue 3.1 — Scope conditions partially overlap; not stated as independent (MAJOR)
- **Location:** §2.2 ¶7 (line 97).
- **Problem:** SC1 (problem structure) includes "compliance-dependent resolution," and SC2 (tool structure) requires "bounded, reversible, issue-specific" tools. These are conceptually correlated: a problem whose resolution requires the foreign actor's active compliance (SC1) almost forces the available tools to include something other than relationship-ending ones (SC2), because relationship-ending tools cannot extract compliance. The text presents them as two separable conditions without flagging the dependency. This is not "non-minimality" in the strict sense — both conditions do real work (one could imagine compliance-dependent problems where the politically salient tool *menu* nonetheless skews to relationship-ending, e.g., Taiwan), but the conceptual overlap should be acknowledged so the framework's empirical content is clear.
- **Severity:** MAJOR analogue (–5, "non-redundancy: assumption partially implied by another, no rationale stated"). Not –20 logic gap because the conditions are logically separable; not pure non-minimality because both can fail independently.
- **Suggested fix:** One sentence: "SC1 and SC2 are conceptually correlated — compliance-dependent problems often co-occur with bounded tool menus — but they can dissociate (e.g., compliance-dependent crises where the politically dominant tools are framed as escalatory), so we list them separately."

### Issue 3.2 — Hidden assumption: respondent capacity to differentiate (MINOR)
- **Location:** §2.2 throughout, especially ¶6b (line 94).
- **Problem:** The framework implicitly assumes respondents have sufficient cognitive capacity / political attention to perform the differentiation IBC requires (between general posture and instrument-level preference). The issue-publics scope condition (§2 ¶5, line 79) partially addresses this by restricting the framework's predictions to attentive subgroups, but §2.2 itself does not import that scope condition explicitly into the SC1/SC2 list. As written, SC1 and SC2 are *situational* (about the problem and the tool menu); the *cognitive-attentional* scope condition is articulated only in the literature paragraph and never elevated to the formal scope-conditions list.
- **Severity:** MINOR (–3, "assumption not interpreted / scope condition not formally promoted").
- **Suggested fix:** Either (a) add a third scope condition (issue-public membership / attentiveness), or (b) state explicitly that SC1 and SC2 condition on the issue-publics premise carried forward from §2.1.

### Phase 3 total: –8

---

## Phase 4: Citations and Linkage

**Assessment: STRONG.** Antecedents for each IBC building block are appropriately cited.

| Building block | Cited antecedent | Verdict |
|---|---|---|
| Image theory / threat-affect | Herrmann 1997, 1999; Marcus 2000; Lerner 2001; Brader 2008; Huddy 2005 | OK |
| Bottom-up / structured opinion | Hurwitz–Peffley 1987; Kertzer 2017; Page–Shapiro 1992 | OK |
| PPO | Jentleson 1992, 1998; Carnegie 2022 | OK |
| Instruments / sanctions | Tomz 2007/2013; Heinrich 2017; Frye 2019; McLean 2017; Levin 2025 | OK |
| Issue publics | Krosnick 1989/1990/1995; Rossiter 2026 | OK |
| China multidimensionality | Li 2021; Gries 2010/2014; Jin 2021; Wick 2014 | OK |

### Issue 4.1 — §5 linkage to OEs is implicit, not labeled (MINOR)
- **Location:** §5 ¶1–2 (lines 320–323).
- **Problem:** The discussion describes findings consistent with OE1 and OE2 but never says "consistent with OE1" / "discriminating evidence for OE2." A reader has to reconstruct the linkage. The OE-labeling discipline that §2.2 sets up is not carried through. This is the "orphan claim" risk in reverse — claims in §5 are supported by results, but the *theoretical* mapping is left to the reader.
- **Severity:** MINOR (–3, exposition).
- **Suggested fix:** Add explicit cross-references: "(consistent with OE1)" after the warmth × blame discussion in §5 ¶1; "(the discriminating test of OE2)" after the eight-probe panorama in §5 ¶2.

### Issue 4.2 — "Leverage" terminology defined but used variably (MINOR)
- **Location:** §2.2 line 94 ("I use *cooperation* and *leverage* interchangeably") and §5 line 323 ("leverage framework").
- **Problem:** The dual usage is signposted clearly in §2.2 and §3.2 (the methods discussion). In §5 the term "leverage framework" appears without recap. Not strictly a notation inconsistency (–3 each), more an exposition issue. Minor enough to mention rather than score separately.
- **Severity:** Noted, not deducted.

### Phase 4 total: –3

---

## Score

| Phase | Deduction |
|---|---|
| Phase 2 (logic) | –6 (Issues 2.1 + 2.2) |
| Phase 3 (scope conditions) | –8 (Issues 3.1 + 3.2) |
| Phase 4 (citations / linkage) | –3 (Issue 4.1) |
| **Total deduction** | **–17** |
| **Final score** | **83 / 100** |

This is consistent with the prior domain-referee (83) and methods-referee (86) scores. The conceptual framework is sound; the issues identified are exposition-level tightening, not structural defects.

---

## Priority Recommendations

1. **[MAJOR — Issue 3.1]** Acknowledge the conceptual correlation between SC1 (compliance-dependent problem) and SC2 (bounded tools) in one sentence in §2.2 ¶7. Note that they can dissociate (Taiwan example already in the paragraph helps) so the framework retains empirical content.

2. **[MINOR — Issue 2.1]** Restore the OE1-as-precondition framing from the commented-out alternative paragraph (line 102). One sentence in §2.2 ¶8: "OE1 rules out the collapse-to-uniform-confrontation scenario but does not by itself discriminate IBC from affect-amplification; OE2 is the discriminating test."

3. **[MINOR — Issue 4.1]** Add explicit "(OE1)" / "(OE2)" tags in §5 ¶1–2 so readers can map findings to predictions without reconstruction.

4. **[MINOR — Issue 2.2]** Tighten the "primary falsifiable implication" claim in §2.2 line 100 to mirror the careful framing already present in the §3 specificity-test footnote (line 147): OE2 jointly tests IBC + scope-condition assignment.

5. **[MINOR — Issue 3.2]** Either elevate issue-public attentiveness to a third scope condition or state it explicitly as a maintained background assumption to which SC1/SC2 are added.

---

## Positive Findings

1. **Falsifiability is taken seriously.** The framework names OE2 as the falsifiable implication and the eight-probe design is genuinely engineered to be capable of falsifying it. Many conceptual frameworks in this literature stop at "consistent with" — IBC commits to "would-disconfirm" conditions explicitly. Notable strength.

2. **Mechanism humility.** §2.2 ¶6b (line 94) names the alternative (normative cueing) and openly states that the cross-sectional design cannot adjudicate. This is rare and correct.

3. **Scope conditions are doing real work.** SC2's Taiwan-counterexample (line 97) shows the author has thought about cases where the framework predicts *non*-IBC patterns. A scope condition with a worked counterexample is a much stronger framework statement than scope conditions stated abstractly.

4. **Two-claim decomposition.** Splitting "structural" from "conditional" claims (line 87) and developing them in turn (line 89, 91) is unusually disciplined exposition for a verbal framework.

---

## Honest Assessment: Is the Theorist-Pair the Right Tool?

**Largely no, with a small useful residual.** The theorist / theorist-critic agents are designed for formal theory sections — proofs, identification arguments, asymptotic distributions, DML-style orthogonal moments. This manuscript has none of those. Running the full Phase 2A–2E rubric is mostly N/A.

What the theorist-critic *can* usefully do for this paper is what was done above: check (a) whether the OEs follow from the verbal premises, (b) whether scope conditions are independent and well-defined, (c) whether the falsifiability claim is logically clean, (d) whether antecedent works are properly cited. That's a real but narrow service — comparable to a *conceptual-framework critic*, which the .claude/agents/ directory does not currently have.

**Recommendation:** For future runs on this manuscript, prefer **domain-referee + methods-referee** as the primary pair (already done, scoring 83 + 86) and use theorist-critic only as a targeted sweep on §2.2's logical structure — exactly what was done here. Or consider adding a `framework-critic.md` agent calibrated to qualitative/conceptual frameworks (scope conditions, observable expectations, falsifiability, mechanism plausibility) for political-science papers without formal models.
