# HW2 – Show Off You’re Data-Fluent (Standard Weight)

**Due:** Oct 19, 2025 by 11:59 PM  
**Points:** 5  
**Submission:** File upload  
**Availability:** Oct 10, 2025 (12:00 AM) – Oct 22, 2025 (11:59 PM)  
**Status:** Assignment locked after Oct 22, 2025 at 11:59 PM  

---

## 🧭 Homework Show-Off: “Prove You’re Data-Fluent!”

Each section title must be **click-bait-style**, but the justification **must come from the student’s actual statistics**.

---

## Goal

You may use:
- The **same dataset as HW1**, or  
- A **different dataset**

Apply the **four inferential frameworks** covered in class so far.  
Each framework must include:
- Clear hypotheses
- Numeric results
- Short interpretation (2–4 sentences)

📌 **Only base R functions are allowed**, including:

- `subset()`
- `tapply()`
- `table()`
- `mean()`
- `nrow()`
- z-test
- permutation test
- chi-square test

You must **show code and outputs**.

Each section must have:
- A **headline that attracts clicks**
- A **justification based on real statistical results**

It is critical that you can explain:
- What your code does
- What each line of code means
- How the statistical method is being implemented

---

## 1. Hypothesis Testing

**Topic:** Hypothesis Testing (z-test and Permutation Test)

Choose **two groups** in your dataset  
(e.g., male vs female customers, weekday vs weekend sales)

### Requirements
- State **H₀** and **H₁**
- Compute the **difference of means** and its **z-score**
- Report the **p-value**
- Verify using a **permutation test**  
  (randomly reshuffle labels ≥ 1,000 times)
- Explain whether the effect is **“shocking”** or just **random noise**

### Headline Justification
Include the **observed difference and p-value**  
(e.g., *“Weekend tips were 18% higher, p = 0.01”*)

---

## 2. Confidence Intervals

**Topic:** Confidence Intervals

Choose **one numerical variable**  
(e.g., price, score, rating)

### Requirements
- Compute:
  - Sample mean
  - Standard deviation
  - Sample size
  - 95% confidence interval for the mean
- Add an interpretation:
  > “If we repeated this study many times…”

### Headline Justification
Explain whether the interval is:
- Narrow (stable estimate), or
- Wide (high uncertainty)

---

## 3. Hypothesis of Independence

**Topic:** Chi-Square Test of Independence

Choose **two categorical variables**  
(e.g., gender vs favorite brand, region vs success)

### Requirements
- Build a contingency table using `table()`
- Run `chisq.test()`
- Report:
  - χ² value
  - Degrees of freedom
  - p-value
- Explain whether categories act independently or show a hidden link

### Headline Justification
If the association is significant, highlight it  
(e.g., *“Taste and Region are linked: χ² = 15.8, p < 0.01”*)

---

## 4. Bayesian Reasoning

**Topic:** Bayesian Reasoning (Posterior Odds)

Use your dataset to:
- Define a **belief (prior)**
- Define an **observation (evidence)**
- Compute:
  - Prior odds
  - Likelihood ratio
  - Posterior odds  
  using the **Odds Version of Bayes’ Theorem**

---

## 🏁 Deliverables

Submit a **short report (.Rmd or .pdf)** with:
- Four sections
- Dataset link

Each section must include:
- Clickbait title
- Code
- Numerical result
- Interpretation
- Justified headline

Include a **final paragraph** connecting all methods:

> *“From z-tests to Bayes, what did I learn about my data story?”*






