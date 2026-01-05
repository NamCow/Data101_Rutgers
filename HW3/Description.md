# HW3 – Final Data 101 Report (Medium Weight)

**Due:** Nov 30, 2025 by 11:59 PM  
**Points:** 10  
**Submission:** File upload  
**Availability:** Nov 11, 2025 (12:00 AM) – Dec 3, 2025 (11:59 PM)  
**Status:** Assignment locked after Dec 3, 2025 at 11:59 PM  

---

## 🎓 Final Data 101 Project  
### “Can Data Fool Us — or Make Us Famous?”

*(Choose any dataset you love — Spotify hits, Airbnb prices, NBA stats, or your own survey — and show off everything you’ve learned this semester.)*

---

## 1. Languages of Data: Translating Raw Chaos into Meaning

**Goal:**  
Describe the dataset, define variable types (**categorical / numeric**), and perform **at least one data transformation**.

**Example headline:**  
*“From Raw CSV to Insights: How I Tamed 10,000 Airbnb Listings.”*

### Requirements
- Use `table()`, `summary()`, `subset()`, and `tapply()` to show variable distributions
- Create **one new variable**  
  (e.g., `ValueIndex = Price / Size` or `AttendanceLevel = Score × Attendance`)

---

## 2. Exploratory Data Analysis: Seeing the Invisible

**Goal:**  
Make **at least two base-R plots**.

### Requirements
- Use `hist()`, `barplot()`, or `boxplot()`
- Visualize:
  - One **numeric** variable
  - One **categorical** variable

**Click-bait title:**  
*“The Shocking Shape of Happiness: Why Most People Rate 7/10.”*

- Briefly describe:
  - Trends
  - Outliers
  - Clusters

---

## 3. Fooled by Data: When Randomness Plays Tricks

**Goal:**  
Simulate or reveal randomness.

### Requirements
- Shuffle one column and examine whether apparent “patterns” still appear

**Headline:**  
*“Coincidence or Correlation? The Illusion I Almost Believed.”*

**Reflection:**  
- What is one misleading pattern you almost trusted?

---

## 4. Central Limit Theorem & Confidence Intervals

**Goal:**  
Demonstrate sampling behavior.

### Requirements
- Take many random samples of size **n ≥ 30**
- Plot the **distribution of sample means**
- Compute a **95% confidence interval** for a mean or proportion

**Headline:**  
*“How 30 Samples Made Me Believe in the Bell Curve.”*

---

## 5. Hypothesis Testing: When Suspicion Meets Statistics

**Goal:**  
Perform **one statistical test**.

### Requirements
- Use a **z-test** or **permutation test** for a mean
- Compute the **p-value**
- State **H₀ / H₁** and conclusion in **one sentence**

**Headline:**  
*“Do Weekend Bookings Really Cost More? The Verdict Is In.”*

---

## 6. Independence & Difference of Proportions (Chi-Square)

**Goal:**  
Test whether two categorical variables are related.

### Requirements
- Use `chisq.test(table(X, Y))`
- Interpret results:
  - Dependent vs independent
  - Any surprises?

**Headline:**  
*“Men Lie, Women Book Earlier? A Chi-Square Shocker.”*

---

## 7. Multiple Hypothesis Testing: The False Discovery Jungle

**Goal:**  
Run several tests at once and correct for multiple comparisons.

### Requirements
- Apply **Bonferroni** or **Benjamini–Hochberg (BH)** correction
- Report which results remain significant after correction

**Headline:**  
*“Ten Hypotheses Walk into a Bar. Only Three Survive Bonferroni.”*

---

## 8. Bayesian Reasoning: Updating Beliefs

**Goal:**  
Apply **prior × likelihood → posterior** logic.

### Requirements
- Compute **posterior odds** or **posterior probability**
- Use new evidence to update beliefs

**Headline:**  
*“After Seeing One 5-Star Review, I’m 3× More Confident It’s a Good Restaurant.”*

- Explain the intuition in **plain English**

---

## 9. Prediction Models: From Correlation to Prediction

**Goal:**  
Build **at least one predictive model**.

### Requirements
- Use:
  - Linear regression, or
  - `rpart()` decision tree  
  *(optionally try boosting)*
- Report:
  - Accuracy (%) on **training data**
  - Accuracy (%) on **testing data**
- Include **one plot**  
  (`plot(tree)` or variable-importance style summary)

**Headline:**  
*“Can a Tree Outsmart My Gut? Predicting Prices Like a Pro.”*

---

## 10. Association Rules & Lift: The Hidden Recipes

**Goal:**  
Find simple **“if–then”** patterns.

### Requirements
- Use `apriori()` to find rules with **lift > 1**

**Headline:**  
*“Buy Wine → Buy Cheese: Confirmed by Data!”*

---

## 11. Reflection: What I Learned & Why It Matters

### Requirements
- One paragraph on the dataset’s **biggest insight**
- One paragraph on which **concept surprised you most**

---

## 12. Data Tour as GPT Chat

Create a **data tour as a GPT chat**, with the dataset attached,  
just like the one demonstrated in class  
(see **Metaprompts**).

