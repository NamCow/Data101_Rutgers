# Homework 1 – Data Journalism (Medium Weight)

**Due:** Sep 21, 2025 by 11:59 PM  
**Points:** 5.5  
**Submission:** File upload  
**Availability:** Sep 11, 2025 (12:00 AM) – Sep 24, 2025 (11:59 PM)  
**Status:** Assignment locked after Sep 24, 2025 at 11:59 PM

---

## YOU ARE A DATA JOURNALIST

Write a **short article (1–2 pages)** about a selected dataset.

- Your **title must be a clickbait** (exciting, encouraging people to click).
- The title must be **related to your informal data exploration**.
- The article should correspond to **a pattern discovered in your data**.
- You may add **subtitles**, which should also be **data-justified and attention-grabbing**.

### About Clickbait
Examples and discussion of effective clickbait headlines can be found here:  
https://www.searchenginejournal.com/12-surprising-examples-of-clickbait-headlines-that-work/362688/

---

## Dataset Selection

- Select a dataset from **https://www.kaggle.com/datasets** or any other source.
- **Do not use datasets from the active textbook.**
- The dataset must contain:
  - At least **two categorical columns**
  - At least **one numerical column**

Import the dataset into **RStudio**, explore it, and identify **at least three interesting patterns**.

---

## Assignment Instructions

### 1. Start with Visualization and Exploration

Begin your data exploration using visualizations such as:
- Scatter plots
- Barplots
- Boxplots

You may also start by eyeballing a small sample of the data.

📌 **Show all steps in a section titled “My Work.”**

**Example:**  
In class, the Moody dataset was explored using a boxplot of *score vs. grade*. This revealed overlapping score intervals between grades. Further investigation showed that **row number** often determined the grade within those overlapping ranges.

---

### 2. Identify Key Variables

Determine the **key (target) variables** that matter most in a real-world or business sense.

**Examples:**
- Score and Grade (Moody dataset)
- Rental Price (Airbnb dataset)
- IMDb Score (Movies dataset)

Plot this key variable:
- Across the entire dataset
- Across meaningful subsets

Use these plots to guide further investigation.

---

### 3. Queries and Tools

- There is **no minimum number of plots or queries** required.
- Use as many queries and plots as necessary to support your conclusions.

You may use **GPT for research support**, but **R code must be limited to R101 functions only**:

**Allowed functions:**
- `nrow()`
- `c()`
- `subset()`
- `table()`
- `tapply()`
- `min()`
- `max()`
- `mean()`

**Allowed plots:**
- `plot()`
- `barplot()`
- `boxplot()`
- `mosaicplot()`

📌 GPT should be used as a **research tool**, not for copy-paste coding.  
📌 The educational value lies in your **reasoning and analysis**.

---

### 4. Documentation Requirements

Submit the following:

1. **Full list of all GPT prompts used**
2. **R code** as a `.R` file
3. **Article** (Word or PDF), minimum **300 words** (excluding code)

The article must include:
- A **clickbait-style main title** based on the most interesting finding
- **Two alternate titles** for other patterns
- At least **one engaging subsection**
- **Plots with supporting explanations**
- A **“My Work” section** explaining how conclusions were reached

---

### 5. Writing the Article

Turn your **most surprising or actionable pattern** into the main title.  
Other discoveries can be used as alternate titles.

**Examples of strong titles and subtitles:**

- *“This One Chart Reveals Why Tuesdays Are the Best Party Nights—You Won’t Believe the Data!”*  
  **Subsection:** Why DJs Love Midweek: Attendance Spikes Explained

- *“How a Hidden Pattern in GPA Predicts Career Success—Are You in the Top Tier?”*  
  **Subsection:** The Surprising Link Between Seniority and Grade Trends

- *“The Shocking Truth About Airbnb Prices in Your City—Is Your Neighborhood Overpriced?”*  
  **Subsection:** Neighborhood Wars: The Most Expensive Spots Revealed

- *“IMDb Ratings Don’t Lie: Which Genres Get Snubbed and Which Score the Highest?”*  
  **Subsection:** The Mystery of Underrated Thrillers vs. Overrated Comedies

---

### 6. What Counts as “Interesting”

An interesting pattern is typically:
- **Unexpected** (different from initial assumptions)
- **Actionable** (useful for people, businesses, or decision-making)

