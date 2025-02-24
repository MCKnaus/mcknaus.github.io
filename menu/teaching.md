---
layout: page
title: Teaching
---

### Course material

[Causal Machine Learning](https://github.com/MCKnaus/causalML-teaching) (consolidated material of courses taught at Universities Aarhus, Hamburg, Luzern and Tübingen)

### Causal Christmas Tree Challenge
[The Challenge]({{ site.url }}/assets/cctc/CCTC.nb.html)

[Solutions 2022]({{ site.url }}/assets/cctc/Causal_Christmas_Tree_Challenge2022.html)

### Student shinyapps

Check out the [Causal Forest Fun shinyapp](https://marenbmg.shinyapps.io/causalForest/) my Tübinger Master students prepared as group assignment.


### Simulation notebooks

These R Notebooks illustrate concepts and methods with simulated data. You can extract the code by clicking on the top right button and download the Rmd file to run/modify the code.

#### Basics:
- [Basics: Convergence rates]({{ site.url }}/assets/notebooks/SNB/SNB_Convergence_rates.nb)
- [Basics: (Conditional) Independence]({{ site.url }}/assets/notebooks/SNB/SNB_Conditional_Independence.nb.html)
- [Basics: Influence functions explained using OLS]({{ site.url }}/assets/notebooks/SNB/SNB_Influence_Function_OLS.nb.html)

#### Supervised ML:
- [Supervised ML: Overfitting of OLS and value of training vs. test sample]({{ site.url }}/assets/notebooks/SNB/
SNB_OLS_in_vs_out_of_sample.nb.html)
- [Supervised ML: Lasso saves the job of OLS]({{ site.url }}/assets/notebooks/SNB/SNB_Lasso_saves_OLS.nb.html)
- [Supervised ML: Tree-based methods]({{ site.url }}/assets/notebooks/SNB/SNB_Tree_based.nb.html)

#### Causal ML:
- [Causal ML: Why naive model selection fails]({{ site.url }}/assets/notebooks/SNB/SNB_Naive_model_selection.nb.html)
- [Causal ML: Double Selection]({{ site.url }}/assets/notebooks/SNB/SNB_Double_selection.nb.html)
- [Causal ML: Partially linear Double ML]({{ site.url }}/assets/notebooks/SNB/SNB_Partially_linear.nb.html)
- [Causal ML: AIPW Double ML (ATE)]({{ site.url }}/assets/notebooks/SNB/SNB_AIPW_DML.nb.html)
- [Causal ML: Group Average Treatment Effects]({{ site.url }}/assets/notebooks/SNB/SNB_GATE.nb.html)
- [Causal ML: Causal Tree and Causal Forest]({{ site.url }}/assets/notebooks/SNB/SNB_Causal_tree_forest.nb.html)
- [Causal ML: Meta-learner]({{ site.url }}/assets/notebooks/SNB/SNB_Meta_learner.nb.html)
- [Causal ML: Offline policy learning]({{ site.url }}/assets/notebooks/SNB/SNB_Policy_learning.nb.html)
- [Causal ML: Multi-armed bandit]({{ site.url }}/assets/notebooks/SNB/SNB_Bandits.nb.html)


### Application notebooks 

These R notebooks use the *pension* dataset of [Chernozhukov and Hansen (2004)](https://doi.org/10.1162/0034653041811734) that is part of the [hdm](https://cran.r-project.org/web/packages/hdm/index.html) package. Focus is on hand-coding the methods to see how they work.

- [Basics: OLS and Frisch-Waugh]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_OLS_Frisch_Waugh.nb.html)
- [Supervised ML: Lasso]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_Lasso.nb.html)
- [Supervised ML: Tree-based methods]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_Tree_based.nb.html)
- [Causal ML: Double Selection and Partially Linear Double ML]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_Double_selection_and_partially_linear_DML.nb.html)
- [Causal ML: Double ML for average treatment effects]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_AIPW_DML.nb.html)
- [Causal ML: Double ML as generic recipe]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_Generic_DML.nb.html)
- [Causal ML: Double ML for group average treatment effects]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_GATE.nb.html)
- [Causal ML: Predicting effects]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_Predicting_effects.nb.html)
- [Causal ML: Offline policy learning]({{ site.url }}/assets/notebooks/appl401k/ANB_401k_Policy_learning.nb.html)


### Identification notebooks

Showcase the use of DAGs and SWIGs.

- [Causal Inference: DAG and SWIG for RCTs]({{ site.url }}/assets/notebooks/ci/CI_RCT_DAG_SWIG.nb.html)
- [Causal Inference: DAG and SWIG for measured confounding]({{ site.url }}/assets/notebooks/ci/CI_MC_DAG_SWIG.nb.html)

