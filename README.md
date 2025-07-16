# Thermal-Sensitivity: experimenting branch
1) Created functions that:
   1) generated MLR models based on **parameters**: includedEVs, correlation coefficient cut-off, VIF cut-off.
   2) **Info summaries** for each MLR model.
   3) Tracking if the **direction** was **mismatched** for each MLR (correlation coefficients & standardized beta coefficients from lm beta MLR).
2) Ran experimentation script to find **9 best** models using VIFs **20, 15, 10, 5** w/ correlation cut-offs **0.8, 0.7, 0.6**:
   * **All** 9 models had **1** direction mismatch:
     * Models w/ VIFs **20, 15**: was always **Solar** EV that created direction mismatch.
     * Models w/ VIFs **10**: was always **WetlandBuf** EV that created direction mismatch. (Solar was also not even in model) 
   * Adjusted R squared: models w/ VIFs **10**: significantly **lower** adjusted R squared. 
   * Significant variables: models w/ VIFs **20**: **larger** % of significant EVs/included EVs.
