# Thermal-Sensitivity: noWetlandBuf branch (built off of experimenting branch)
1) Removed WetlandBuf as a landscape EV: resulted in **more** direction mismatches in **9 best** models.
   * **All** 9 models had **1-2** direction mismatches: **Solar** **and/or** **WetlandsRC** was now causing direction mismatches.
2) Removed Solar as an EV too: resulting in **minimizing** direction mismatches in **9 best** models.
3) Ran experimentation script to find **9 best** models using VIFs **20, 15, 10, 5** w/ correlation cut-offs **0.8, 0.7, 0.6**: **results/2021/MLR/comparisonResults/model_comparison_results.csv.**
   * Models w/ VIFs **5** had **1** direction mismatches (WetlandsRC).
   * Similar to **noSolar** model.
