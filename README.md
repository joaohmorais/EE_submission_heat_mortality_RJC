# Cause-specific heat-related mortality in Rio de Janeiro city: comparing exposure metrics and the role of exposure duration

João Henrique de Araujo Morais, Valeria Saraceni, Caroline Dias Ferreira, Débora Medeiros de Oliveira e Cruz, Gislani Mateus Aguilar, Oswaldo Gonçalves Cruz.

- `00_aux_functions.R` includes common functions used throughout the analysis in the study.
- `01_descriptive_analysis.R` generates summary tables and descriptive figures of the study results.
- `02_model_structure.R` fits the general model structure used throughout the study and plots its components (trend, seasonality, dow effect).
- `03_model_fitting.R` main file that generates all combinations and fits the DLNM model for Tmed, HImed and number of hours.
- `04_model_results.R` reads from the results from last script and generates visualizations presented in the paper.