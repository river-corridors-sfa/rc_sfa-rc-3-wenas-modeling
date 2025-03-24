# rc_sfa-rc-3-wenas-modeling

#### Data and scripts associated with “Thresholds of Area Burned and Burn Severity for Downstream Riverine Systems to ‘Feel the Burn’”
Katie A Wampler, Kevin D Bladon, Brieanne Forbes, Hyunwoo Kang, Beck Powers-McCormack, Peter Regier, Timothy D Scheibe, and Allison Myers-Pigg

## Summary
This data package is associated with the publication “Thresholds of Area Burned and Burn Severity for Downstream Riverine Systems to ‘Feel the Burn’” submitted to Water Resources Research  (Wampler et al. 2025).  

This study used the Soil and Water Assessment Tool (SWAT), a processed based model to explore the impacts of area burned and burn severity on streamflow, nitrate, and dissolved organic carbon (DOC) in two test basins: a semi-arid, mixed land use basin and a humid, primarily forested basin. We developed 1800 wildfire scenarios that we ran in each basin: 20 different burn extents (5 to 100% by 5%), 3 different burn severities (low, moderate, and high), and 30 different post-fire precipitation scenarios. We also ran an additional 30 scenarios associated with no wildfire for the 30 post-fire precipitation scenarios. For each scenario we were interested in the change in runoff ratio (streamflow) and average concentration and annual loads (nitrate and DOC) across the wildfire scenarios.  

This data package contains the data and scripts required to build SWAT models for the two test basins, create and run the wildfire scenarios, and generate the data summaries and figures used in the associated manuscript. 

This dataset  contains (1) file-level metadata; (2) data dictionary; (3) data package readme; (4) workflow documentation; (5) a folder with model input data (“inputs”); (6) a folder with model output data (“outputs”); and (7) a folder with scripts needed to create and run the models and analyze the outputs (“scripts”). The input data folder contains the following items: (1) a DOC/wildfire module where the DOC outputs are in kilograms per time step (Wampler et al. 2023); (2) a DOC/wildfire module where the DOC outputs are in milligrams per liter; (3) a modified SWAT-CUP (https://www.2w2e.com/home/SwatCup) file with absolute parameter values; (4) a .csv file with calibrated parameter values for both models; (5) a .txt file called “model.in” with updated parameters for the DOC module; and (6) a .pdf with directions for using the wildfire module. The folder with model output data contains three subfolders: (1) “data”, which contains the processed model outputs; (2) “figures”, which contains the figures from the manuscript; and (3) “summary-outputs”, which contains summarized data used to create tables and results for the manuscript. This package contains the following file types: csv, exe, in, txt, pdf, R, png.

## Manuscript Reference
> Wampler, K. A., Myers-Pigg, A. N., Kang, H., Regier, P., Scheibe, T. D., and Bladon, K. D. (2025). Thresholds of Area Burned and Burn Severity for Downstream Riverine Systems to ‘Feel the Burn’. ESS Open Archive [preprint].

## Data Reference
In addition to this repo, the Data Package is published and publicly available on ESS-DIVE. If using these data, please cite the Data Package with the following citation and DOI:  
> Wampler K A ; Bladon K D ; Forbes B ; Kang H ; Powers-McCormack B ; Regier P ; Scheibe T D ; Myers-Pigg A (2025): Data and scripts associated with “Thresholds of Area Burned and Burn Severity for Downstream Riverine Systems to ‘Feel the Burn’”. River Corridor and Watershed Biogeochemistry SFA, ESS-DIVE repository. Dataset. doi:10.15485/2529445 accessed via https://data.ess-dive.lbl.gov/datasets/doi:10.15485/2529445 on 2025-03-24

## Contact
- Katie A. Wampler, katie.wampler@oregonstate.edu  
- Allison Myers-Pigg, allison.myers-pigg@pnnl.gov  
