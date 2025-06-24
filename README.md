# ASAPbio Crowd Preprint Review Evaluation Manuscript

This GitHub repository contains the data and source code used for the following manuscript:

[Li X](https://orcid.org/0000-0003-1796-8555), [Otto J](https://orcid.org/0009-0003-5210-9091), [Elkheir LYM](https://orcid.org/0000-0002-3516-334X), and [Coates JA](https://orcid.org/0000-0001-9039-9219). 2025. Evaluating the ASAPbio Crowd Preprint Review Initiative: Experiences, Feedback, and Future Enhancements . ***[Link TBA]***

This repository is also archived on Zenodo ***[Link TBA]***

<br/>

## Repository structure 

- The [`code/`](code/) directory contains code used in the analysis, incuding those used to generate figures and tables in the manuscript 
- The [`data/`](data/) directory contains pre-processed survey data and data dictionary
  - ***Survey data was recoeded via Google Form/Sheet. Raw data was pre-processed and all free-text entries were removed to protect respondent anonymity.***
- The [`figures_main/`](figures_main/) directory contains Figure 2-6 of the manuscript
- The [`figures_supp/`](figures_supp/) directory contains Supplemental Figure 1-4 of the manuscript
- The [`tables_supp/`](tables_supp/) directory contains Supplemental Table 1-4 of the manuscript

<br/>

## Reproducibility note
Session info obtained using `utils:::print.sessionInfo(sessionInfo()[-8])`
```
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20
Running under: macOS 15.2

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Los_Angeles
tzcode source: internal

attached base packages:
NULL

other attached packages:
 [1] openxlsx_4.2.7.1   scales_1.3.0       ggpubr_0.6.0       geomtextpath_0.1.4 here_1.0.1        
 [6] readxl_1.4.3       lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4       
[11] purrr_1.0.2        readr_2.1.5        tidyr_1.3.1        tibble_3.2.1       ggplot2_3.5.1     
[16] tidyverse_2.0.0   

loaded via a namespace (and not attached):
 [1] gtable_0.3.6      rstatix_0.7.2     tzdb_0.4.0        vctrs_0.6.5       tools_4.4.1      
 [6] generics_0.1.3    parallel_4.4.1    fansi_1.0.6       pkgconfig_2.0.3   lifecycle_1.0.4  
[11] compiler_4.4.1    farver_2.1.2      textshaping_0.4.0 munsell_0.5.1     carData_3.0-5    
[16] Formula_1.2-5     pillar_1.9.0      car_3.1-3         crayon_1.5.3      abind_1.4-8      
[21] tidyselect_1.2.1  zip_2.3.1         stringi_1.8.4     labeling_0.4.3    cowplot_1.1.3    
[26] rprojroot_2.0.4   grid_4.4.1        archive_1.1.9     colorspace_2.1-1  cli_3.6.3        
[31] magrittr_2.0.3    utf8_1.2.4        broom_1.0.7       withr_3.0.2       backports_1.5.0  
[36] bit64_4.5.2       timechange_0.3.0  bit_4.5.0         gridExtra_2.3     ggsignif_0.6.4   
[41] cellranger_1.1.0  ragg_1.3.3        hms_1.1.3         viridisLite_0.4.2 rlang_1.1.4      
[46] Rcpp_1.0.13-1     glue_1.8.0        rstudioapi_0.17.1 vroom_1.6.5       R6_2.5.1         
[51] systemfonts_1.1.0
```
