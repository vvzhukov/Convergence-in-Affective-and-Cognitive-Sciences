# Convergence-in-Affective-and-Cognitive-Sciences
This repository contains the R, Python code to curate, ensure quality control, normalize, and generate final model results from the raw data collected
PubMed and other resuorces in the context of a research study.  **Convergence in Affective and Cognitive Sciences**, published in 2023.


## Getting Started

#### Prerequisites
- R and RStudio
- Required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')
	
	
## Script Set
##### Please run the following scripts sequentially

The **Data Curation (dc)** scrips will process the raw data and create the model data. The **Modeling Scripts (ms)** will perform the multy linear regression and produce the visualizations.




##### Note: Please do not run any script after this
-------------------------------------------------------------------------------------------------------------
**Details of Utility Scripts (us)**

The **Utility Scripts (us)** are used to define the common functions like read-write files, and are being called from the **Data Curation (dc)** scripts.  

	- 0_parser.py
	    - Python parser for data collection.
	- 10_iEEE.R
	    - iEEE data collection script.
	- 17_meshtree_ui.py
	    - MeSH tree data decoder.
	    
	    
**Details of Data Curation (dc) & Modeling Scripts (ms)**

	- 0_* -> 17_* files used to collect / transform data, build models and provide desriptive stats
	
	- Descriptive_* files used for data collection and initial analyzis.
	    - Produces the exploratory data visualization
	
	- OldFigures_* files used for previous versions of vizualisations
