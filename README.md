# Convergence-in-Affective-and-Cognitive-Sciences
This repository contains the R & Python code to curate, ensure quality control, normalize, and generate final model results from the raw data collected at
PubMed and other resuorces in the context of a research study.  **Convergence in Affective and Cognitive Sciences**, published in 2023.

Data is located at the OSF [link](https://osf.io/v8qxs/).

## Getting Started

#### Prerequisites
- R v. 4.2.2 and RStudio
- Required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')
	

## Script Set
##### Please run the following scripts sequentially

- ./main/*
	    - Main set of files used for raw data filtering, grouping, labeling. N_F* files used to generate visualizations,  N_model* files for model construction.

- ./Descriptive/*
	    - Files used for data collection and initial analyzis.

- ./GPT_affiliations/*
	    - Files used for affiliations labelling, data curation and some affiliations related figures.

- ./Network/*
	    - Files used network graph construction.

- ./OldFigures/*
	    - Files used for previous versions of vizualisations.
  

The **Utility Scripts** are used to define the common functions like read-write files.  

	- 0_parser.py
	    - Python parser for data collection.
	- 10_iEEE.R
	    - iEEE data collection script.
	- 17_meshtree_ui.py
	    - MeSH tree data decoder.
	- other files 
	    - data visualizations
