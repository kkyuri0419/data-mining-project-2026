# Data Mining Project Template

This repository contains the code for a small data mining project developed as part of the course:

**Data Access and Data Mining for Social Sciences**

University of Lucerne

Student Name : Yuri Kim  
Course: Data Mining for the Social Sciences using R  
Term: Spring 2026

## Project Goal

The goal of this project is to collect and analyze data from an online source (API or web scraping) in order to answer a research question relevant to political or social science.

The project should demonstrate:

- Identification of a suitable data source
- Automated data collection (API or scraping)
- Data cleaning and preparation
- Reproducible analysis


## Research Question

- RQ1. Do ideologically distinct YouTube channels exhibit systematic differences in their language use when discussing a shared political issue?
This question focuses on identifying semantic and emotional variation across channels, examining whether differences in vocabulary, phrasing, and sentiment reflect underlying ideological positions.

- RQ2. Do these linguistic differences correspond to structural clustering in the channel network?
This question investigates whether channels that are linguistically similar are also structurally connected, forming ideologically cohesive communities within the network.


## Data Source


- API: https://developers.google.com/youtube/v3
- Documentation: https://developers.google.com/youtube/v3/docs?hl=ko
- Access method: HTTP GET requests


## Repository Structure

- /data_preprocessed : data that are preprocessed from the raw data for analysis
- /data_raw : output datasets (not tracked/pushed by git)
- /report : final report
- /figures : visualizations
- /scripts : R scripts for data collection, cleaning, and analysis
- README.md : project description


## Reproducibility

To reproduce this project:

1. Clone the repository
2. Install required R packages : Version 2026.01.0+392 (2026.01.0+392)
3. Run the scripts in the `scripts/` folder
4. 'youtube_data_mining.R' should run from start to finish, generating all necessary datasets for the other scripts.

All data should be generated automatically by the scripts.


## Good Practices

Please follow these guidelines:

- Do **not upload raw datasets** to GitHub.
- Store **API keys outside the repository** (e.g., environment variables).
- Write scripts that run **from start to finish**.
- Commit your work **frequently**.
- Use **clear commit messages**.

Example commit messages:
added API request
cleaned dataset structure
added visualization
fixed JSON parsing


## Notes

Large datasets should not be pushed to GitHub.  
If necessary, provide instructions for downloading the data instead.
