---
editor_options: 
  markdown: 
    wrap: 72
---

# HeatwaveProject

Phase 1 heatwave experiment for masters project

## Project Identification

#### Dataset Repository Link:

<https://github.com/alexandralalor/HeatwaveProject>

#### Publication Associated with Data:

Lalor, A.R., Law, D.J., Breshears, D.D., Falk, D.A., Field, J.P.,
Loehman, R.A., Triepke, F.J., Barron-Gafford, G.A., 2023. Mortality
thresholds of juvenile trees to drought and heatwaves: implications for
forest regeneration across a landscape gradient. Frontiers in Forests
and Global Change 6, 1–16. <https://doi.org/10.3389/ffgc.2023.1198156>

#### Author Contact Information:

-   **Name:** Alexandra Lalor

-   **Phone:** (408) 220-5214

-   **Email:** allielalor\@gmail.com

## Contents of HeatwaveProject repository

#### Important files:

-   README.md
    -   This file! Housed in the "README" folder
-   .gitignore
    -   very large files containing photo information should be ignored
        to prevent issues with committing changes

#### Folders:

-   README folder with .csv files

    -   **Metadata_CollectedValues.csv**
    -   **Metadata_CalculatedValues.csv**

-   Scripts folder with .R files

    -   **scripts**

-   Data folders with .csv files

    -   **data_raw**

    -   **data_clean**

    -   **data_QAQC**

    -   **data_analysis**

-   Photo folders with .jpg files

    -   **Phase1_Photos**

    -   **Phase1_Photos_Background**

-   Graphs folder with a mix of graphing outputs

    -   **graphs**

## How to Use HeatwaveProject:

#### README and Metadata

1.  There are 2 .csv files in the "README" folder. One is titled
    "Metadata_CollectedValues.csv" and the other
    "Metadata_CalculatedValues.csv". Both files contain a descriptoin of
    each column name found in the data folders of this project. The
    CollectedValues.csv file describes data that was collected directly
    (data found in the "data_raw", "data_clean", and "data_QAQC"
    folders). The CalculatedValues.csv file describes data that was
    calculated from the collected data and used in statistical analysis
    (found in the "data_analysis" folder).

#### Scripts folder

1.  enter the "scripts" folder. The order of the folders (1-5) is the
    order you should run the code. You may skip folder 0_colors unless
    there are new photos to extract pixel data from (will need a
    supercomputer).
2.  enter a subfolder in the "scripts" folder (e.g. 1_clean). You will
    see a second level of ordering in the script naming (1_clean_1,
    1_clean_2), followed by a 1-word description of what the script
    does. Run these scripts in order. If a script has the same level of
    ordering, it doesn't matter which one is run first.
3.  each script will contain more detailed notes about each line of code
4.  to re-perform data analysis and statistics only, you may skip
    folders 0_colors, 1_clean, and 2_QAQC (these scripts are to put the
    data in a usable format and quality check the data). You can start
    directly with folders 3_analysis, 4_stats, and 5_viz.
5.  the "old" folder contains exploratory code, not used for analysis

#### Data folders

1.  the "data_raw" folder contains .csv files which were directly
    entered during data collection. It contains unaltered data.
2.  the "data_clean" folder contains .csv files which are in a format
    useful for analysis in R. It contains unaltered data.
3.  the "data_QAQC" folder contains .csv files which were quality
    checked for data errors. The data in these files were altered if an
    error was found. This data was used for final analysis
4.  the "data_analysis" folder contains .csv files with outputs from
    analysis and new columns with summary statistics. It is a repository
    for statistical data output.

#### Photo folders

1.  the folders "Phase1_Photos" and "Phase1_Photos_Background" are to
    house .jpg files to be used for scripts contained in the folder
    "scrips/0_colors". Photos are not backed up on github due to space
    limitations. The data extracted from these photos are housed in the
    data folders, so it shouldn't be necessary to re-process these
    photos.

#### Graphs folder

1.  The "graphs" folder is a repository for graphing outputs from R,
    produced using scrips in the "scripts/5_viz" folder.
