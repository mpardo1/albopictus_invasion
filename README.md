## Code for the paper: Understanding Mosquito Vector Invasion Pathways: Synergistic Effects of Human Mobility, Climate, and Natural Dispersal
This repository contains code used to produce the results and figures for the paper: "Understanding Mosquito Vector Invasion Pathways: Synergistic Effects of Human Mobility, Climate, and Natural Dispersal".

# Data
## Processed Data sets
We have uploaded the processed files required to run the parameter estimation (code inside 1_Hanski) and to generate the figures (code inside 3_Plots) included in the main text and supplementary material of the manuscript. Some of the raw datasets are very large (over 30 GB), and therefore we processed them on a computing cluster. All the processed files are in the directory albopictus_invasion/data/output/. The files are the following:
  - 3_rm_alb_ESP_com_0_2_v2.csv: daily average mosquito reproduction number from 01-01-2005 to 31-12-2023 for each comarca. The first column defines the date. The remaining columns contain the mosquito reproduction number for each comarca. The comarca ID is the number that appears after R0_alb. in each column name.
  - detection_albopictus.csv: csv  with the year of detection (year_detec) for Aedes albopictus for each municipality (NATCODE).
  - pa_com.csv: csv with the year of detection (year_detec) for Aedes albopictus for each comarca (CO_COMARCA).
  - dist_mat_com_ESP.csv: matrix with distances betweem the centroids of the comarca. The names of the rows and columns defines the comarca ID.
  - flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv:  matrix with the daily average human flows between comarcas for the period April 2023 to November 2023. The names of the rows and columns defines the comarca ID.
  - min_temp_yearly_mean_ESP.csv: vector with the average minimum yearly temperature for the period 01-01-2005 to 31-12-2023 for each comarca. The rowname define the id comarca (CO_COMARCA).
  - comarca_mitma_NATCODE_ESP.csv: relationship between the ids of the different regions in Spain (to be able to join different data sets), the id for the comarcas (CO_COMARCA), the id for the mobility regions (id_mitma), the id for the municipalities in Spain (NATCODE).
  - obs_2005-2023.csv: Matrix with the yearly (columns) detection status of Aedes albopictus in each comarca (one comarca per row, identified by CO_COMARCA). Values are 0 (not detected) or 1 (detected).
  - output_mean_tminRM_H_0_2_IC_2004_2025-05-23.csv: matrix with the average yearly occupancy probability predicted by the full model for each comarca. The first column contains the comarca ID, and the remaining columns provide the average occupancy probability for each year. The corresponding year is indicated in the column names.
  - 
## Row data sets
To run the entire code from the beginning (without using the processed files described above) without changing any paths in the albopictus_invasion code, create a folder named data/ inside albopictus_invasion and follow the instructions below:

Download the following data sets:
  - Boundaries relationship: A set of administrative boundaries for Spain used to join different datasets. Run 0_extract_mob_data.R. This automatically downloads all necessary boundary datasets from the Government of Spain and saves them under: data/boundaries/. The datasets are the following:
    - mitma_municip.gpkg: shapefile with the boundaries of the mobility areas with ids id_mitma from the human mobility data set.
    - ine_to_mitma_id_correspondence.csv: csv with the relationship between mobility areas ids (id_mitma) and the Spanish administrative units (id_ine).
  - Human mobility data: Human mobility data between mobility areas in Spain (https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad). Run 0_extract_mob_data.R to obtain the daily human mobility averages. This will create two directories (od_flows/mitma_municip/) where it will save the data. Unzip the file (od_flows/mitma_municip/mean_daily_trips_apr_2023_nov_2023.csv.gz) after downloaded.
  - Past climate data: Download ERA5-Land via the Python API using: 01_download_ERA5LAnd.py. Before running it, create the directories temp and rain inside data. Ignore these directories from git to avoid problems with space limitation.
  - Future climate data: AEMET-CMIP6 Data for future climate change projections for Spain, downscaling data for Spain from the CMIP6 climate change projections data. To download the aemet-cmip6 data in the website (https://archivo-proyecciones-climaticas.aemet.es/) select the following:
    - Dominio: Península y baleares (rejila 5km)
    - Tipo de división: Región completa
    - Región: Región completa
    - Escenarios: SSP2-4.5 (ESD-RegBA) (in the previous version there were only one option for SSP2-4.5 now there are two different downscaling methods all information in: https://escenarios.adaptecca.es/doc/pnacc.pdf#page=12.42)
    - Modelos: Ensemble completo (11 modelos)
    - Variables: Choose each variable and download each.

  - Comarcas shapefile: Shapefile containing the geometries for the comarcas in Spain from the Goverment of Spain. Download the file from https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura by clicking the blue coloured text "Archivo Shapefile de las Comarcas Agrarias de España (7,4 MB)"
  - Population density Spain: csv files with population density at municipality level for Spain from 1996 to 2024. Download it from https://www.ine.es/dynt3/inebase/es/index.htm?padre=525 clicking on the text "(Descargar archivo comprimido con los ficheros excel municipales de cada año a nivel nacional)" unzip the directory (pobmun.zip) in the data directory with the same name.
  - Aedes albopictus detection data in Spain: csv file (File name: InvaMoSP_2004_2024.csv) with detection data for Aedes albopictus in Spain at municipality level. Website to download it: https://zenodo.org/records/15869763

# Code
The code should be run following the numbering in the directories and then the filenames. The file names contains two numbers, the first number indicates the directory and the second number gives the order within that directory. For example, the file 20_phase_space_future.R belongs to the directory 2_phase_space and should be run after all files in 0_Weather_mob_process and 1_Hanski have been completed, the 0 indicates that it is the first script to run within its own directory.
When multiple files share the same number, their order is not important and they can be run in any sequence.

The only script that is not meant to be executed directly is funcR0.R, which is sourced by other R scripts.

The code is structure in 4 main directories:
  - 0_Weather_mob_process: This directory contains the script to download some data and the data process of the data for later use.
  - 1_Hanski: This directory contains the R file (11_input_Hanski_com.R) to process the data for the future parameter estimation, and the julia codes to do the parameter estimation.
  - 2_phase_space: This directory contains the two R files needed to compute the eigenvalues for different combination of the parameters and different scenarios.
  - 3_Plots: This directory contains the different R files two produced the plots in the main and Supplementary material.
    
Before running any code that generates results, make sure you have downloaded all required datasets (all of them are publicly available). Instructions for downloading and saving each dataset are provided in the Data section of this Readme. Some datasets require running specific scripts. In particular, the first scripts to run are 0_extract_mob_data.R and 0_Weather_process/01_download_ERA5Land.py, which download the datasets that are not obtained manually.

After this, run the remaining scripts in 0_Weather_mob_process (except 00_extract_mob_data.R and 01_download_ERA5Land.py), following the numerical order in their filenames. Remember to unzip the file data/od_flows/mitma_municip/mean_daily_trips_apr_2023_nov_2023.csv.gz, which is produced when running 0_Weather_mob_process/0_extract_mob_data.R. Then move to the 1_Hanski directory. Start with 11_input_Hanski_com.R, which creates the input files needed for the parameter estimation code written in Julia.  After that, run the parameter-estimation scripts whose names begin with 12_Param_estimation. These scripts require long execution times, so it is recommended to run them on a computing cluster, preferably with multiple threads. Once they finish, run the final two scripts in the 1_Hanski folder: 13_Full_model_sensitivity_analysis.jl, which performs the sensitivity analysis, and 13_future_integration.jl, which runs the future model integrations.

Next, run the scripts in the 2_phase_space directory. These files do not need to be run in any specific order, but they also require significant computation time and memory, so running them on a cluster is advised. These two files compute the eigenvalues for the different combination of parameters for section Control strategies and climate change scenarios in the main text of the manuscript.

Finally, run the scripts in the 3_Plots directory. These generate all figures for the main text of the manuscript and the supplementary material.
