## Code for the paper: Understanding Mosquito Vector Invasion Pathways: Synergistic Effects of Human Mobility, Climate, and Natural Dispersal
This repository contains code used to produce the results and figures for the paper: "Understanding Mosquito Vector Invasion Pathways: Synergistic Effects of Human Mobility, Climate, and Natural Dispersal".

# Data
Here is the explanation of how to download the raw data sets. We have also added several processed data files to allow you to run parts of the code directly. Downloading and processing some of the raw files is very costly in terms of execution time and memory, so the processed files are provided to make the workflow more efficient.

## Raw data sets
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

  - Comarcas shapefile: Shapefile containing the geometries for the comarcas in Spain from the Goverment of Spain. Download the file from https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura by clicking the blue coloured text "Archivo Shapefile de las Comarcas Agrarias de España (7,4 MB)". 
  - Population density Spain: csv files with population density at municipality level for Spain from 1996 to 2024. Download it from https://www.ine.es/dynt3/inebase/es/index.htm?padre=525 clicking on the text "(Descargar archivo comprimido con los ficheros excel municipales de cada año a nivel nacional)" unzip the directory (pobmun.zip) in the data directory with the same name.
  - Aedes albopictus detection data in Spain: csv file (File name: InvaMoSP_2004_2024.csv) with detection data for Aedes albopictus in Spain at municipality level. Website to download it: https://zenodo.org/records/15869763

## Processed Data sets
In this section we explain the processed files and how to download one file needed for the execution of the parameter estimation (code inside 1_Hanski), the phase space (code inside 2_phase_space) and to generate the figures (code inside 3_Plots) included in the main text and supplementary material of the manuscript. All the processed files are in the directory albopictus_invasion/data/output/. 

The processed files in the data/output/ folder are the following:
  - ComarcasAgrarias.shp/shx/sbx/sbn/sbx/prj/dbf: All the files related to the Comarcas shapefile download from https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura (see Raw Data sets Section above for more details) and owned by the Ministerio de Agricultura, Pesca y Alimentación from the Spanish Goverment.
  - 3_rm_alb_ESP_com_0_2_v2.csv: It contains the daily average mosquito reproduction number from 01-01-2005 to 31-12-2023 for each comarca. The first column defines the date. The remaining columns contain the mosquito reproduction number for each comarca. The comarca ID is the number that appears after R0_alb. in each column name.
  - comarca_mitma_NATCODE_ESP.csv: data processed from the Spanish Goverment boundaries data. It contains the relationship between the ids of the different regions in Spain (to be able to join different data sets), the id for the comarcas (CO_COMARCA), the id for the mobility regions (id_mitma), the id for the municipalities in Spain (NATCODE).
  - detection_albopictus.csv: csv  with the year of detection (year_detec) for Aedes albopictus for each municipality (NATCODE).
  - dist_mat_com_ESP.csv: data processed from the comarcas shapefile from the Spanish Goverment. It contains a matrix with distances betweem the centroids of the comarca. The names of the rows and columns defines the comarca ID.
  - flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv:  matrix with the daily average human flows between comarcas for the period April 2023 to November 2023. The names of the rows and columns defines the comarca ID.
  - flows_com_df.csv: data frame with average daily human mobility flows from each comarca for the period period April 2023 to November 2023. The first column (CO_COMARCA_origin) defines the id comarca of the origin, the second column (CO_COMARCA_destination) defines the id comarca of the destination, and the third column (mean_n_trips) that defines the average daily human mobility flows.
  - min_temp_ESP.csv: data processed from the ERA5 Land data. This csv contains a matrix with the minimum daily temperatures for the period 01-01-2005 to 31-12-2023 for each comarca, with the first column the date and the next column each of the comarcas with the number after the X in the column name defining the comarca ID (CO_COMARCA).
  - min_temp_ESP_future.csv: data processed from the AEMET-CMIP6 data sets. The first column define the date and next columns define the daily minimum temperature for each comarca. The id of the comarca (CO_COMARA) is the number after the X in the name of the column.
  - min_temp_yearly_mean_ESP.csv: data processed from the ERA5 Land data.vector with the average minimum yearly temperature for the period 01-01-2005 to 31-12-2023 for each comarca. The rowname define the id comarca (CO_COMARCA).
  - natural_dispersal_one_year_2025-05-28.csv: occupancy probability for the full model parameters with only natural dispersal for one year. The first row determines the id comarca (CO_COMARCA) and the second row the occupancy probability (out).
  - obs_2005-2023.csv: data processed from the detection data in Zenodo. The csv contains a matrix with the yearly (columns) detection status of Aedes albopictus in each comarca (one comarca per row, identified by CO_COMARCA). Values are 0 (not detected) or 1 (detected).
  - output_mean_tminRM_H_0_2_IC_2004_2025-05-23.csv: Matrix generated by the 12_Param_estimation_full_model.jl. This csv contains a matrix with the average yearly occupancy probability predicted by the Full model for each comarca. The first column contains the comarca ID, and the remaining columns provide the average occupancy probability for each year. The corresponding year is indicated in the column names.
  - output_estimation_dist_meanRM_tmin_H_0_2_2025-05-23.csv: Matrix generated by the 12_Param_estimation_natural_dispersal_model.jl. This csv contains a matrix with the average yearly occupancy probability predicted by the Natural dispersal model for each comarca. The first column contains the comarca ID, and the remaining columns provide the average occupancy probability for each year. The corresponding year is indicated in the column names.
  - output_estimation_hummob_meanrm__IC_2004_tmin_H_0_2_2025-05-23.csv: Matrix generated by the 12_Param_estimation_human-mediated_model.jl. This csv contains a matrix with the average yearly occupancy probability predicted by the Human-mediated dispersal model for each comarca. The first column contains the comarca ID, and the remaining columns provide the average occupancy probability for each year. The corresponding year is indicated in the column names.
  - output_estimation_pop_growth_meanRMtmin_H_0_2_2025-05-23.csv: Matrix generated by the 12_Param_estimation_climate_driven_model.jl. This csv contains a matrix with the average yearly occupancy probability predicted by the Climate-driven model for each comarca. The first column contains the comarca ID, and the remaining columns provide the average occupancy probability for each year. The corresponding year is indicated in the column names.
  - output_mean_tminRM_H_0_2_nodist_IC_2004_2025-05-23.csv: Matrix generated by the 12_Param_estimation_full_model.jl. This csv contains the occupancy probabilities for the knock scenarios for the scenario with no human mediated dispersal. The first column contains the comarca ID, and the remaining columns provide the average occupancy probability for each year. The corresponding year is indicated in the column names.
  - output_mean_tminRM_H_0_2_nohum_IC_2004_2025-05-23.csv: Matrix generated by the 12_Param_estimation_full_model.jl. This csv contains the occupancy probabilities for the knock scenarios for the scenario with no natural dispersal. The first column contains the comarca ID, and the remaining columns provide the average occupancy probability for each year. The corresponding year is indicated in the column names.
  - pa_com.csv: data processed from the detection data in Zenodo. The csv contains the year of detection (year_detec) for Aedes albopictus for each comarca (CO_COMARCA).
  - rm_alb_ESP_com_future.csv: It contains the daily average mosquito reproduction number from 01-01-2025 to 31-12-2100 for each comarca. The first column defines the date. The remaining columns contain the mosquito reproduction number for each comarca. The comarca ID is the number that appears after X. in each column name.
  - low_sig_high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv: it contains future predictions under climate change projections for control strategy d figure 4 main text. The first column define the id comarca (CO_COMARCA) and the following columns define the summer mean occupancy probability for each year, the year is the name of the column.
  - low_sig_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv: it contains future predictions under climate change projections for control strategy c figure 4 main text. The first column define the id comarca (CO_COMARCA) and the following columns define the summer mean occupancy probability for each year, the year is the name of the column.
  - low_sig_high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv: it contains future predictions under climate change projections for control strategy b figure 4 main text. The first column define the id comarca (CO_COMARCA) and the following columns define the summer mean occupancy probability for each year, the year is the name of the column.
  - com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv: it contains future predictions under climate change projections for control strategy a figure 4 main text. The first column define the id comarca (CO_COMARCA) and the following columns define the summer mean occupancy probability for each year, the year is the name of the column.
  - min_temp_yearly_mean_fut_ESP.csv: vector with average minimum yearly temperature for the future climate change projection from 2025 to 2050 for each comarca.
# Code
The code should be run following the numbering in the directories and then the filenames. The file names contains two numbers, the first number indicates the directory and the second number gives the order within that directory. For example, the file 20_phase_space_future.R belongs to the directory 2_phase_space and should be run after all files in 0_Weather_mob_process and 1_Hanski have been completed, the 0 indicates that it is the first script to run within its own directory.
When multiple files share the same number, their order is not important and they can be run in any sequence.

The only script that is not meant to be executed directly is funcR0.R, which is sourced by other R scripts.

Before running any code that generates results, make sure you have downloaded all required datasets (all of them are publicly available). Instructions for downloading and saving each dataset are provided in the Data section of this Readme. Some datasets require running specific scripts. In particular, the first scripts to run are 0_extract_mob_data.R and 0_Weather_process/01_download_ERA5Land.py, which download the datasets that are not obtained manually.

After this, run the remaining scripts in 0_Weather_mob_process (except 00_extract_mob_data.R and 01_download_ERA5Land.py), following the numerical order in their filenames. Remember to unzip the file data/od_flows/mitma_municip/mean_daily_trips_apr_2023_nov_2023.csv.gz, which is produced when running 0_Weather_mob_process/0_extract_mob_data.R. Then move to the 1_Hanski directory. Start with 11_input_Hanski_com.R, which creates the input files needed for the parameter estimation code written in Julia.  After that, run the parameter-estimation scripts whose names begin with 12_Param_estimation. These scripts require long execution times, so it is recommended to run them on a computing cluster, preferably with multiple threads. Once they finish, run the final two scripts in the 1_Hanski folder: 13_Full_model_sensitivity_analysis.jl, which performs the sensitivity analysis, and 13_future_integration.jl, which runs the future model integrations.

Next, run the scripts in the 2_phase_space directory. These files do not need to be run in any specific order, but they also require significant computation time and memory, so running them on a cluster is advised. These two files compute the eigenvalues for the different combination of parameters for section Control strategies and climate change scenarios in the main text of the manuscript.

Finally, run the scripts in the 3_Plots directory. These generate all figures for the main text of the manuscript and the supplementary material.

## Code description
The code is structure in 4 main directories:
  - 0_Weather_mob_process: This directory contains the scripts for downloading climatic data, human mobility data, and regional boundaries, as well as processing them for later use.
      - 00_extract_mob_data.R: R code to download and extract human mobility data from the Spanish government website (explained in more detail in the Raw Data Sets section below), as well as to extract the files containing regional boundaries and their relationships.
      - 01_download_ERA5LAnd.py: Python ode to download the ERA5 Land data from the API. This code may take a very long time to run and can consume a large amount of memory.
      - 01_process_aemet_cmip6.R: R code to process future climate data from the AEMET website. This script requires very large input files and may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 02_process_ERa5_monthlydata_prec.R: R code to proces precipitation data download in 01_download_ERA5LAnd.py. This script requires very large input files and may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 02_process_ERa5_monthlydata.R: R code to proces temperature data download in 01_download_ERA5LAnd.py. This script requires very large input files and may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 03_join_monthly_ERA5_data_to_year.R: R code to join data processed in 02_process_ERa5_monthlydata_prec.R and 02_process_ERa5_monthlydata.R.
      - 04_matrix_min_temp.R: R code to compute the matrix with minimum temperatures from the ERA5 Land data.
  - 1_Hanski: This directory contains the R script (11_input_Hanski_com.R) used to process the data for subsequent parameter estimation. Also, it contains the Julia code used to perform the parameter estimation for the different models, the knock-out scenarios, the future climate change predictions and the sensitivity analysis.
      - 11_input_Hanski_com.R: R code to process the data for the parameter estimation of the metapopulation model.
      - 12_Param_estimation_climate_driven_model.jl: Julia code that performs parameter estimation for the Climate-driven metapopulation model. This script may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 12_Param_estimation_full_model.jl: Julia code that performs parameter estimation for the Full metapopulation model. This script may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 12_Param_estimation_human-mediated_model.jl: Julia code that performs parameter estimation for the Human-mediated metapopulation model. This script may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 13_Full_model_sensitivity_analysis.jl: Julia code that performs the sensitivity analysis. This script may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 13_future_integration.jl: Julia code that performs the Full model integration for future climate change projections.
  - 2_phase_space: This directory contains the two R files needed to compute the eigenvalues for the different combination of the parameters and different scenarios.
      - 20_phase_space_future.R: R code to compute the eigenvalues for the climate change scenario. This script produces very large output files and may take a long time to run, so it is recommended to execute it on a computing cluster.
      - 20_phase_space_pres.R: R code to compute the eigenvalues for the constant climate scenario. This script produces very large output files and may take a long time to run, so it is recommended to execute it on a computing cluster.
  - 3_Plots: This directory contains the different R files two produced the plots in the main and Supplementary material.
      - 31_panel_1.R: R code to produces Figure 1 in the main text.
      - 32_panel_2.R: R code to produces Figure 2 in the main text and Figure S1, S2 Supplementary material.
      - 33_panel_3.R: R code to produces Figure 3 in the main text.
      - 34_panel_4.R: R code to produces Figure 4 in the main text and Figure S9 Supplementary material.
      - 35_Supp_plots.R: R code to create remaining Supplementary figures.
    

