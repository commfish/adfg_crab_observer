# adfg_crab_observer
## Contact information
Corresponding author: Tyler Jackson  
Email: tyler.jackson@alaska.gov  
Phone: (907) 486 - 1861  
Teleworking number: (810) 488 - 2778

## Purpose
Summarise raw crab observer, dockside sampling, and fish ticket data to estimate fishery removals (i.e., directed catch and bycatch) for Alaskan crab stocks. For details on data reports and summaries for specific stocks, see below.  

## Data sources
Available fishery data includes 1) onboard observer data, 2) dockside sampling of retained catch, and 3) fish ticket data. Observer data is further subdivided into two main catagories: measure pots and count pots. Measure pot data includes counts of crab caught in observer pots by group (e.g., sex, legal status) with the addition of biological characteristics on each crab (e.g., carapace width, shell condition, clutch condition). Count pots include only counts of crab by group (e.g., sex, legal status). Observer and dockside data are provided in three large files by species: 
* SPECIES-1990-2019_crab_dump - measure pot data for a single species caught in all BSAI fisheries in which it was encountered from 1990 - 2019 (present year).
* SPECIES-1990-2019_potsum - count pot data for a single species caught in all BSAI fisheries in which it was encountered from 1990 - 2019 (present year).
* SPECIES-1990-2019_dockside - dockside sampling data for a single species retained in all BSAI fisheries from 1990 - 2019 (present year).  

Fish ticket data summarises retained catch and total known effort (directed and indcidental). Fish ticket summaries are provided by statistical area for each directed fishery in the most recent season, and/or by trip including both directed and incidental effort and catch.

## File structure
Each BSAI crab stock has it's own subdirectory within the main repository. By cloning or downloading the repository, you'll have access to materials related to each stock (you may delete what you like, but it will be restored with each new pull). Raw data for each stock is stored within the stock's subdirectory, in a subdirectory labelled *data*. No data is provided online, instead contact Tyler Jackson (tyler.jackson@alaska.gov) for the most updated data files. Within the stock's subdirectory, you will also find a subdirectory labelled *code* which houses scripts to produce data summary products, and an example workflow markdown document which details the method and code used to create the desired data summaries. All data summary outputs are stored in a separate subdirectory labelled *output* containing an additional subdirectory for the season (example: 2019_20). File paths are sourced using the ```here``` package, so only the relative file structure must remain in tact.  

The *misc* subdirectory contains the subdirectories *code* and *data* which contain custom data manipulation functions and growth parameter estimates used for estimating animal weight, respectively. The *misc* subdirectory **must** be present within the main repository directory.  

An example directory map is as follows:  
* misc
  - code
  - data
* snow_crab
  - data
  - code
  - output
    - 2019_20
* tanner_crab
* bbrkc


