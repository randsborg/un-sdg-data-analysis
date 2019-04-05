# UN SDG data analysis

This is my first full data science project, and - if all goes well - it will be my capstone project for the [HarvardX Data Science program](https://www.edx.org/professional-certificate/harvardx-data-science). It's currently a work in progress.

## Data

I've used several different sources and fetch methods to practice data collection and wrangling. I might add more to complement analysis or just to experiment.

Data | Fetch method
-------------|--------------
UN Sustainable Development Goals data | [bigrquery](https://cran.r-project.org/web/packages/bigrquery/) - Interface to [BigQuery](https://cloud.google.com/bigquery/)
Gapminder geography data | [googlesheets](https://cran.r-project.org/web/packages/googlesheets/) - Online spreadsheet available via [this link](https://www.gapminder.org/data/geo/)
World Bank population data | [wbstats](https://cran.r-project.org/web/packages/wbstats/) - Interface to World Bank API
Spatial map data | [maps](https://cran.r-project.org/web/packages/maps/index.html) and [mapdata](https://cran.r-project.org/web/packages/mapdata/index.html)

## Map structure
**code:** .R code files.

**figs:** Generated plots and GIFs.

**raw_data:** Downloaded data is saved here.

**tidy_data:** Processed data is saved here.


## Project outline

- [X] Download data
- [X] Tidy data
- [ ] Exploratory data analysis **(current)**
- [ ] Statistical analysis / projection / prediction
- [ ] Write summary report
