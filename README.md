---
editor_options: 
  markdown: 
    wrap: 72
---

## \# lucidus

`lucidus` is a repository containing visualisation experiments using
data from the [Atlas of Living Australia](https://www.ala.org.au) (ALA).
The primary visualisation of this repository is an interactive Shiny app
that summarises data derived from the EcoAssets project. The
dashboard-like visualisation enables an exploration of spatial,
temporal, taxonomic, provider and basis trends in ALA data from
1900-2020.

The repository is named after the Shining Bronze Cuckoo (*Chrysococcyx
lucidus*), a native Australian bird known for it's 'Shiny' plumage.

### How to Use

To interact with the lucidus project, there are three main scripts.
`data/data_processing.R`, `visualisations/data_visualisation.R` and
`lucidus_shiny/app.R`.

If you simply wish to run and interact with the shiny app then you only
have to run `lucidus_shiny/app.R` as you would for any Shiny app. It
loads in pre-processed data files and stands alone from all other
scripts.

If you wish to explore the raw code used for the visualisations then you
can view and/or run `visualisations/data_visualisation.R`.

`data/data_processing.R` pre-processes the raw data files (in
`.gitignore`) into summarised data-frames and shape-files - there isn't
really any need for a user to run this script as it takes a while to
run, however users may be interested in the data process.

### Shiny app Layout

The summarised data covers five main fields that occurrences are grouped
into unique combinations of: spatial region (which IBRA/IMCRA region),
temporal attributes (year of record), taxonomic attributes (kingdom,
phylum, class and order of record), basis of record, and a binary
classification of data provider (Citizen Science or Non-citizen
Science). Each field has its own plot, except for Year and Data Provider
which are grouped together.

The plots are interactive, meaning you can select aspects of any one
plot (by clicking) and the other plots will update to show the data as
filtered by your click. More than one value can be selected for all
plots except taxonomy. For instance, you may which to select multiple
Tasmanian IBRA regions across the years from 2000-2005. To deselect a
selected value or field, simply click on it again. Unselected values
will be faded.

The sunburst taxonomy plot operates slightly differently - you navigate
up and down levels of taxonomy by either clicking on outer rings (to
drill down) or on the centre selected circle to move back up. Each
Kingdom is assigned its own colour and levels below that will be shades
of that colour. When a taxonomic level is selected, all other plots will
change their colour scheme to match that of the selected taxa. If no
taxonomic level is currently selected, all other plots will be a neutral
blue.

Have fun exploring - if you click too many times too quickly then the
app may get stuck in a loop. In this case simple close it down and
reopen it.

### Repository Structure

There are only a few key directories within the repository, all of which
contain either data or R scripts.

#### `raw-data`

`raw-data` is a .gitignore directory that contains the pre-processed
data files which are too large to be stored with Git. The files in this
directory include the full occurrence parquet files (1900-2020), the
full occurrence summary in csv form, a citizen science allocation
dataset, and lastly shapefiles for IBRA and IMCRA.

#### `data`

The `data` directory contains .rds files used within the shiny app, and
the R script (`data_processing.R`) used to produce these files.
`data_processing.R` loads in the `raw-data/` files and processes them
into .rds objects that are small enough to be stored on git. The
following data files are available in the `data` directory: \*
`aus_outline.rds`: an sf object containing a shapefile of the merged
outline of all IBRA regions \* `oc_summary.rds`: a dataframe containing
summarised counts of occurrences for 2001-05 \* `occ_sumary120.rds`: a
dataframe containing summarised counts of occurrences for 1900-2020 \*
`regions.rds`: an sf object containing shapefiles for all IBRA and IMCRA
regions \* `taxa_colours.rds`: a dataframe created by the `treemap`
library that assigns colours to unique taxa

#### `visualisations`

This directory contains a single script for producing the visualisations
seen in the shiny app. It is easier to explore the visualisation process
here before embedding the plots into the app.

#### `lucidus_shiny`

The directory containing the Shiny app script `app.R`. \*
