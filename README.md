# dengue-forecasting

Code and data assets for modeling and forecasting dengue incidence with statistical and time-series models. The repository is organized around three analysis scopes: Rio de Janeiro, Joinville/Santa Catarina, and Brazilian state capitals.

## Repository structure

```text
.
|-- code/
|   |-- get_data.py                 # Download dengue and climate data with mosqlient
|   |-- aggregate_climate_data.R    # Aggregate daily climate data to epidemiological weeks
|   |-- merge_data.R                # Merge dengue, climate, and spatial data
|   |-- aux_func.r                  # Forecasting, plotting, and evaluation helpers
|   |-- ar_p.stan                   # Bayesian autoregressive model used by cmdstanr
|   |-- rio_de_janeiro/             # Rio de Janeiro data prep, EDA, models, evaluation
|   |-- joinville/                  # Joinville and Santa Catarina data prep, models, evaluation
|   `-- capital_cities/             # State-capital pipelines and per-state SARIMAX scripts
|-- data/
|   |-- environ_vars.csv
|   |-- rio_de_janeiro/             # Rio de Janeiro dengue, climate, and merged inputs
|   |-- joinville/                  # Joinville/Santa Catarina dengue, climate, and spatial inputs
|   `-- capital_cities/             # Per-UF capital city dengue, climate, and prepared inputs
|-- results/
|   |-- rio_de_janeiro/             # Fitted models, plots, and metrics for Rio de Janeiro
|   |-- joinville/                  # Fitted models, plots, and metrics for Joinville
|   `-- capital_cities/             # Per-UF selected model outputs
|-- presentations/
|   `-- capital_cities_presentation.tex  # Beamer deck for capital-city results
|-- vignettes/
|   |-- rio_de_janeiro/             # R Markdown report and rendered HTML
|   `-- joinville/                  # R Markdown report and rendered HTML
|-- LICENSE
`-- README.md
```

## Main workflow

Run scripts from the repository root because most paths are relative to the project directory.

1. Download raw dengue and climate data:

   ```bash
   python code/get_data.py
   ```

   This writes raw CSV files into `data/rio_de_janeiro/`, `data/joinville/`, and `data/capital_cities/`. Configure a valid Mosqlimate API key before running data downloads.

2. Aggregate climate observations to epidemiological weeks:

   ```bash
   Rscript code/aggregate_climate_data.R
   ```

3. Merge dengue, climate, and spatial data:

   ```bash
   Rscript code/merge_data.R
   ```

4. Create lagged climate features and model-ready files:

   ```bash
   Rscript code/rio_de_janeiro/prepare_data.r
   Rscript code/joinville/prepare_data.r
   Rscript code/capital_cities/prepare_data.r
   ```

5. Fit models for each analysis scope:

   ```bash
   Rscript code/rio_de_janeiro/fit_inla.R
   Rscript code/rio_de_janeiro/fit_ar.r
   Rscript code/rio_de_janeiro/fit_sarimax.r

   Rscript code/joinville/fit_inla.r
   Rscript code/joinville/fit_ar.R
   Rscript code/joinville/fit_sarimax.r

   Rscript code/capital_cities/fit_inla.r
   Rscript code/capital_cities/fit_ar.R
   Rscript code/capital_cities/fit_sarimax.r
   ```

6. Evaluate models and generate summary metrics/figures:

   ```bash
   Rscript code/rio_de_janeiro/eval_models.r
   Rscript code/joinville/eval_models.r
   Rscript code/capital_cities/eval_models.r
   ```

## Models

The local Rio de Janeiro and Joinville pipelines compare three model families:

- INLA models saved as `results_M0.rds` through `results_M7.rds`.
- Autoregressive models using `code/ar_p.stan`, saved as `results_M8.rds`.
- SARIMAX baselines and climate-augmented variants, saved as `results_M9.rds` through `results_M13.rds`.

The capital-cities pipeline stores selected per-state outputs as `.qs2` files under `results/capital_cities/<UF>/`.

## Data and outputs

- `data/*/dengue_*.csv` stores dengue incidence time series.
- `data/*/climate_*.csv` stores climate inputs.
- `data/*/*_weekly.csv` stores weekly aggregated climate variables.
- `data/*/dengue_climate_*` stores merged dengue-climate modeling inputs.
- `results/*/summary_metrics.csv`, `mae_*`, `rmse_*`, and `mape_*` contain evaluation metrics.
- `presentations/` stores slide decks and generated presentation files.
- `vignettes/*/report.Rmd` contains reproducible reports, with rendered `report.html` files committed alongside generated figures.

## Presentation

The capital-cities Beamer deck is available at `presentations/capital_cities_presentation.tex`. If you want to compile it with the current relative paths, run LaTeX from the `presentations/` directory.

## Dependencies

The project uses R, Python, Stan, and geospatial tooling. Key R packages include `tidyverse`, `slider`, `sf`, `geobr`, `INLA`, `cmdstanr`, `posterior`, `forecast`, `TSA`, `zoo`, `pbapply`, `parallel`, `patchwork`, `qs2`, and `ggplot2`. Python data retrieval uses `pandas`, `tqdm`, and `mosqlient`.

Some modeling scripts are computationally intensive and create large `.rds`/`.qs2` outputs. Review the parallel worker settings in the fitting scripts before running them on a new machine.
