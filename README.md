# pib-critical-minerals-dashboard

# Methods (Dashboard Construction and Use)
An interactive dashboard was developed using R Shiny to support exploratory and descriptive analysis of Press Information Bureau (PIB) press releases related to critical minerals. The dashboard ingests a cleaned dataset of PIB releases, with each record containing the publication date, issuing ministry, title text, and associated metadata. Dates were parsed and aggregated to daily, monthly, or yearly levels to allow flexible temporal analysis. The application enables users to subset the data by year, issuing ministry, and keyword searches applied to press release titles. Summary indicators display the total number of press releases, the temporal coverage of the selected subset, and the number of ministries represented. A time-series visualization dynamically reflects changes in the applied filters, while an interactive table allows detailed inspection of individual press releases. All filtering and aggregation are performed reactively within the application, ensuring that visualizations and summary statistics update consistently as user selections change. The dashboard was deployed on a public Shiny hosting platform to facilitate transparency, reproducibility, and ease of access for review and demonstration purposes.

## Live Interactive Dashboard
https://mousumib.shinyapps.io/pib_shiny_app/

## Citation
If you use this dashboard or replication materials, please cite:  Baruah, Mousumi. 2025. *PIB Critical Minerals Dashboard*. R Shiny application and replication package. Available at: https://mousumib.shinyapps.io/pib_shiny_app/
