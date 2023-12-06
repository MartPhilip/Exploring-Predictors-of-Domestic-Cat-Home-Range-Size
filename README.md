# Intrinsic and extrinsic drivers of home range size in owned domestic cats Felis catus: Insights from a French suburban study
Reproducible Code for the article : "Intrinsic and extrinsic drivers of home range size in owned domestic cats Felis catus: Insights from a French suburban study".

The RCode for statistical analysis, Stat_analysis.R, encapsulates the scripts essential for producing all analyses and figures presented in this article. To execute this code successfully, only two CSV files are required, all.cats.akde.50.csv and all.cats.akde.90.csv, which are included either as supplementary data with the article or within the data folder of this GitHub repository.

The additional codes, namely AKDE_computation.R and BUFFER.R, elucidate the methodologies employed in computing the aKDE at 50% and 95% from GPS data and determining the percentage of vegetation within 100 and 500m buffers surrounding the residences of cat owners, respectively. These scripts generate the two essential CSV files crucial for running the Stat-analysis RCode.

It is imperative to note that raw data necessary for executing AKDE_computation.R and BUFFER.R is not provided herein. This omission is intentional and is rooted in the commitment to safeguard the privacy of cat owner data, as well as data sourced from the Paris Region Institute for Land Use in Île-de-France and METEOFRANCE for climatic information. This precautionary measure ensures the responsible handling of sensitive data while still facilitating the replication of our analyses by interested parties.

