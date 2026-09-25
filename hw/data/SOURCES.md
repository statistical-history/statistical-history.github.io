# Homework 3 Data Sources

Retrieved: 2026-09-24 UTC (September 23 in America/New_York)

These files are local snapshots for the Homework 3 normal approximation activity. The CSV files are stored unmodified from the retrieval URLs listed below.

## `us_daily_births_1994_2003.csv`

- Retrieval URL: <https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_1994-2003_CDC_NCHS.csv>
- Publisher documentation: <https://github.com/fivethirtyeight/data/tree/master/births>
- Source/provenance: FiveThirtyEight describes this file as U.S. births data for 1994 to 2003, provided by the Centers for Disease Control and Prevention's National Center for Health Statistics.
- Columns: `year`, `month`, `date_of_month`, `day_of_week`, `births`
- Rows: 3,652 data rows
- Selected variable for activity: `births`
- Selected variable range: 6,443 to 14,540 births
- Missing values in selected variable: 0
- SHA256: `2d0d0c2734468e22a7195f30bfee99885da2322ccf538dbad5d2125d4a06c011`
- License/attribution: FiveThirtyEight states that, unless otherwise noted, its datasets are available under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). Attribute to FiveThirtyEight and CDC/NCHS; this snapshot is unmodified.
- Teaching limitation: Each row is a daily aggregate count, not an individual birth record. The observed sequence also has calendar structure, such as weekday and holiday patterns. In the homework, sampling with replacement from the `births` column creates an artificial independent resampling experiment from the observed finite list; it is not a claim that real calendar days are independent.

## `diamonds.csv`

- Retrieval URL: <https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/diamonds.csv>
- Publisher documentation: <https://ggplot2.tidyverse.org/reference/diamonds.html>
- CSV distributor documentation: <https://vincentarelbundock.github.io/Rdatasets/doc/ggplot2/diamonds.html>
- Source/provenance: The file is a CSV snapshot of the `diamonds` dataset from `ggplot2`. The official ggplot2 documentation describes it as prices and other attributes of almost 54,000 round cut diamonds.
- Columns: `rownames`, `carat`, `cut`, `color`, `clarity`, `depth`, `table`, `price`, `x`, `y`, `z`
- Rows: 53,940 data rows
- Selected variable for activity: `price`
- Selected variable range: 326 to 18,823 US dollars
- Missing values in selected variable: 0
- SHA256: `974c2ce1c1ce245508bd357ca11a7fba2b37813ecf0f1158808a9249ebff67a1`
- License/attribution: ggplot2's license is MIT. The complete copyright and license notice is retained in `DIAMONDS_LICENSE.txt`, downloaded from <https://raw.githubusercontent.com/tidyverse/ggplot2/main/LICENSE.md>. Attribute to ggplot2 and the ggplot2 core developer team; the CSV was retrieved through the Rdatasets mirror.
- Teaching limitation: This is a convenient, real priced sample of diamonds, but it should not be treated as a random sample from all diamonds or all diamond transactions. It is useful here because the `price` distribution is strongly right-skewed and finite.
