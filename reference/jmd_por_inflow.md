# JMD Period of Record Inflow

Daily inflow at JMD for the available period of record.

## Usage

``` r
jmd_por_inflow
```

## Format

A data frame with 40,908 rows and 4 columns:

- timestep:

  Timestep index

- date:

  Date

- time:

  Time

- flow_cfs:

  Daily average inflow (cfs)

## Examples

``` r
sapply(jmd_por_inflow,class)
#>    timestep        date        time    flow_cfs 
#>   "integer" "character" "character"   "numeric" 
head(jmd_por_inflow)
#>   timestep      date time flow_cfs
#> 1        1 10/1/1912 0:00      105
#> 2        2 10/2/1912 0:00       80
#> 3        3 10/3/1912 0:00       80
#> 4        4 10/4/1912 0:00      105
#> 5        5 10/5/1912 0:00      120
#> 6        6 10/6/1912 0:00      190
plot(as.Date(jmd_por_inflow$date,tryFormats = c("%m/%d/%Y")), jmd_por_inflow$flow_cfs,
      xlab = "Year", ylab = "Inflow (cfs)",
      type = "l")
```
