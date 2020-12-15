# Data Wrangling and Analysis - Tugas Komstat

## Import Library dan Deklarasi Function


``` r
library(dplyr)
library(ggplot2)
library(nycflights13)
library(lubridate)
library(corrplot)
library(reshape2)
library(knitr)
```

### Fungsi `cekNa`

Fungsi ini berguna untuk menghitung jumlah NA pada setiap kolom

``` r
cekNa <- function (data){
      sapply(data, function(x) sum(is.na(x)))      
}
```

### Fungsi `convertToDate`

Fungsi ini berguna untuk menggabungkan tanggal yang terpisah

``` r
convertToDate <- function(year, month, day, hour_minute){
      ymd_hm(paste(paste(year, month, day, sep = "-"), 
                   paste(floor(hour_minute / 100), hour_minute %% 100, sep = ":")))
}
```

# Data Cleaning dan Integration


Flight data merupakan data penerbangan dari tanggal 1 Januari 2013
sampai 31 Desember 2013 di 3 bandara yang ada di New York City, yaitu
Bandara International John F Kennedy (JFK), Bandara La Guardia (LGA) dan
Bandara International Newark Liberty (EWR). Data ini berasal dari
library
[`nycflights13`](https://cran.r-project.org/web/packages/nycflights13/nycflights13.pdf).

``` r
data("flights")
kable(sample_n(flights, 7))
```

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 8%" />
<col style="width: 5%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 2%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 2%" />
<col style="width: 4%" />
<col style="width: 11%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">year</th>
<th style="text-align: right;">month</th>
<th style="text-align: right;">day</th>
<th style="text-align: right;">dep_time</th>
<th style="text-align: right;">sched_dep_time</th>
<th style="text-align: right;">dep_delay</th>
<th style="text-align: right;">arr_time</th>
<th style="text-align: right;">sched_arr_time</th>
<th style="text-align: right;">arr_delay</th>
<th style="text-align: left;">carrier</th>
<th style="text-align: right;">flight</th>
<th style="text-align: left;">tailnum</th>
<th style="text-align: left;">origin</th>
<th style="text-align: left;">dest</th>
<th style="text-align: right;">air_time</th>
<th style="text-align: right;">distance</th>
<th style="text-align: right;">hour</th>
<th style="text-align: right;">minute</th>
<th style="text-align: left;">time_hour</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">14</td>
<td style="text-align: right;">1920</td>
<td style="text-align: right;">1930</td>
<td style="text-align: right;">-10</td>
<td style="text-align: right;">2210</td>
<td style="text-align: right;">2213</td>
<td style="text-align: right;">-3</td>
<td style="text-align: left;">DL</td>
<td style="text-align: right;">1715</td>
<td style="text-align: left;">N323NB</td>
<td style="text-align: left;">LGA</td>
<td style="text-align: left;">MSY</td>
<td style="text-align: right;">189</td>
<td style="text-align: right;">1183</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">30</td>
<td style="text-align: left;">2013-01-14 19:00:00</td>
</tr>
<tr class="even">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">1647</td>
<td style="text-align: right;">1648</td>
<td style="text-align: right;">-1</td>
<td style="text-align: right;">1823</td>
<td style="text-align: right;">1824</td>
<td style="text-align: right;">-1</td>
<td style="text-align: left;">UA</td>
<td style="text-align: right;">627</td>
<td style="text-align: left;">N803UA</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">ORD</td>
<td style="text-align: right;">115</td>
<td style="text-align: right;">719</td>
<td style="text-align: right;">16</td>
<td style="text-align: right;">48</td>
<td style="text-align: left;">2013-01-06 16:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">30</td>
<td style="text-align: right;">647</td>
<td style="text-align: right;">650</td>
<td style="text-align: right;">-3</td>
<td style="text-align: right;">843</td>
<td style="text-align: right;">908</td>
<td style="text-align: right;">-25</td>
<td style="text-align: left;">UA</td>
<td style="text-align: right;">343</td>
<td style="text-align: left;">N574UA</td>
<td style="text-align: left;">LGA</td>
<td style="text-align: left;">DEN</td>
<td style="text-align: right;">207</td>
<td style="text-align: right;">1620</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">50</td>
<td style="text-align: left;">2013-08-30 06:00:00</td>
</tr>
<tr class="even">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">26</td>
<td style="text-align: right;">709</td>
<td style="text-align: right;">710</td>
<td style="text-align: right;">-1</td>
<td style="text-align: right;">950</td>
<td style="text-align: right;">1000</td>
<td style="text-align: right;">-10</td>
<td style="text-align: left;">AA</td>
<td style="text-align: right;">2493</td>
<td style="text-align: left;">N640AA</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">MCO</td>
<td style="text-align: right;">128</td>
<td style="text-align: right;">944</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-09-26 07:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">28</td>
<td style="text-align: right;">608</td>
<td style="text-align: right;">615</td>
<td style="text-align: right;">-7</td>
<td style="text-align: right;">734</td>
<td style="text-align: right;">729</td>
<td style="text-align: right;">5</td>
<td style="text-align: left;">EV</td>
<td style="text-align: right;">3824</td>
<td style="text-align: left;">N13994</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">IAD</td>
<td style="text-align: right;">48</td>
<td style="text-align: right;">212</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">15</td>
<td style="text-align: left;">2013-08-28 06:00:00</td>
</tr>
<tr class="even">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">11</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">2125</td>
<td style="text-align: right;">2010</td>
<td style="text-align: right;">75</td>
<td style="text-align: right;">2232</td>
<td style="text-align: right;">2122</td>
<td style="text-align: right;">70</td>
<td style="text-align: left;">B6</td>
<td style="text-align: right;">418</td>
<td style="text-align: left;">N373JB</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">BOS</td>
<td style="text-align: right;">37</td>
<td style="text-align: right;">187</td>
<td style="text-align: right;">20</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-11-07 20:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">18</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">1721</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">1926</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">EV</td>
<td style="text-align: right;">3847</td>
<td style="text-align: left;">N14960</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">IND</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">645</td>
<td style="text-align: right;">17</td>
<td style="text-align: right;">21</td>
<td style="text-align: left;">2013-06-18 17:00:00</td>
</tr>
</tbody>
</table>

Pada library `nycflights13` tersedia juga data lainnya yang mendukung
data flight tersebut. 1. Data weather, yaitu data yang berisi tentang
cuaca pada tiga bandara tersebut selama tahun 2013

``` r
data("weather")
kable(sample_n(weather, 7))
```

<table style="width:100%;">
<colgroup>
<col style="width: 5%" />
<col style="width: 4%" />
<col style="width: 5%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 7%" />
<col style="width: 9%" />
<col style="width: 8%" />
<col style="width: 5%" />
<col style="width: 7%" />
<col style="width: 5%" />
<col style="width: 17%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">origin</th>
<th style="text-align: right;">year</th>
<th style="text-align: right;">month</th>
<th style="text-align: right;">day</th>
<th style="text-align: right;">hour</th>
<th style="text-align: right;">temp</th>
<th style="text-align: right;">dewp</th>
<th style="text-align: right;">humid</th>
<th style="text-align: right;">wind_dir</th>
<th style="text-align: right;">wind_speed</th>
<th style="text-align: right;">wind_gust</th>
<th style="text-align: right;">precip</th>
<th style="text-align: right;">pressure</th>
<th style="text-align: right;">visib</th>
<th style="text-align: left;">time_hour</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">LGA</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">16</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">33.80</td>
<td style="text-align: right;">30.92</td>
<td style="text-align: right;">91.66</td>
<td style="text-align: right;">70</td>
<td style="text-align: right;">8.05546</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">0.12</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">6</td>
<td style="text-align: left;">2013-01-16 05:00:00</td>
</tr>
<tr class="even">
<td style="text-align: left;">LGA</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">13</td>
<td style="text-align: right;">32.00</td>
<td style="text-align: right;">12.92</td>
<td style="text-align: right;">44.74</td>
<td style="text-align: right;">310</td>
<td style="text-align: right;">14.96014</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1016.8</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-01-02 13:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">LGA</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">3</td>
<td style="text-align: right;">66.02</td>
<td style="text-align: right;">57.92</td>
<td style="text-align: right;">75.12</td>
<td style="text-align: right;">320</td>
<td style="text-align: right;">4.60312</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1018.4</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-06-09 03:00:00</td>
</tr>
<tr class="even">
<td style="text-align: left;">LGA</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">19</td>
<td style="text-align: right;">80.06</td>
<td style="text-align: right;">53.96</td>
<td style="text-align: right;">40.54</td>
<td style="text-align: right;">330</td>
<td style="text-align: right;">10.35702</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1015.2</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-08-10 19:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">LGA</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">42.08</td>
<td style="text-align: right;">32.00</td>
<td style="text-align: right;">67.22</td>
<td style="text-align: right;">80</td>
<td style="text-align: right;">3.45234</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1015.0</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-12-02 05:00:00</td>
</tr>
<tr class="even">
<td style="text-align: left;">LGA</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">4</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">39.02</td>
<td style="text-align: right;">10.04</td>
<td style="text-align: right;">29.80</td>
<td style="text-align: right;">10</td>
<td style="text-align: right;">12.65858</td>
<td style="text-align: right;">18.41248</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1025.2</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-04-06 09:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">JFK</td>
<td style="text-align: right;">2013</td>
<td style="text-align: right;">4</td>
<td style="text-align: right;">12</td>
<td style="text-align: right;">22</td>
<td style="text-align: right;">42.08</td>
<td style="text-align: right;">39.02</td>
<td style="text-align: right;">88.81</td>
<td style="text-align: right;">30</td>
<td style="text-align: right;">17.26170</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1007.4</td>
<td style="text-align: right;">7</td>
<td style="text-align: left;">2013-04-12 22:00:00</td>
</tr>
</tbody>
</table>

1.  Data airlines, yaitu data nama maskapai yang melakukan penerbangan
    ditiga bandara tersebut

``` r
data("airlines")
kable(sample_n(airlines, 7))
```

| carrier | name                   |
|:--------|:-----------------------|
| F9      | Frontier Airlines Inc. |
| B6      | JetBlue Airways        |
| WN      | Southwest Airlines Co. |
| OO      | SkyWest Airlines Inc.  |
| YV      | Mesa Airlines Inc.     |
| UA      | United Air Lines Inc.  |
| HA      | Hawaiian Airlines Inc. |

1.  Data airports, yaitu data mengenai bandara yang ada di America

``` r
data("airports")
kable(sample_n(airports, 7))
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 37%" />
<col style="width: 9%" />
<col style="width: 12%" />
<col style="width: 5%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 21%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">faa</th>
<th style="text-align: left;">name</th>
<th style="text-align: right;">lat</th>
<th style="text-align: right;">lon</th>
<th style="text-align: right;">alt</th>
<th style="text-align: right;">tz</th>
<th style="text-align: left;">dst</th>
<th style="text-align: left;">tzone</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">3W2</td>
<td style="text-align: left;">Put-in-Bay Airport</td>
<td style="text-align: right;">41.35210</td>
<td style="text-align: right;">-82.49700</td>
<td style="text-align: right;">595</td>
<td style="text-align: right;">-5</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/New_York</td>
</tr>
<tr class="even">
<td style="text-align: left;">ZTF</td>
<td style="text-align: left;">Stamford Amtrak Station</td>
<td style="text-align: right;">41.04694</td>
<td style="text-align: right;">-73.54149</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-5</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/New_York</td>
</tr>
<tr class="odd">
<td style="text-align: left;">NGF</td>
<td style="text-align: left;">Kaneohe Bay Mcaf</td>
<td style="text-align: right;">21.45045</td>
<td style="text-align: right;">-157.76800</td>
<td style="text-align: right;">24</td>
<td style="text-align: right;">-10</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">Pacific/Honolulu</td>
</tr>
<tr class="even">
<td style="text-align: left;">LVM</td>
<td style="text-align: left;">Mission Field Airport</td>
<td style="text-align: right;">45.69939</td>
<td style="text-align: right;">-110.44831</td>
<td style="text-align: right;">4660</td>
<td style="text-align: right;">-7</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Denver</td>
</tr>
<tr class="odd">
<td style="text-align: left;">LOZ</td>
<td style="text-align: left;">London-Corbin Airport-MaGee Field</td>
<td style="text-align: right;">37.08689</td>
<td style="text-align: right;">-84.07739</td>
<td style="text-align: right;">1212</td>
<td style="text-align: right;">-5</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/New_York</td>
</tr>
<tr class="even">
<td style="text-align: left;">DAW</td>
<td style="text-align: left;">Skyhaven Airport</td>
<td style="text-align: right;">43.28406</td>
<td style="text-align: right;">-70.92928</td>
<td style="text-align: right;">321</td>
<td style="text-align: right;">-5</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/New_York</td>
</tr>
<tr class="odd">
<td style="text-align: left;">RCE</td>
<td style="text-align: left;">Roche Harbor Seaplane Base</td>
<td style="text-align: right;">48.60806</td>
<td style="text-align: right;">-123.15972</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-8</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Los_Angeles</td>
</tr>
</tbody>
</table>

1.  Data planes, yaitu data mengenai pesawat yang melakukan penerbangan
    ditiga bandara tersebut

``` r
data("airports")
kable(sample_n(airports, 7))
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 41%" />
<col style="width: 9%" />
<col style="width: 11%" />
<col style="width: 5%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">faa</th>
<th style="text-align: left;">name</th>
<th style="text-align: right;">lat</th>
<th style="text-align: right;">lon</th>
<th style="text-align: right;">alt</th>
<th style="text-align: right;">tz</th>
<th style="text-align: left;">dst</th>
<th style="text-align: left;">tzone</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">FMN</td>
<td style="text-align: left;">Four Corners Rgnl</td>
<td style="text-align: right;">36.74125</td>
<td style="text-align: right;">-108.22994</td>
<td style="text-align: right;">5506</td>
<td style="text-align: right;">-7</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Denver</td>
</tr>
<tr class="even">
<td style="text-align: left;">NIB</td>
<td style="text-align: left;">Nikolai Airport</td>
<td style="text-align: right;">63.01083</td>
<td style="text-align: right;">-154.38389</td>
<td style="text-align: right;">427</td>
<td style="text-align: right;">-9</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Anchorage</td>
</tr>
<tr class="odd">
<td style="text-align: left;">CEF</td>
<td style="text-align: left;">Westover Arb Metropolitan</td>
<td style="text-align: right;">42.19401</td>
<td style="text-align: right;">-72.53478</td>
<td style="text-align: right;">241</td>
<td style="text-align: right;">-5</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/New_York</td>
</tr>
<tr class="even">
<td style="text-align: left;">HDO</td>
<td style="text-align: left;">Hondo Municipal Airport</td>
<td style="text-align: right;">29.35910</td>
<td style="text-align: right;">-99.17750</td>
<td style="text-align: right;">930</td>
<td style="text-align: right;">-6</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Chicago</td>
</tr>
<tr class="odd">
<td style="text-align: left;">SPS</td>
<td style="text-align: left;">Sheppard Afb Wichita Falls Muni</td>
<td style="text-align: right;">33.98880</td>
<td style="text-align: right;">-98.49189</td>
<td style="text-align: right;">1019</td>
<td style="text-align: right;">-6</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Chicago</td>
</tr>
<tr class="even">
<td style="text-align: left;">RFD</td>
<td style="text-align: left;">Chicago Rockford International Airport</td>
<td style="text-align: right;">42.19536</td>
<td style="text-align: right;">-89.09722</td>
<td style="text-align: right;">742</td>
<td style="text-align: right;">-6</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Chicago</td>
</tr>
<tr class="odd">
<td style="text-align: left;">MYU</td>
<td style="text-align: left;">Mekoryuk Airport</td>
<td style="text-align: right;">60.37140</td>
<td style="text-align: right;">-166.27100</td>
<td style="text-align: right;">48</td>
<td style="text-align: right;">-9</td>
<td style="text-align: left;">A</td>
<td style="text-align: left;">America/Anchorage</td>
</tr>
</tbody>
</table>

### Menghapus data yang tidak legkap

``` r
cekNa(flights)
```

    ##           year          month            day       dep_time sched_dep_time 
    ##              0              0              0           8255              0 
    ##      dep_delay       arr_time sched_arr_time      arr_delay        carrier 
    ##           8255           8713              0           9430              0 
    ##         flight        tailnum         origin           dest       air_time 
    ##              0           2512              0              0           9430 
    ##       distance           hour         minute      time_hour 
    ##              0              0              0              0

Terlihat bahwa 6 kolom yang masih mengandung NA. Agar memudahkan
analisis maka data tersebut akan dihilangkan

``` r
data <- na.omit(flights)
cekNa(data)
```

    ##           year          month            day       dep_time sched_dep_time 
    ##              0              0              0              0              0 
    ##      dep_delay       arr_time sched_arr_time      arr_delay        carrier 
    ##              0              0              0              0              0 
    ##         flight        tailnum         origin           dest       air_time 
    ##              0              0              0              0              0 
    ##       distance           hour         minute      time_hour 
    ##              0              0              0              0

### Menghapus kolom yang tidak diperlukan

``` r
data <- data %>%
      select(-c(arr_time, sched_arr_time, flight, air_time, minute)) %>%
      mutate(sched_dep_time = convertToDate(2013, month, day, sched_dep_time),
             dep_time = sched_dep_time + dep_delay * 60) 
kable(sample_n(data, 7))
```

<table>
<colgroup>
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 2%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 7%" />
<col style="width: 7%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 3%" />
<col style="width: 6%" />
<col style="width: 3%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">year</th>
<th style="text-align: right;">month</th>
<th style="text-align: right;">day</th>
<th style="text-align: left;">dep_time</th>
<th style="text-align: left;">sched_dep_time</th>
<th style="text-align: right;">dep_delay</th>
<th style="text-align: right;">arr_delay</th>
<th style="text-align: left;">carrier</th>
<th style="text-align: left;">tailnum</th>
<th style="text-align: left;">origin</th>
<th style="text-align: left;">dest</th>
<th style="text-align: right;">distance</th>
<th style="text-align: right;">hour</th>
<th style="text-align: left;">time_hour</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">19</td>
<td style="text-align: left;">2013-07-19 10:53:00</td>
<td style="text-align: left;">2013-07-19 11:00:00</td>
<td style="text-align: right;">-7</td>
<td style="text-align: right;">-14</td>
<td style="text-align: left;">DL</td>
<td style="text-align: left;">N981DL</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">MCO</td>
<td style="text-align: right;">944</td>
<td style="text-align: right;">11</td>
<td style="text-align: left;">2013-07-19 11:00:00</td>
</tr>
<tr class="even">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">17</td>
<td style="text-align: left;">2013-01-17 14:40:00</td>
<td style="text-align: left;">2013-01-17 14:45:00</td>
<td style="text-align: right;">-5</td>
<td style="text-align: right;">5</td>
<td style="text-align: left;">US</td>
<td style="text-align: left;">N563UW</td>
<td style="text-align: left;">LGA</td>
<td style="text-align: left;">CLT</td>
<td style="text-align: right;">544</td>
<td style="text-align: right;">14</td>
<td style="text-align: left;">2013-01-17 14:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">30</td>
<td style="text-align: left;">2013-09-30 16:55:00</td>
<td style="text-align: left;">2013-09-30 17:00:00</td>
<td style="text-align: right;">-5</td>
<td style="text-align: right;">-20</td>
<td style="text-align: left;">DL</td>
<td style="text-align: left;">N914DE</td>
<td style="text-align: left;">LGA</td>
<td style="text-align: left;">ATL</td>
<td style="text-align: right;">762</td>
<td style="text-align: right;">17</td>
<td style="text-align: left;">2013-09-30 17:00:00</td>
</tr>
<tr class="even">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">11</td>
<td style="text-align: left;">2013-01-11 16:19:00</td>
<td style="text-align: left;">2013-01-11 16:20:00</td>
<td style="text-align: right;">-1</td>
<td style="text-align: right;">0</td>
<td style="text-align: left;">US</td>
<td style="text-align: left;">N523UW</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">PHX</td>
<td style="text-align: right;">2153</td>
<td style="text-align: right;">16</td>
<td style="text-align: left;">2013-01-11 16:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">2013-06-02 12:09:00</td>
<td style="text-align: left;">2013-06-02 12:01:00</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">EV</td>
<td style="text-align: left;">N16981</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">JAX</td>
<td style="text-align: right;">820</td>
<td style="text-align: right;">12</td>
<td style="text-align: left;">2013-06-02 12:00:00</td>
</tr>
<tr class="even">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">16</td>
<td style="text-align: left;">2013-06-16 17:34:00</td>
<td style="text-align: left;">2013-06-16 17:35:00</td>
<td style="text-align: right;">-1</td>
<td style="text-align: right;">-10</td>
<td style="text-align: left;">AA</td>
<td style="text-align: left;">N3EEAA</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">IAH</td>
<td style="text-align: right;">1417</td>
<td style="text-align: right;">17</td>
<td style="text-align: left;">2013-06-16 17:00:00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2013</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">15</td>
<td style="text-align: left;">2013-07-15 16:03:00</td>
<td style="text-align: left;">2013-07-15 16:04:00</td>
<td style="text-align: right;">-1</td>
<td style="text-align: right;">-12</td>
<td style="text-align: left;">EV</td>
<td style="text-align: left;">N751EV</td>
<td style="text-align: left;">LGA</td>
<td style="text-align: left;">BNA</td>
<td style="text-align: right;">764</td>
<td style="text-align: right;">16</td>
<td style="text-align: left;">2013-07-15 16:00:00</td>
</tr>
</tbody>
</table>

### Integrasi dengan dataset lainnya

Saya menggabungkan 4 dataset dengan data flights ini, data yang
digabungkan adalah kolom yang akan digunakan untuk melakukan analisis
kedepannya.

``` r
data <- data %>%
            left_join(select(weather, -c(dewp, wind_dir, wind_gust, precip)), 
                      by = c("origin", "time_hour", "month", "day", "year", "hour")) %>%
            left_join(select(airports, faa, name), 
                      by = c("origin" = "faa")) %>%
            select(-year) %>%
            rename(dep_airport = name) %>%
            left_join(select(airports, faa, name),
                      by = c("dest" = "faa")) %>%
            rename(dest_airport = name) %>%
            left_join(select(planes, tailnum, year, engine), 
                      by = "tailnum") %>%
            rename(year_aiplane = year) %>%
            left_join(airlines, by = "carrier")
kable(sample_n(data, 7))
```

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 7%" />
<col style="width: 7%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 3%" />
<col style="width: 1%" />
<col style="width: 7%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 7%" />
<col style="width: 12%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">month</th>
<th style="text-align: right;">day</th>
<th style="text-align: left;">dep_time</th>
<th style="text-align: left;">sched_dep_time</th>
<th style="text-align: right;">dep_delay</th>
<th style="text-align: right;">arr_delay</th>
<th style="text-align: left;">carrier</th>
<th style="text-align: left;">tailnum</th>
<th style="text-align: left;">origin</th>
<th style="text-align: left;">dest</th>
<th style="text-align: right;">distance</th>
<th style="text-align: right;">hour</th>
<th style="text-align: left;">time_hour</th>
<th style="text-align: right;">temp</th>
<th style="text-align: right;">humid</th>
<th style="text-align: right;">wind_speed</th>
<th style="text-align: right;">pressure</th>
<th style="text-align: right;">visib</th>
<th style="text-align: left;">dep_airport</th>
<th style="text-align: left;">dest_airport</th>
<th style="text-align: right;">year_aiplane</th>
<th style="text-align: left;">engine</th>
<th style="text-align: left;">name</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">11</td>
<td style="text-align: right;">30</td>
<td style="text-align: left;">2013-11-30 10:57:00</td>
<td style="text-align: left;">2013-11-30 10:59:00</td>
<td style="text-align: right;">-2</td>
<td style="text-align: right;">-22</td>
<td style="text-align: left;">UA</td>
<td style="text-align: left;">N475UA</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">TPA</td>
<td style="text-align: right;">997</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">2013-11-30 10:00:00</td>
<td style="text-align: right;">33.08</td>
<td style="text-align: right;">48.98</td>
<td style="text-align: right;">6.90468</td>
<td style="text-align: right;">1040.7</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">Newark Liberty Intl</td>
<td style="text-align: left;">Tampa Intl</td>
<td style="text-align: right;">2001</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">United Air Lines Inc.</td>
</tr>
<tr class="even">
<td style="text-align: right;">8</td>
<td style="text-align: right;">25</td>
<td style="text-align: left;">2013-08-25 13:22:00</td>
<td style="text-align: left;">2013-08-25 13:26:00</td>
<td style="text-align: right;">-4</td>
<td style="text-align: right;">-25</td>
<td style="text-align: left;">EV</td>
<td style="text-align: left;">N14960</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">IND</td>
<td style="text-align: right;">645</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">2013-08-25 13:00:00</td>
<td style="text-align: right;">82.04</td>
<td style="text-align: right;">36.77</td>
<td style="text-align: right;">9.20624</td>
<td style="text-align: right;">1027.1</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">Newark Liberty Intl</td>
<td style="text-align: left;">Indianapolis Intl</td>
<td style="text-align: right;">1998</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">ExpressJet Airlines Inc.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">10</td>
<td style="text-align: right;">15</td>
<td style="text-align: left;">2013-10-15 14:54:00</td>
<td style="text-align: left;">2013-10-15 15:00:00</td>
<td style="text-align: right;">-6</td>
<td style="text-align: right;">-2</td>
<td style="text-align: left;">US</td>
<td style="text-align: left;">N954UW</td>
<td style="text-align: left;">LGA</td>
<td style="text-align: left;">BOS</td>
<td style="text-align: right;">184</td>
<td style="text-align: right;">15</td>
<td style="text-align: left;">2013-10-15 15:00:00</td>
<td style="text-align: right;">68.00</td>
<td style="text-align: right;">58.80</td>
<td style="text-align: right;">8.05546</td>
<td style="text-align: right;">1020.8</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">La Guardia</td>
<td style="text-align: left;">General Edward Lawrence Logan Intl</td>
<td style="text-align: right;">2007</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">US Airways Inc.</td>
</tr>
<tr class="even">
<td style="text-align: right;">5</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">2013-05-03 14:13:00</td>
<td style="text-align: left;">2013-05-03 12:00:00</td>
<td style="text-align: right;">133</td>
<td style="text-align: right;">136</td>
<td style="text-align: left;">UA</td>
<td style="text-align: left;">N466UA</td>
<td style="text-align: left;">LGA</td>
<td style="text-align: left;">ORD</td>
<td style="text-align: right;">733</td>
<td style="text-align: right;">12</td>
<td style="text-align: left;">2013-05-03 12:00:00</td>
<td style="text-align: right;">60.08</td>
<td style="text-align: right;">37.37</td>
<td style="text-align: right;">16.11092</td>
<td style="text-align: right;">1030.2</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">La Guardia</td>
<td style="text-align: left;">Chicago Ohare Intl</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">United Air Lines Inc.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">3</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">2013-03-03 09:11:00</td>
<td style="text-align: left;">2013-03-03 09:18:00</td>
<td style="text-align: right;">-7</td>
<td style="text-align: right;">-7</td>
<td style="text-align: left;">B6</td>
<td style="text-align: left;">N516JB</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">BOS</td>
<td style="text-align: right;">187</td>
<td style="text-align: right;">9</td>
<td style="text-align: left;">2013-03-03 09:00:00</td>
<td style="text-align: right;">33.08</td>
<td style="text-align: right;">48.98</td>
<td style="text-align: right;">13.80936</td>
<td style="text-align: right;">1007.0</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">John F Kennedy Intl</td>
<td style="text-align: left;">General Edward Lawrence Logan Intl</td>
<td style="text-align: right;">2000</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">JetBlue Airways</td>
</tr>
<tr class="even">
<td style="text-align: right;">8</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">2013-08-03 06:11:00</td>
<td style="text-align: left;">2013-08-03 06:14:00</td>
<td style="text-align: right;">-3</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">US</td>
<td style="text-align: left;">N103US</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">CLT</td>
<td style="text-align: right;">541</td>
<td style="text-align: right;">6</td>
<td style="text-align: left;">2013-08-03 06:00:00</td>
<td style="text-align: right;">73.04</td>
<td style="text-align: right;">70.68</td>
<td style="text-align: right;">6.90468</td>
<td style="text-align: right;">1012.9</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">John F Kennedy Intl</td>
<td style="text-align: left;">Charlotte Douglas Intl</td>
<td style="text-align: right;">1999</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">US Airways Inc.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">11</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">2013-11-08 16:49:00</td>
<td style="text-align: left;">2013-11-08 16:50:00</td>
<td style="text-align: right;">-1</td>
<td style="text-align: right;">-7</td>
<td style="text-align: left;">9E</td>
<td style="text-align: left;">N295PQ</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">CVG</td>
<td style="text-align: right;">589</td>
<td style="text-align: right;">16</td>
<td style="text-align: left;">2013-11-08 16:00:00</td>
<td style="text-align: right;">48.02</td>
<td style="text-align: right;">32.53</td>
<td style="text-align: right;">21.86482</td>
<td style="text-align: right;">1019.1</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">John F Kennedy Intl</td>
<td style="text-align: left;">Cincinnati Northern Kentucky Intl</td>
<td style="text-align: right;">2013</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">Endeavor Air Inc.</td>
</tr>
</tbody>
</table>

Data yang akan dianalisis lebih lanjut sebanyak *327346 data.*

Soal Nomor 1
------------

Apakah delay pada semua bandara memiliki suatu pola tertentu ataukah
bersifat random? Dengan mengetahui informasi ini dapat membantu kita
memahami apakah ada pola tertentu pada delay atau tidak. Jika ada,
dengan mengetahui pola delay, kita dapat mencoba mengatasi penyebab
sistemik dari delay tersebut. Jika tidak ada, setidaknya kita dapat
mengidentifikasi beberapa anomali yang menyebabkan delay. Kita juga
dapat mengukur kinerja bandara selama 12 bulan.

Yang akan menjadi fokus adalah *delay keberangkatan*. Ada dua jenis
delay. yaitu delay positif adalah jam keberangkatan pesawat lebih lambat
dari yang dijadwalkan sebaliknya delay negatif adalah jam keberangkatan
pesawat lebih cepat dari yang dijadwalkan.

### Identifikasi delay secara keseluruhan

``` r
summary(data$dep_delay)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -43.00   -5.00   -2.00   12.56   11.00 1301.00

Terlihat bahwa pada tahun 2013 penerbangan paling cepet berangkat 43
menit dari yang dijadwalkan. Jika diliat dari mediannya yang bernilai
-2, berarti ada lebih dari 50% penerbangan pada tahun 2013 yang terbang
lebih cepat daripada jadwalnya. Jika ditelusuri lebih lanjut ternyata
benar ada 183315 penerbangan yang lebih cepat berangkat dan ada 127745
yang lebih lambat dari yang dijadwalkan

Jika dilihat hasil 5 summarynya terdapat hal yang cukup unik yaitu nilai
mediannya negatif akan tetapi nilai rata-ratanya positif dan berbeda
sangat jauh, Selain itu kuartil 3 nya lebih kecil dari rata-ratanya. Hal
ini disebabkan karena banyaknya delay positif yang masuk ke kategori
outlier. Hal ini terlihat dari blox plot dibawah ini. Bentuk dari
boxplotnya sampai tidak terlihat dengan jelas karena banyaknya nilai
outlier yang positif.

``` r
out <- boxplot.stats(data$dep_delay)$out
outPositif <- sum(out > 0)
outNegatif <- length(out) - outPositif
data %>%
      ggplot(aes(y = dep_delay)) +
      geom_boxplot() +
      labs(title = "Box Plot Delay Keberangkatan",
           subtitle = paste("Jumlah Outlier", length(out)),
           caption = paste(outPositif, "Outlier atas dan", outNegatif, "outlier bawah")) +
      theme_minimal() +
      theme(plot.title = element_text(hjus = 0.5),
            plot.subtitle = element_text(hjus = 0.5))
```

![](laporan_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
kable(data[which.max(data$dep_delay), ])
```

<table style="width:100%;">
<colgroup>
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 7%" />
<col style="width: 7%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 3%" />
<col style="width: 1%" />
<col style="width: 7%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 7%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 3%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">month</th>
<th style="text-align: right;">day</th>
<th style="text-align: left;">dep_time</th>
<th style="text-align: left;">sched_dep_time</th>
<th style="text-align: right;">dep_delay</th>
<th style="text-align: right;">arr_delay</th>
<th style="text-align: left;">carrier</th>
<th style="text-align: left;">tailnum</th>
<th style="text-align: left;">origin</th>
<th style="text-align: left;">dest</th>
<th style="text-align: right;">distance</th>
<th style="text-align: right;">hour</th>
<th style="text-align: left;">time_hour</th>
<th style="text-align: right;">temp</th>
<th style="text-align: right;">humid</th>
<th style="text-align: right;">wind_speed</th>
<th style="text-align: right;">pressure</th>
<th style="text-align: right;">visib</th>
<th style="text-align: left;">dep_airport</th>
<th style="text-align: left;">dest_airport</th>
<th style="text-align: right;">year_aiplane</th>
<th style="text-align: left;">engine</th>
<th style="text-align: left;">name</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">1</td>
<td style="text-align: right;">9</td>
<td style="text-align: left;">2013-01-10 06:41:00</td>
<td style="text-align: left;">2013-01-09 09:00:00</td>
<td style="text-align: right;">1301</td>
<td style="text-align: right;">1272</td>
<td style="text-align: left;">HA</td>
<td style="text-align: left;">N384HA</td>
<td style="text-align: left;">JFK</td>
<td style="text-align: left;">HNL</td>
<td style="text-align: right;">4983</td>
<td style="text-align: right;">9</td>
<td style="text-align: left;">2013-01-09 09:00:00</td>
<td style="text-align: right;">44.96</td>
<td style="text-align: right;">73.59</td>
<td style="text-align: right;">4.60312</td>
<td style="text-align: right;">1029</td>
<td style="text-align: right;">8</td>
<td style="text-align: left;">John F Kennedy Intl</td>
<td style="text-align: left;">Honolulu Intl</td>
<td style="text-align: right;">2011</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">Hawaiian Airlines Inc.</td>
</tr>
</tbody>
</table>

Dari 5 summary, terlihat bahwa ada penerbangan yang delay hingga 1301
menit, atau sekitar 21 jam. Delay terlama itu ternyata terjadi pada
tanggal 9 Januari. Penerbangan tersebut adalah penerbangan dari Bandara
John F Kennedy ke Bandara Honolulu. Penerbangan tersebut dijadwalkan
berangkat padad jama 9 pagi akan tetapi baru berhasil berangkat pada
esok harinya jam 6:41 pagi. Maskapai dari penerbangan tersebut adalah
Hawaiian Airlines Inc.

### Melihat delay per bandara keberangkatan

``` r
tapply(data$dep_delay, data$dep_airport, function(x){
      jml <- length(x)
      c(jumlah = jml,
           cepat_berangkat = round(sum(x < 0) / jml * 100, 1) ,
           tepat = round(sum(x == 0)/jml * 100, 1),
           lama_berangkat = round(sum(x > 0) / jml * 100, 1))
})
```

    ## $`John F Kennedy Intl`
    ##          jumlah cepat_berangkat           tepat  lama_berangkat 
    ##        109079.0            55.9             5.7            38.4 
    ## 
    ## $`La Guardia`
    ##          jumlah cepat_berangkat           tepat  lama_berangkat 
    ##        101140.0            62.3             4.6            33.1 
    ## 
    ## $`Newark Liberty Intl`
    ##          jumlah cepat_berangkat           tepat  lama_berangkat 
    ##        117127.0            50.5             4.8            44.7

Terlihat bahwa ditiga bandara tersebut persentasi yang lebih cepat
berangkat selalu lebih besar dari persentasi yang lambar berangkat. Jika
dilihat bandara yang tepat waktu adalah bandara John F Kennedy, sebesar
5.7 persen penerbangannya tepat sesuai dengan yang di jadwalkan,
sedangkan untuk bandara yang paling sering delay adalah bandara Newark
Liberty dengan persentase 44.7 penerbangan delay pada tahun 2013.

``` r
perBandara <- split(data$dep_delay, data$dep_airport)
kable(sapply(perBandara, summary))
```

|         |  John F Kennedy Intl|  La Guardia|  Newark Liberty Intl|
|:--------|--------------------:|-----------:|--------------------:|
| Min.    |            -43.00000|   -33.00000|            -25.00000|
| 1st Qu. |             -5.00000|    -6.00000|             -4.00000|
| Median  |             -1.00000|    -3.00000|             -1.00000|
| Mean    |             12.02361|    10.28658|             15.00911|
| 3rd Qu. |             10.00000|     7.00000|             15.00000|
| Max.    |           1301.00000|   911.00000|           1126.00000|

Telihat bahwa di ketiga bandara tersebu rata-rata waktu delai hanya
sekitar 10-15 menit. Akan tetapi terlihat juga di Bandara La Guardia
terjadi delay terlama selama kurang lebih 15 jam sementara di Newark
Liberty kurang lebih 18 jam. Berikut data penerbangan yang memiliki
waktu delay terlama di setiap bandara.

``` r
maxDelay <- sapply(perBandara, which.max)
kable(data[maxDelay, ])
```

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 7%" />
<col style="width: 7%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 1%" />
<col style="width: 3%" />
<col style="width: 1%" />
<col style="width: 7%" />
<col style="width: 2%" />
<col style="width: 2%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 7%" />
<col style="width: 9%" />
<col style="width: 5%" />
<col style="width: 3%" />
<col style="width: 8%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">month</th>
<th style="text-align: right;">day</th>
<th style="text-align: left;">dep_time</th>
<th style="text-align: left;">sched_dep_time</th>
<th style="text-align: right;">dep_delay</th>
<th style="text-align: right;">arr_delay</th>
<th style="text-align: left;">carrier</th>
<th style="text-align: left;">tailnum</th>
<th style="text-align: left;">origin</th>
<th style="text-align: left;">dest</th>
<th style="text-align: right;">distance</th>
<th style="text-align: right;">hour</th>
<th style="text-align: left;">time_hour</th>
<th style="text-align: right;">temp</th>
<th style="text-align: right;">humid</th>
<th style="text-align: right;">wind_speed</th>
<th style="text-align: right;">pressure</th>
<th style="text-align: right;">visib</th>
<th style="text-align: left;">dep_airport</th>
<th style="text-align: left;">dest_airport</th>
<th style="text-align: right;">year_aiplane</th>
<th style="text-align: left;">engine</th>
<th style="text-align: left;">name</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">1</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">2013-01-03 18:19:00</td>
<td style="text-align: left;">2013-01-03 18:20:00</td>
<td style="text-align: right;">-1</td>
<td style="text-align: right;">-42</td>
<td style="text-align: left;">AA</td>
<td style="text-align: left;">N3DNAA</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">LAX</td>
<td style="text-align: right;">2454</td>
<td style="text-align: right;">18</td>
<td style="text-align: left;">2013-01-03 18:00:00</td>
<td style="text-align: right;">30.92</td>
<td style="text-align: right;">51.38</td>
<td style="text-align: right;">10.35702</td>
<td style="text-align: right;">1019.6</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">Newark Liberty Intl</td>
<td style="text-align: left;">Los Angeles Intl</td>
<td style="text-align: right;">NA</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">American Airlines Inc.</td>
</tr>
<tr class="even">
<td style="text-align: right;">10</td>
<td style="text-align: right;">22</td>
<td style="text-align: left;">2013-10-22 13:55:00</td>
<td style="text-align: left;">2013-10-22 13:55:00</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-9</td>
<td style="text-align: left;">WN</td>
<td style="text-align: left;">N733SA</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">PHX</td>
<td style="text-align: right;">2133</td>
<td style="text-align: right;">13</td>
<td style="text-align: left;">2013-10-22 13:00:00</td>
<td style="text-align: right;">64.04</td>
<td style="text-align: right;">53.73</td>
<td style="text-align: right;">12.65858</td>
<td style="text-align: right;">1008.6</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">Newark Liberty Intl</td>
<td style="text-align: left;">Phoenix Sky Harbor Intl</td>
<td style="text-align: right;">1999</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">Southwest Airlines Co.</td>
</tr>
<tr class="odd">
<td style="text-align: right;">1</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">2013-01-04 11:00:00</td>
<td style="text-align: left;">2013-01-04 11:00:00</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">-49</td>
<td style="text-align: left;">UA</td>
<td style="text-align: left;">N87513</td>
<td style="text-align: left;">EWR</td>
<td style="text-align: left;">SFO</td>
<td style="text-align: right;">2565</td>
<td style="text-align: right;">11</td>
<td style="text-align: left;">2013-01-04 11:00:00</td>
<td style="text-align: right;">37.04</td>
<td style="text-align: right;">47.75</td>
<td style="text-align: right;">20.71404</td>
<td style="text-align: right;">1016.9</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">Newark Liberty Intl</td>
<td style="text-align: left;">San Francisco Intl</td>
<td style="text-align: right;">2008</td>
<td style="text-align: left;">Turbo-fan</td>
<td style="text-align: left;">United Air Lines Inc.</td>
</tr>
</tbody>
</table>

``` r
data %>%
      select(dep_delay, dep_airport) %>%
      group_by(dep_airport) %>%
      mutate(dep_delay = round(dep_delay / 60)) %>%
      filter(dep_delay != 0) %>%
      count(dep_delay) %>%
      arrange(dep_airport, dep_delay) %>%
      rename(dep_delay_jam = dep_delay) %>%
      kable()
```

| dep\_airport        |  dep\_delay\_jam|      n|
|:--------------------|----------------:|------:|
| John F Kennedy Intl |               -1|      1|
| John F Kennedy Intl |                1|  10154|
| John F Kennedy Intl |                2|   3141|
| John F Kennedy Intl |                3|   1140|
| John F Kennedy Intl |                4|    433|
| John F Kennedy Intl |                5|    159|
| John F Kennedy Intl |                6|     59|
| John F Kennedy Intl |                7|     18|
| John F Kennedy Intl |                8|      6|
| John F Kennedy Intl |                9|      1|
| John F Kennedy Intl |               10|      3|
| John F Kennedy Intl |               11|      2|
| John F Kennedy Intl |               12|      1|
| John F Kennedy Intl |               13|      4|
| John F Kennedy Intl |               14|      3|
| John F Kennedy Intl |               15|      1|
| John F Kennedy Intl |               16|      1|
| John F Kennedy Intl |               17|      2|
| John F Kennedy Intl |               19|      1|
| John F Kennedy Intl |               22|      1|
| La Guardia          |               -1|      2|
| La Guardia          |                1|   8639|
| La Guardia          |                2|   2557|
| La Guardia          |                3|   1040|
| La Guardia          |                4|    440|
| La Guardia          |                5|    211|
| La Guardia          |                6|     85|
| La Guardia          |                7|     31|
| La Guardia          |                8|     15|
| La Guardia          |                9|      3|
| La Guardia          |               10|      4|
| La Guardia          |               11|      2|
| La Guardia          |               12|      1|
| La Guardia          |               13|      4|
| La Guardia          |               14|      2|
| La Guardia          |               15|      2|
| Newark Liberty Intl |                1|  13231|
| Newark Liberty Intl |                2|   4092|
| Newark Liberty Intl |                3|   1487|
| Newark Liberty Intl |                4|    579|
| Newark Liberty Intl |                5|    221|
| Newark Liberty Intl |                6|     86|
| Newark Liberty Intl |                7|     24|
| Newark Liberty Intl |                8|      6|
| Newark Liberty Intl |                9|      2|
| Newark Liberty Intl |               10|      2|
| Newark Liberty Intl |               11|      1|
| Newark Liberty Intl |               12|      1|
| Newark Liberty Intl |               13|      2|
| Newark Liberty Intl |               14|      2|
| Newark Liberty Intl |               15|      2|
| Newark Liberty Intl |               19|      1|

Dari tabel diatas terlihat bahwa di ketiga bandara tersebut jumlah
penerebangan yang delay kurang lebih sejam merupakan yang terbanyak.
Penerbangan yang delay kurang lebih 4 jam hanya sekitar 400-500 di
setiap Bandara. Sementara itu untuk delay yang lebih dari 6 jam di
Bandara John F Kennedy sebanyak 103 penerbangan, di Bandara La Guardia
sebanyak 149 penerbangan, dan di Bandara Newark Liberty 129 penerbangan.
Dari beberapa hal yang telah disebutkan diatas, dapat diketahu bahwa
kinerja ketiga bandara tersebut relatif sama.

Sebelumnya saya akan memisahkan delay yang positif dan negatif

``` r
cepatBerangkat <- filter(data, dep_delay < 0)
lambatBerangkat <- filter(data, dep_delay > 0)
```

### Pola Delay Per Bulan

``` r
lambatBerangkat %>%
      group_by(dep_airport, month) %>%
      summarise(jumlah = round(mean(dep_delay), 1)) %>%
      ggplot() +
      geom_col(aes(x = factor(month), y = jumlah, fill = dep_airport), position = "dodge") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 62)) +
      labs(fill = "",
           title = "Rata-Rata Delay Positif (menit) per Bulan",
           x = "",
           y = "") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 15),
            plot.margin = margin(c(20,20,20,10)),
            legend.box.margin = margin(-20, 0, 0, 0))
```


![](laporan_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
data %>% 
      count(month) %>% 
      arrange(desc(n)) %>%
      rename(jumlah_penerbangan = n) %>%
      kable()
```

|  month|  jumlah\_penerbangan|
|------:|--------------------:|
|      8|                28756|
|     10|                28618|
|      7|                28293|
|      5|                28128|
|      3|                27902|
|      4|                27564|
|      6|                27075|
|     12|                27020|
|      9|                27010|
|     11|                26971|
|      1|                26398|
|      2|                23611|

Terlihat bahwa rata-rata delay terlama pada tahan 2013 terjadi pada
bulan Juni dan Juli. Sementara itu pada bulan Agustus dan Oktober yang
memiliki jumlah penerbangan terbanyak malah memiliki rata-rata delay
yang lebih rebdah dari bulan Juni dan Juli. Ini mengindikasikan ada
sesuatu yang terjadi pada bulan Juni dan Juli hingga menyebabkan
rata-rata waktu delay terlama.

### Pola Delay Perjam

``` r
lambatBerangkat %>%
      group_by(dep_airport, hour) %>%
      summarise(jumlah = round(mean(dep_delay), 1)) %>%
      ggplot() +
      geom_col(aes(x = factor(hour), y = jumlah, fill = dep_airport), position = "dodge") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 62)) +
      labs(fill = "",
           title = "Rata-Rata Delay Positif (menit) per Jam",
           x = "",
           y = "") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 15),
            plot.margin = margin(c(20,20,20,10)),
            legend.box.margin = margin(-20, 0, 0, 0))
```

  

![](laporan_files/figure-markdown_github/unnamed-chunk-23-1.png)

Dari diagram diatas terlihat pola yang sama untuk setiap bandara.
Bandara akan mulai penerbangan sekitar jam 5 pagi hingga jam 23, kecuali
untuk Bandara La Guardia, hanya sampai jam 22. Semakin bertambah jamnya
maka rata-rata delay per menitnya semakin tinggi dan akan memuncak pada
malam hari pukul 19-22. Untuk penerbangan di pagi hari, rata-rata delay
tidak sampai 30 menit. Setalah pukul 16 sore rata-rata delay sudah
mencapai 40 menit.

``` r
cepatBerangkat %>%
      group_by(dep_airport, hour) %>%
      summarise(jumlah = round(mean(abs(dep_delay)), 1)) %>%
      ggplot() +
      geom_col(aes(x = factor(hour), y = jumlah, fill = dep_airport), position = "dodge") +
      scale_y_continuous(expand = c(0, 0)) +
      labs(fill = "",
           title = "Rata-Rata Delay negatif (menit) per Jam",
           x = "",
           y = "") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 15),
            plot.margin = margin(c(20,20,20,10)),
            legend.box.margin = margin(-20, 0, 0, 0))
```



![](laporan_files/figure-markdown_github/unnamed-chunk-24-1.png)

Sementara untuk delay yang negatif tidak terlihat pola yang signifikan.
Jika dilihat dari delay rata-ratanya tidak mencapai 10 menit.
Keberangkatan yang lebih cepat ternyata tidak terlalu signifikan dan
masih dapat dianggap tepat waktu, karena bahkan tidak melebihi 10 menit.

``` r
lambatBerangkat %>%
      group_by(dep_airport, visib) %>%
      summarise(jumlah = round(mean(abs(dep_delay)), 1)) %>%
      ggplot() +
      geom_col(aes(x = factor(round(visib)), y = jumlah, fill = dep_airport), position = "dodge") +
      scale_y_continuous(expand = c(0, 0)) +
      labs(fill = "",
           title = "Rata-Rata Delay Positif (menit) per Visibilitas",
           x = "Visibility in miles",
           y = "") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 15),
            plot.margin = margin(c(20,20,20,10)),
            legend.box.margin = margin(-10, 0, 0, 0))
```



![](laporan_files/figure-markdown_github/unnamed-chunk-25-1.png)

Jika dilihat berdasarkan visibilitas pandangan di udara, semakin kecil
visibilitas maka semakin tinggi rata-rata delay. Ini adalah sesuatu yang
wajar karena suatu penerbangan tidak akan mengambil resiko jika jarak
pandang diudaranya sangat pendek.

``` r
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
correlation <- lambatBerangkat %>%
                  select(wind_speed, pressure, humid, dep_delay, temp) %>%
                  na.omit() %>% cor()

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation, method = "color", type="lower", col = col(200), tl.srt=45, 
         addCoef.col = "black", order="hclust") + 
title("Correlation Plot")
```

![](laporan_files/figure-markdown_github/unnamed-chunk-26-1.png)

  

Dari korelasi plot diatas tidak didapati korelasi yang signifikan antara
delay keberangkatan penerbangan tahun 2013 dengan keadaan cuaca seperti
wind\_speed, pressure, temperature, dan humadity

### Kesimpulan

Dari beberapa hal yang dijabarkan diatas dapat diambil beberapa
kesimpulan

1.  Kinerja dari 3 bandara tersebut sama, tidak ada yang menonjol lebih
    baik. Ketiganya memiliki jumlah rata-rata delay yang hampir sama.
2.  Pola delay berdasarkan bulan, terjadi paling sering pada bulan Juni
    dan Juli meskipun begitu Juni dan Juli bukan merupakan bulan yang
    memiliki penerbangan paling banyak
3.  Pola delay positif memiliki trend yang positif terhadap jam di
    ketiga bandara tersebut. Puncak rata-rata delay positif terjadi pada
    jam 19-22.
4.  Delay negatif tidak memiliki pola tehadap jam. Rata-ratanya juga
    hanya berkisar 4-5 menit disetiap bandara.
5.  Delay juga mengikuti pola terhadap visibilitas penglihatan di udara
    dan ini adalah hal yang wajar. Jika jarang pandang diudara kecil
    maka penerbangan akan cenderung ditunda.
6.  Penerbangan pada tahun 2013 di New York tidak dibanyak dipengaruhi
    oleh cuaca, karena korelasi antara beberapa komponen cuaca dengan
    waktu delay sangat kecil.

Soal 2
------

Carrier mana yang memiliki kinerja paling baik dan paling buruk selama
tahun 2013? Which carriers have been the top and the bottom performers
in 2013? Pertanyaan ini akan membantu kita mengidentifikasi carrier mana
yang memiliki kinerja buruk sepanjang 2013. Informasi ini dapat
menginformasikan masyarakat untuk menghindari berpergian dengan carrier
tersebut.

Untuk melihat kinerja dari maskapai saya akan menggunakan 3 indikator

### Indikator 1

Indikator pertama adalah ketepatan keberangkatan dan kedatangan sesuai
dengan jadwal

``` r
data %>% 
      mutate(dep_delay = abs(dep_delay),
             arr_delay = abs(arr_delay)) %>%
      select(dep_delay, arr_delay, carrier, name) %>%
      group_by(carrier) %>%
      summarise(mean_dep_delay = mean(dep_delay),
                mean_arr_delay = mean(arr_delay)) %>%
      melt() %>%
      ggplot() +
      geom_col(aes(x = reorder(carrier, value), 
                   y = value, 
                   fill = variable), 
               position = "dodge") +
      labs(title = "Rata-rata delay (menit)\nKeberangkatan dan Kedatangan Bedasarkan Maskapai",
           fill = "",
           x = "", 
           y = "") +
      scale_y_continuous(expand = c(0, NA), limits = c(0, 35)) +
      scale_fill_brewer(labels = c("Delay Keberangkatan", "Delay Kedatangan"),
                        palette = "Set2") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 15),
            plot.margin = margin(c(20,20,20,10)),
            legend.box.margin = margin(-10, 0, 0, 0))
```

  

    ## Using carrier as id variables

![](laporan_files/figure-markdown_github/unnamed-chunk-27-1.png)

Telihat berdasarkan indikator ini maskapai yang sering delay baik
keberangkatan maupun kedatangan adalah maskapai Mesa Airlines
Inc. Sementara itu maskapai yang sangat baik dalam indikator ini adalah
maskapai US Airways Inc.

### Indikator Dua

Indikator kedua adalah kesesuaian lama diudara. Misalanya jika
keberangkatan delay 2 menit maka maksimal kedatangan juga maksimal delay
2 menit, jika lebih dari itu maka akan dianggap tidak wajar, karena
melebihi waktu penerbangan yang sesungguhnya.

``` r
data %>%
      filter(arr_delay > dep_delay) %>%
      mutate(tambahan_diudara = arr_delay - dep_delay) %>%
      select(tambahan_diudara, carrier) %>%
      group_by(carrier) %>%
      summarise(jumlah = mean(tambahan_diudara)) %>%
      arrange(jumlah) %>%
      ggplot(aes(x = reorder(carrier, jumlah), 
                   y = jumlah)) +
      geom_col(fill = "#fc8d62") +
      geom_text(aes(label = round(jumlah, 1)), vjust = -0.7, size = 3) +
      labs(title = "Rata-rata Tambahan Waktu (menit)\ndi Udara Bedasarkan Maskapai",
           x = "Maskapai", 
           y = "") +
      scale_y_continuous(expand = c(0, NA), limits = c(0, 23)) +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 15),
            plot.margin = margin(c(20,20,20,10)),
            legend.box.margin = margin(-10, 0, 0, 0),
            axis.text.y = element_blank())
```

 

![](laporan_files/figure-markdown_github/unnamed-chunk-28-1.png)

Dari gambar diatas terlihat bahwa rata-rata penambahan waktu di udara
yang terlama adalah maskapai Frontier Airlines Inc. dengan rata-rata
20.8 menit. Sementara itu maskapai yang memiliki rata-rata penambahan
waktu paling sedikit adalah maskapai ExpressJet Airlines Inc.

### Indikator ketiga

Indikator ketiga adalah tahun keluaran pesawat yang dimiliki oleh
maskapai

``` r
tmp <- data %>%
      distinct(tailnum, carrier, .keep_all = TRUE) %>%
      select(year_aiplane, carrier) %>%
      na.omit()
tmp$year_aiplane <- cut(tmp$year_aiplane, 
                        breaks = c(1940, 2000, 2010, 2020),
                        labels = c("Bawah 2000", "2000an", "2010an"))

tmp %>%
      group_by(carrier, year_aiplane) %>%
      summarise(jumlah = n()) %>%
      ggplot(aes(x = reorder(carrier, jumlah), y = jumlah, fill = year_aiplane)) +
      geom_col(position = "dodge") +
      labs(title = "Jumlah Pesawat Berdasarkan Waktu Pembuatan",
           fill = "",
           x = "", 
           y = "") +
      scale_y_continuous(expand = c(0, NA), limits = c(0, 490)) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 15),
            plot.margin = margin(c(20,20,20,10)),
            legend.box.margin = margin(-10, 0, 0, 0))
```

 

![](laporan_files/figure-markdown_github/unnamed-chunk-29-1.png)

Dari gambar diatas terlihat bahwa maskapai Envoy Air (MQ) hanya memiliki
pesawat buatan tahun 2000 kebawah. Begitu juga dengan maskapai Delta Air
Lines Inc. (DL) masih banyak menggunakan pesawat buatan tahun 2000
kebawah, tentu saja hal ini dapat menjadi pertimbangan dalam menentukan
maskapai yang akan digunakan. Semakin tua pesawat maka akan semakin
beresiko pesawat itu.

Sementara itu maskapai dengan jumlah pesawat terbanyak buatan 2010an
keatas adalah maskapai Southwest Airlines Co (VN) dan terdapat juga
beberapa maskapai lainnya yang sudah memiliki pesawat buatan 2010an
keatas.

### Kesimpulan

Dari beberapa penjelasan diatas untuk menentukan carrier yang mempunyai
performa terbaik dapat dilihat dari carrier yang paling sesuai dengan
jadwal keberangkatan maupun kedatangan dan lama waktu di udara yang
sesuai. Dari hasil analisis diperoleh bahwa maskapai US Airways Inc yang
memiliki performa terbaik karena pada indikator satu menduduki peringkat
pertama dan indikator dua memduduki urutan ke 4.

Selain itu, untuk masyarakat dapat menghindari pemakaian carrier dengan
melihat jenis pesawat yang digunakan oleh maskapai tersebut, terutama
dari segi waktu pembuatan pesawat tersebut
