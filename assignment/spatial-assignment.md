The ecological and evolutionary consequences of systemic racism
================
Millie Chapman (GSI), Jiawen Tang, Mark Sun

Create the Environment before starting:

``` r
#install these for the first time to run
#install.packages(c('gdalcubes','tmap','rstac','jsonlite','condtools'))
```

    ## Breaking News: tmap 3.x is retiring. Please test v4, e.g. with
    ## remotes::install_github('r-tmap/tmap')

    ## terra 1.7.55

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::extract() masks terra::extract()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
    ## 
    ## 
    ## Attaching package: 'gdalcubes'
    ## 
    ## 
    ## The following objects are masked from 'package:terra':
    ## 
    ##     animate, crop, size
    ## 
    ## 
    ## 
    ## Attaching package: 'jsonlite'
    ## 
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

# Background

In August 2020, [Christopher
Schell](http://directory.tacoma.uw.edu/employee/cjschell) and collegues
published a review in *Science* on [‘The ecological and evolutionary
consequences of systemic racism in urban
environments’](https://science.sciencemag.org/content/early/2020/08/12/science.aay4497)
, showing how systematic racism and classism has significant impacts on
ecological and evolutionary processes within urban environments. Here we
explore a subset of the data used to support these findings in this
review and the broader literature.

The [press
release](https://www.washington.edu/news/2020/08/13/systemic-racism-has-consequences-for-all-life-in-cities/)
on the paper is worth a read:

> “Racism is destroying our planet, and how we treat each other is
> essentially structural violence against our natural world,” said lead
> author Christopher Schell, an assistant professor of urban ecology at
> the University of Washington Tacoma. “Rather than just changing the
> conversation about how we treat each other, this paper will hopefully
> change the conversation about how we treat the natural world.”

In the paper, Schell writes:

> “In multiple cases, neighborhood racial composition can be a stronger
> predictor of urban socio-ecological patterns than wealth.”

**Figure 2** in the Schell paper shows how NDVI (Normalized Difference
Vegetation Index) tracks historical redlining. ![Fig.
1.](figures/fig2.png)

In this report, we explored one metric for how structural racism and
classism underpin landscape heterogeneity in cities. We recreated the
city map of San Francisco, and plot the distributions and mean
vegetation patterns to explore the structural inequality and racism that
Schell et al highlight in their paper. We used the following spatial
data to do this:

# **1.Mapping Inequality:**

  
The introduction to the dataset we used is
[here](https://dsl.richmond.edu/panorama/redlining/#loc=3/41.245/-105.469&text=intro).
The “holc_grade” column represents the assigned grades to residential
neighborhoods that reflected their “mortgage security” from minimal risk
to being hazardous. We will then visualize these grades on color-coded
maps.

In this case, we will use San Francisco as an example to visualize the
environmental injustice caused by historical Redlining. We will then be
using Fresno for comparison. The grade is not randomly distributed
within blocks. For instance, almost all regions with low grade (C or D)
locate in current near-shore area, indicating they were poor or color
residence area before.Western San Francisco has significanrlt better
environmental condition and consists of mostly best-ranked blocks (A and
B). This map clearly shows the spatial inequality of historical
Redlining.

``` r
sfzip <-"https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CASanFrancisco1937.zip"

sfurl <- paste0("/vsizip/vsicurl/",sfzip)
sf <- read_sf(sfurl)
tmap_mode("plot")
```

    ## tmap mode set to plotting

``` r
tm_shape(sf)+tm_polygons("holc_grade")
```

![](spatial-assignment_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# **2.Normalized Difference Vegetation Index (NDVI)**

NDVI is used as proxy measure of vegetation health, cover and phenology
(life cycle stage) over large areas. It is calculated using multiple
bands (NIR - R) / (NIR + R) from satellite images.

Now we know the area that we are interested in based on the holc_grade
graph, we will start finding the NDVI, Normalized Difference Vegetation
Index in the near summer. NDVI stands for the “greenness” of the land,
with higher value indicates higher reflection in green wavelength,
suggesting more vegetation. Dr. Jim Tucker wants us using
“photosynthesis capacity” to describe NDVI, as it reflects more the
ability of the land to absorb wavelength. In previous studies, there is
a strong correlation of NDVI with wealth, since green space is a luxury
product in most place of America.

``` r
## STAC Search over 400 million assets.
box <- st_bbox(sf) 
start_date <- "2022-06-01"
end_date <- "2022-09-01"
items <- 
  stac("https://earth-search.aws.element84.com/v0/") |>
  stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox = c(box),
              datetime = paste(start_date, end_date, sep="/"),
              limit = 100) |>
  post_request() 
col <-
  stac_image_collection(items$features,
                        asset_names = c("B02", "B03", "B04","B08", "SCL"),
                        property_filter = \(x) {x[["eo:cloud_cover"]] < 20})
cube <- cube_view(srs = "EPSG:4326",  
                  extent = list(t0 = start_date, t1 = "2022-08-31",
                                left = box[1], right = box[3],
                                top = box[4], bottom = box[2]),
                  nx = 1000, ny = 1000, dt = "P1M",
                  aggregation = "median", resampling = "average")
S2.mask <- image_mask("SCL", values=c(3,8,9)) # mask clouds and cloud shadows
raster_cube(col, cube, mask = S2.mask) |>
  select_bands(c("B04", "B03", "B02")) |>
  plot(rgb = 1:3)
```

![](spatial-assignment_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

As we selected the location (San Francisco), the time (summer 2022), the
data source (ESA Sentinel-2), and the means to remove the cloud cover,
we will carry out the calculation of NDVI. NDVI is calculated as a ratio
between the red (R) and near infrared (NIR) values in traditional
fashion: (NIR - R) / (NIR + R). In this case, they are represented by
specific band (like B08, B04). We calculate a raster format ndvi and the
average ndvi per holc_grade.

``` r
ndvi <- 
  raster_cube(col, cube, mask = S2.mask) |>
  select_bands(c("B08", "B04")) |>
  apply_pixel("(B08-B04)/(B08+B04)", "NDVI") |>
  aggregate_time("P3M")
ave_ndvi <- ndvi |> extract_geom(sf, FUN = mean )
ndvi2 <- ndvi |>st_as_stars()
ave_ndvi
```

    ## # A tibble: 97 × 3
    ##      FID time        NDVI
    ##    <int> <chr>      <dbl>
    ##  1     1 2022-06-01 0.314
    ##  2     2 2022-06-01 0.407
    ##  3     3 2022-06-01 0.384
    ##  4     4 2022-06-01 0.246
    ##  5     5 2022-06-01 0.303
    ##  6     6 2022-06-01 0.390
    ##  7     7 2022-06-01 0.305
    ##  8     8 2022-06-01 0.238
    ##  9     9 2022-06-01 0.309
    ## 10    10 2022-06-01 0.268
    ## # ℹ 87 more rows

``` r
ndvi2
```

    ## stars object with 3 dimensions and 1 attribute
    ## attribute(s):
    ##             Min.    1st Qu.    Median      Mean   3rd Qu.     Max.
    ## NDVI  -0.7842497 0.02900888 0.1133295 0.1473095 0.2829977 0.930139
    ## dimension(s):
    ##      from   to offset      delta  refsys point                  values x/y
    ## x       1 1000 -122.5  0.0001474  WGS 84    NA                    NULL [x]
    ## y       1 1000  37.81 -9.867e-05  WGS 84    NA                    NULL [y]
    ## time    1    1     NA         NA POSIXct FALSE [2022-06-01,2022-09-01)

> “As you explore the materials Mapping Inequality, you will quickly
> encounter exactly that kind of language, descriptions of
> the”infiltration” of what were quite often described as “subversive,”
> “undesirable,” “inharmonious,” or “lower grade” populations, for they
> are everywhere in the HOLC archive ….These grades were a tool for
> redlining: making it difficult or impossible for people in certain
> areas to access mortgage financing and thus become homeowners.
> Redlining directed both public and private capital to native-born
> white families and away from African American and immigrant families.
> As homeownership was arguably the most significant means of
> intergenerational wealth building in the United States in the
> twentieth century, these redlining practices from eight decades ago
> had long-term effects in creating wealth inequalities that we still
> see today. Mapping Inequality, we hope, will allow and encourage you
> to grapple with this history of government policies contributing to
> inequality.”

## Map of current (2019) mean NDVI across city redlining from the 1950s.

At present, we already have the ndvi data, and we want to make sure that
the area of the two data is the same before directly displaying the ndvi
data within each tier. The following map stack our average NDVI data
with holc_grade of San Francisco.

``` r
tm_shape(ndvi2) + tm_raster(style = "quantile") + tm_shape(sf) + tm_polygons("holc_grade", alpha = 0.5) + tm_layout(legend.outside=TRUE)
```

    ## Variable(s) "NA" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.

![](spatial-assignment_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# 3. NDVI

Now that we have average NDVI data, we joined it with the sf dataset, so
that we can plot the NDVI of specific sub districts in San
Francisco.From this graph,it’s obvious that there are significant
differences in NDVI across all grades, and better grades like A and B
are associated with higher NDVI.This provides evidence for the
environmental inequality caused by historical injustic, making
vulnerable communities even more harsh and perilous.

``` r
ave_ndvi |> as_tibble()
```

    ## # A tibble: 97 × 3
    ##      FID time        NDVI
    ##    <int> <chr>      <dbl>
    ##  1     1 2022-06-01 0.314
    ##  2     2 2022-06-01 0.407
    ##  3     3 2022-06-01 0.384
    ##  4     4 2022-06-01 0.246
    ##  5     5 2022-06-01 0.303
    ##  6     6 2022-06-01 0.390
    ##  7     7 2022-06-01 0.305
    ##  8     8 2022-06-01 0.238
    ##  9     9 2022-06-01 0.309
    ## 10    10 2022-06-01 0.268
    ## # ℹ 87 more rows

``` r
sf2 <- sf |> rowid_to_column("FID")
ndvi_poly <- left_join(sf2 , ave_ndvi)
```

    ## Joining with `by = join_by(FID)`

``` r
tmap_mode("plot")
```

    ## tmap mode set to plotting

``` r
tm_basemap() + 
tm_shape(ndvi_poly) + tm_polygons("NDVI", palette = "Greens") + 
  tm_shape(ndvi_poly) + tm_text("holc_grade", size = 0.5) + 
  tm_layout(legend.outside=TRUE)
```

![](spatial-assignment_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## NDVI Average in each Grade of Land:

Now that we know the mean between each grade, can we verify it with the
overall mean? We can calculate this by taking some simple averages.Here
we summarize the mean NDVI across every holc_grade. The NDVI for the 2
worst grades (C and D) are almost the same, while the blocks with the a
little better grade (B) has slightly greater NDVI. As Redlining’s zoning
is dominated by D, it cannot be proved that Redlining has a direct
relationship with lower NDVI based on a single case in San
Francisco.However, blocks with the best grade (A) has significanrly
higher NDVI than all others (almost 150% of grade B).

``` r
ndvi_poly |> as_tibble() |>
  group_by(holc_grade) |>
  summarise(mean_NDVI = mean(NDVI))
```

    ## # A tibble: 4 × 2
    ##   holc_grade mean_NDVI
    ##   <chr>          <dbl>
    ## 1 A              0.315
    ## 2 B              0.209
    ## 3 C              0.190
    ## 4 D              0.189

## Fresno as Comparison

We will compare Fresno, an inland city, to San Francisco to increase
scientific significance. Fresno’s process resembles those for SF, and we
will merge everything directly, and present the result & final mean as a
map.

``` r
fresno <-"https://dsl.richmond.edu/panorama/redlining/static/downloads/shapefiles/CAFresno1936.zip"

fsurl <- paste0("/vsizip/vsicurl/",fresno)
fs <- read_sf(fsurl)
tmap_mode("plot")
```

    ## tmap mode set to plotting

``` r
tm_shape(fs)+tm_polygons("holc_grade")
```

![](spatial-assignment_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
box <- st_bbox(fs)
start_date <- "2022-06-01"
end_date <- "2022-08-01"
items <- 
  stac("https://earth-search.aws.element84.com/v0/") |>
  stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox = c(box),
              datetime = paste(start_date, end_date, sep="/"),
              limit = 100) |>
  post_request() 

col <-
  stac_image_collection(items$features,
                        asset_names = c("B02", "B03", "B04","B08", "SCL"),
                        property_filter = \(x) {x[["eo:cloud_cover"]] < 20})
fs_cube <- cube_view(srs = "EPSG:4326",  
                  extent = list(t0 = start_date, t1 = "2022-08-31",
                                left = box[1], right = box[3],
                                top = box[4], bottom = box[2]),
                  nx = 1000, ny = 1000, dt = "P1M",
                  aggregation = "median", resampling = "average")
S2.mask <- image_mask("SCL", values=c(3,8,9))
fs_ndvi <- 
  raster_cube(col, fs_cube, mask = S2.mask) |>
  select_bands(c("B08", "B04")) |>
  apply_pixel("(B08-B04)/(B08+B04)", "NDVI") |>
  aggregate_time("P3M")
fs_ave_ndvi <- fs_ndvi |> extract_geom(fs, FUN = mean)
ndvi3 <- fs_ndvi |>st_as_stars()
tm_shape(ndvi3) + tm_raster(style = "quantile") + tm_shape(fs) + tm_polygons("holc_grade", alpha = 0.5)+ tm_layout(legend.outside=TRUE)
```

    ## Variable(s) "NA" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.

![](spatial-assignment_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
fs_ave_ndvi |> as_tibble()
```

    ## # A tibble: 24 × 3
    ##      FID time        NDVI
    ##    <int> <chr>      <dbl>
    ##  1     1 2022-06-01 0.399
    ##  2     2 2022-06-01 0.391
    ##  3     3 2022-06-01 0.416
    ##  4     4 2022-06-01 0.307
    ##  5     5 2022-06-01 0.249
    ##  6     6 2022-06-01 0.340
    ##  7     7 2022-06-01 0.257
    ##  8     8 2022-06-01 0.254
    ##  9     9 2022-06-01 0.262
    ## 10    10 2022-06-01 0.245
    ## # ℹ 14 more rows

``` r
fs2 <- fs |> rowid_to_column("FID")
ndvi_poly2 <- left_join(fs2 , fs_ave_ndvi)
```

    ## Joining with `by = join_by(FID)`

``` r
tmap_mode("plot")
```

    ## tmap mode set to plotting

``` r
tm_basemap() + 
tm_shape(ndvi_poly2) + tm_polygons("NDVI", palette = "Greens") + 
  tm_shape(ndvi_poly2) + tm_text("holc_grade", size = 0.5)+ 
  tm_layout(legend.outside=TRUE)
```

![](spatial-assignment_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ndvi_poly2 |> as_tibble() |>
  group_by(holc_grade) |>
  summarise(mean_NDVI = mean(NDVI))
```

    ## # A tibble: 4 × 2
    ##   holc_grade mean_NDVI
    ##   <chr>          <dbl>
    ## 1 A              0.395
    ## 2 B              0.314
    ## 3 C              0.228
    ## 4 D              0.207

Fresno is a famous agricultural city, and during the planting period, we
can expect a high NDVI (any plant has a high NDVI, and in the city area
and density are more important than the type of plant). When the overall
Sample value was large, we found that the NDVI value of area D, where
Redlining was located, was significantly lower. These two very different
cities collectively suggest that NDVI provides evidence of social
injustic impact of historical racism practices like Redlining.

# **4. Conclusion**

#### **Explain why considering systematic inequity and racism is important in the context of global change and developing solutions to the biodiversity crisis.**

Considering the systematic inequality helps inform a more comprehensive
understanding on the historical background of many environmental crisis
today. For instance, despite climate change being a global environmental
issue, systematic injustices and inequality has resulted in uneven harm
against certain groups of people who are usually the least responsible
for such crisis, and have minor financial or social influence. When
developing solutions of biodiversity crisis, it’s important to identify
the most vulnerable regions/people that should be prioritized. Moreover,
future solution including regeneration and sustainability should aim for
redress the issues caused by historical inequality. As professor Schell
pointed out, incorporating environmental justice principles into our
interpretation of urban ecology and biodiversity is vital, as they laid
the foundation of effective conservation and sustainability. This also
helps to foster civic responsibility and enhance social resiliency.
