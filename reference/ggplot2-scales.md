# Scales to use for ggplot2

**\[experimental\]**

## Usage

``` r
scale_color_jma_absolute(type = "precipitation", ...)

scale_color_jma_relative(type = "amedas", ...)

scale_fill_jma_absolute(type = "precipitation", ...)

scale_fill_jma_relative(type = "amedas", ...)
```

## Arguments

- type:

  Display item

- ...:

  Arguments to pass on to ggplot2::scale_color_gradientn or
  ggplot2::scale_fill_gradientn

## Value

A `ScaleContinuous` object that can be added to a `ggplot` object

## Examples

``` r
d <-
data.frame(
  date = as.Date(c(17410, 17411, 17412, 17413, 17414, 17415), origin = "1970-01-01"),
  precipitation_sum = units::set_units(c(3.5, 9.5, 0, 0, 0, 5), "mm"))

library(ggplot2)
library(units)
#> udunits database from /usr/share/xml/udunits/udunits2.xml
ggplot(d, aes(date, precipitation_sum,
          color = drop_units(precipitation_sum),
          fill = drop_units(precipitation_sum))) +
  geom_bar(stat = "identity") +
  scale_color_jma_absolute(type = "precipitation") +
  scale_fill_jma_absolute(type = "precipitation")
```
