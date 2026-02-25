# window_intersect

given two intervals, figure out the intersection between the first and
the second

NOTE that the intervals are treated as half-open (start,finish\] so that
an event that happens exactly at time b is counted in (a,b\] but not in
(b, c\]

## Usage

``` r
window_intersect(a, b)
```

## Arguments

- a:

  the first window; here, we use all three components (start, finish,
  event)

- b:

  the second window; here, we only use the start and finish time, and
  ignore any events

## Value

a window containing the exposure and events from a that takes place
during the time given by b; also, if a has an event and it takes place
during b, it is included

## Details

TODO - should write a more detailed description
