# Coerce an EQ5D object

Methods to convert an EQ5D object to a data frame, tibble or data.table.

## Usage

``` r
# S3 method for class 'EQ5D'
as.data.frame(x, row.names, optional, ...)

# S3 method for class 'EQ5D'
as.data.table(x, keep.rownames, ...)

# S3 method for class 'EQ5D'
as_tibble(x, ..., .rows, .name_repair, rownames)
```

## Arguments

- x:

  An EQ5D object.

- row.names:

  Not used.

- optional:

  Not used.

- ...:

  Not used.

- keep.rownames:

  Not used.

- .rows:

  Not used.

- .name_repair:

  Not used.

- rownames:

  Not used.

## Value

An appropriate representation of the data frame underlying the EQ5D
object. Only column names are preserved with all other attributes,
including row names, dropped.

## Note

Apart from `x` no other parameters are used and they are only present in
the method signatures for compatibility with the underlying generic.
