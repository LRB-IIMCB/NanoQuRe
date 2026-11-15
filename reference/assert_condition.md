# Assert condition is TRUE, stop with message if FALSE

Stops with the given message, without the call, unless the condition
evaluates to a single TRUE.

## Usage

``` r
assert_condition(cond, msg = "Assertion failed")
```

## Arguments

- cond:

  Logical condition to check

- msg:

  Error message if condition is FALSE

## Value

Invisible TRUE if assertion passes
