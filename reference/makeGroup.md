# Create a new group

This function creates a new group for CF objects. Groups are organized
in an hierarchy, starting with a root node. A root node is specified by
an empty string for a name and with argument `parent_group = NULL`.

## Usage

``` r
makeGroup(name = "", parent_group = NULL)
```

## Arguments

- name:

  A name for the group. This must be a valid name for CF objects. The
  default value is the empty string "".

- parent_group:

  Optionally, a parent for the current group. This must be an instance
  of `CFGroup` itself.

## Value

A new instance of `CFGroup`.

## Examples

``` r
root <- makeGroup()
sub_group <- makeGroup("sub", root)
```
