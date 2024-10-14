---
name: Bug report
about: Create a report on a bug found while running the code
title: "[BUG]: "
labels: bug
assignees: pvanlaake

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Provide a code example to reproduce the error:

```
# If the netCDF resource is online in a THREDDS server, provide the URL here.
# If not on a THREDDS server but in a repository, provide the location of the repository.
# If a file on your system, share via online file sharing if possible.
url <- "/uri/to/the/netcdf-resource.nc"
ds <- open_ncdf(url)
...
```

**Expected behavior**
A clear and concise description of what you expected to happen.

**Your environment:**
 - OS: 
 - R version: 
 - ncdfCF version:

**Additional context**
Add any other context about the problem here.
