## Resubmission

In this resubmission I have:

* Fixed documentation on the generics that were flagged by the reviewer.
* Detailed analysis shows no use of `cat()` or `print()` outside of methods
intended to print text to the console: `show()` (full details of the object) and 
`brief()` (printing concise details of the object). `show_attributes()` is used
by `show()` only (although it is exported so users can choose to display the
attributes). Other functions do not print information to the console. The only 
other text printed to the console is upon encountering an error by `stop()`.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* There are no references to published scientific literature for this package.
