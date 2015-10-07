# STL3: Seasonal Trend Decomposition using Loess

This package contains enhancements to the `stl` implementation that comes with base R.  Ryan is making it public here.

Here are some of the added features over `stl` done by Ryan Hafen:

- Can handle NA values
- Higher order loess smoothing (more than just local constant and linear)
- Automated parameter choices for local quadratic
- Plot methods for diagnostics

Here are some of the features done by Xiaosu Tong:

- Can pass a custom initial trend component 
- Can pass a custom initial fc.first and fc.second components

For experimental inference, prediction, and variance reduction at endpoints, see the [operator](http://github.com/hafen/operator) package.

## References

- [Cleveland, R. B., Cleveland, W. S., McRae, J. E., & Terpenning, I. (1990). STL: A seasonal-trend decomposition procedure based on loess. *Journal of Official Statistics*, 6(1), 3-73.](http://cs.wellesley.edu/~cs315/Papers/stl%20statistical%20model.pdf)
- [Hafen, R. P. "Local regression models: Advancements, applications, and new methods." (2010).](http://search.proquest.com/docview/749923640)

## Installation

```s
library(devtools)
#Ryan's version
install_github("stl2", "hafen") 
#Xiaosu's version
install_github("XiaosuTong/stl3")
```

## License

This software is released under the BSD license.  Please read the [license](https://github.com/hafen/stl2/blob/master/LICENSE.md) document.

