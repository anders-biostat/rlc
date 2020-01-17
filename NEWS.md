# rlc 0.2.0

* It is possible now to create server apps that can be accessed by multiple users simultaneously. To this end, one should use arguments 
`sessionVars`, `beforeLoad` and `afterLoad` of the `openPage` functions. `sessionVars` defines local variables with their default values
for each client, `beforeLoad` and `afterLoad` are callback functions that are called once for each new web page. Other than that, there are
no differences between creating an `rlc` app locally or on a server.

* `rlc` has been restructured so that the entire app is now stored inside a single `R6` object of class `LCApp` (see man pages for more details).

* Property `axesTitlePos` added for all the charts that have axes. This property allows to place axis labels above or below the x-axis 
(to the left or to the right from the y-axis) and at the end, in the middle or next to the start of the axis.

* Now scatter plots in canvas mode also have ticks that indicate presence of points outside of the current axes range.

* NAs are correctly processed.

* Bug with mispositioning a click in Firefox v.70 fixed.

