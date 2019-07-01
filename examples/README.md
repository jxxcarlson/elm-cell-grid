# The Examples

- **GameOfLife.**  The usual Conway's game of Life with
a 100x100 grid with `State = Occupied | Unoccupied`.
The cells are clickable.  Clicking toggles
the state between `Occupied` and `Unoccupied`.

- **HeatEquation.** Simulation of heat conduction in
a thin plate, using `State = Float`. There are two
versions, 'HeatEquation.elm' and `HeatEquationGL.elm`.
The first uses a 70x70 grid and renders using
`CellGrid.Render.asHtml`.  Under the hood, this function
calls `CellGrid.Render.asSvg`,  The second uses WegGL,
calling `CellGrid.WebGL.asHtml`.

- **HeatMap.** Static display of a heat map using WebGL.
The files `Image1.elm` and `Image2.elm` draw different
heat maps. 
