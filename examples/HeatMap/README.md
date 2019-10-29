# Images

Image1.elm and Image2.elm are simple programs that construct
a WebGL images from some mathematial function.

## Image1

Construct a 700x700 pixel image using 

```
   colorAtMatrixIndex : ( Int, Int ) -> ( Int, Int ) -> Vec3
```

Here 

```
   colorAtMatrixIndex ( row, col ) : ( Int, Int ) -> Vec3
```

is a function that assigns an RGB value, represented as a 3-vector : Vec3,
to position (i, j).  Such a function is fed to 
 `RenderWegGL.meshWithColorizer` to create a `Mesh Vertex` values.
 These can be rendered using `CellGrid.RenderWebGL.meshToHtml`.



## Image2

Construct a 700x700 pixel image using 

```
   colorAtMatrixIndex : ( Int, Int ) -> ( Int, Int ) -> Vec3
```

Here 

```
   colorAtMatrixIndex ( row, col ) : ( Int, Int ) -> Vec3
```

is a function that assigns an RGB value, represented as a 3-vector : Vec3,
to postion (i, j).