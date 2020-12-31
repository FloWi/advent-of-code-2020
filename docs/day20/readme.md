# day 20

## transformations

I didn't want to calculate the image transformations myself, so I used java's `java.awt.geom.AffineTransform`.
I created a 4x4 testimage with 16 different shades of red to calculate a hash of the overall image to check, which transformation yield the same result.

all 18 possible transformations

- Clockwise180
- Clockwise180,FlipHorizontal
- Clockwise180,FlipVertical
- Clockwise270
- Clockwise270,FlipHorizontal
- Clockwise270,FlipVertical
- Clockwise90
- Clockwise90,FlipHorizontal
- Clockwise90,FlipVertical
- FlipHorizontal
- FlipHorizontal,Clockwise180
- FlipHorizontal,Clockwise270
- FlipHorizontal,Clockwise90
- FlipVertical
- FlipVertical,Clockwise180
- FlipVertical,Clockwise270
- FlipVertical,Clockwise90
- NoOp

### examples of different transformations yielding the same image

| image                                                                                                                                                                | transformations            | hash                                     |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------- | ---------------------------------------- |
| <img src="./image-transformations/image_hash_0153045607590105120135150165180195210225_NoOp.png" width="50" style="image-rendering: pixelated" />                     | NoOp                       | 0153045607590105120135150165180195210225 |
| <img src="./image-transformations/image_hash_0601201801575135195309015021045105165225_Clockwise270FlipVertical.png" width="50" style="image-rendering: pixelated" /> | Clockwise270, FlipVertical | 0601201801575135195309015021045105165225 |
| <img src="./image-transformations/image_hash_0601201801575135195309015021045105165225_FlipVerticalClockwise90.png" width="50" style="image-rendering: pixelated" />  | FlipVertical, Clockwise90  | 0601201801575135195309015021045105165225 |
| <img src="./image-transformations/image_hash_1801952102251201351501656075901050153045_Clockwise180FlipVertical.png" width="50" style="image-rendering: pixelated" /> | Clockwise180, FlipVertical | 1801952102251201351501656075901050153045 |
| <img src="./image-transformations/image_hash_1801952102251201351501656075901050153045_FlipHorizontal.png" width="50" style="image-rendering: pixelated" />           | FlipHorizontal             | 1801952102251201351501656075901050153045 |

### There are only 8 distinct transformations

| image                                                                                                                                                               | transformations           | hash                                     |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------- | ---------------------------------------- |
| <img src="./image-transformations/image_hash_0153045607590105120135150165180195210225_NoOp.png" width="50" style="image-rendering: pixelated" />                    | NoOp                      | 0153045607590105120135150165180195210225 |
| <img src="./image-transformations/image_hash_4510516522530901502101575135195060120180_Clockwise90.png" width="50" style="image-rendering: pixelated" />             | Clockwise90               | 4510516522530901502101575135195060120180 |
| <img src="./image-transformations/image_hash_2252101951801651501351201059075604530150_Clockwise180.png" width="50" style="image-rendering: pixelated" />            | Clockwise180              | 2252101951801651501351201059075604530150 |
| <img src="./image-transformations/image_hash_1801206001951357515210150903022516510545_Clockwise270.png" width="50" style="image-rendering: pixelated" />            | Clockwise270              | 1801206001951357515210150903022516510545 |
| <img src="./image-transformations/image_hash_2251651054521015090301951357515180120600_Clockwise90FlipVertical.png" width="50" style="image-rendering: pixelated" /> | Clockwise90, FlipVertical | 2251651054521015090301951357515180120600 |
| <img src="./image-transformations/image_hash_1801952102251201351501656075901050153045_FlipHorizontal.png" width="50" style="image-rendering: pixelated" />          | FlipHorizontal            | 1801952102251201351501656075901050153045 |
| <img src="./image-transformations/image_hash_4530150105907560165150135120225210195180_FlipVertical.png" width="50" style="image-rendering: pixelated" />            | FlipVertical              | 4530150105907560165150135120225210195180 |
| <img src="./image-transformations/image_hash_0601201801575135195309015021045105165225_FlipVerticalClockwise90.png" width="50" style="image-rendering: pixelated" /> | FlipVertical, Clockwise90 | 0601201801575135195309015021045105165225 |

## image transformations

take this tile from the example input

```text
Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..
```

### tiles as images

| image                                                                                                               | transformations           |
| ------------------------------------------------------------------------------------------------------------------- | ------------------------- |
| <img src="./tile-transformations/1951_NoOp.png" width="50" style="image-rendering: pixelated" />                    | NoOp                      |
| <img src="./tile-transformations/1951_Clockwise180.png" width="50" style="image-rendering: pixelated" />            | Clockwise180              |
| <img src="./tile-transformations/1951_Clockwise270.png" width="50" style="image-rendering: pixelated" />            | Clockwise270              |
| <img src="./tile-transformations/1951_Clockwise90.png" width="50" style="image-rendering: pixelated" />             | Clockwise90               |
| <img src="./tile-transformations/1951_Clockwise90FlipVertical.png" width="50" style="image-rendering: pixelated" /> | Clockwise90, FlipVertical |
| <img src="./tile-transformations/1951_FlipHorizontal.png" width="50" style="image-rendering: pixelated" />          | FlipHorizontal            |
| <img src="./tile-transformations/1951_FlipVertical.png" width="50" style="image-rendering: pixelated" />            | FlipVertical              |
| <img src="./tile-transformations/1951_FlipVerticalClockwise90.png" width="50" style="image-rendering: pixelated" /> | FlipVertical, Clockwise90 |
