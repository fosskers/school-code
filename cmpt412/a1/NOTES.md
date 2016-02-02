Picking up Pencils - Notes
==========================

2016 February 1
---------------
`multithresh` is the way to go (I actually discovered this last week)
for removing noise.

2016 January 25
---------------
- *Image Analysis -> Edge Detection*
- Things to try:
  - `imcontour`: Draws a coloured, fairly accurate contouring of the pencil
  - `imhist`: Gives a bell-curve of colour values that appear in the image
  - `bwperim`
  - `edge(I,'log',n)` where n is some threshold value

2016 January 21
---------------
- Explored the *Image Enhancement* kit.
- After doing some tests with the famous *Lena* image, `imgaussfilt`
seems to be ideal for smoothing. Experimentally, `sigma = 1` seems to
give smoothing that is pleasant to the eyes, but we'll see whether
that or the default of 0.5 is better for actual edge detection.

2016 January 18
---------------
Random thoughts, having seen the test images:
- Could the presence of shadows be used to discover "on-top" pencils?
- I need to think of a way to represent a pencil's location. The centre
  isn't good enough, given that a pencil isn't a circle like a penny.
- Pencils don't have to be their usual off-yellow. They can also be
  multicoloured.
- Pencils don't always have the same length.
- Background could be noisy. *Smooth out the image?*
- Probably no point in trying to detect the eraser, or the metal holding it.
