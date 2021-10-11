:raw-latex:`\pagebreak`


.. _spatial:

Spatial Support
===============


.. Note:: Work in progress only. Stay tuned!


Motivation
----------

The purpose of this section is to describe the Myriad-provided operations for various spatial operations, notably for 3D support.

We introduced these elements mostly for convenience, to have them readily available in a simple, controllable form, pure Erlang, easy to enrich and without involving extra dependencies.

This support is by no means expected to be complete, battle-tested or efficient. If looking for such traits, one may consider:

- the elements already available directly in Erlang, notably the `gl <https://erlang.org/doc/man/gl.html>`_ module, providing for example primitives to `load OpenGL matrices <https://erlang.org/doc/man/gl.html#loadTransposeMatrixd-1>`_ and operate on them
- in the Erlang community: `Wings 3D <http://www.wings3d.com/>`_, an open-source modeller `whose sources <https://github.com/dgud/wings/tree/master/src>`_ of course implement many spatial operations

- integrating advanced, non-Erlang libraries such as ones for linear operations implementing the `BLAS <https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms>`_ specification; using the C binding (*CBLAS interface*) of a renown implementation (optimised at length and making use of processor-specific extensions, such as `LAPACK <https://en.wikipedia.org/wiki/LAPACK>`_) and making it available to Erlang typically thanks to either NIFs (most suitable approach here) or a C-node (possibly thanks to `Ceylan-Seaplus <http://seaplus.esperide.org>`_) would certainly be an option - all the more relevant that a bulk of linear operations could be offset to it; some Erlang projects target similar objectives, like `linalg <https://github.com/sklassen/erlang-linalg-native>`_ or `matrex <https://github.com/versilov/matrex>`_; more generally a library such as `GSL <https://www.gnu.org/software/gsl/>`_ (the GNU Scientific Library) ideally could be integrated as a whole to Erlang


Conventions
-----------


Linear Conventions
..................

Dimensions are non-null (a zero dimension has little interest). Dimension 1 corresponds to scalar and is not special-cased (hence one shall preferably use directly scalars if able to determine that being in a single dimension context).

A linear-related **index** (ex: of a coordinate of a point, a vector or a matrix) starts at ``1`` (not 0), as by default all indices_ in Myriad.

.. Coordinates can be specified as ``linear:any_coordinate/0``, i.e. either ``linear:integer_coordinate/0`` (meaning ``integer/0``) or, more often ``linear:coordinate/0``, i.e. ``float/0`` (hence double-precision floating point values); internally all coordinates are ``float/0``.

**Points** are to be specified by the user as *tuples* (preferably to lists) whose coordinates are either integer ones (for example :math:`P = \begin{pmatrix} 10 \\ 45\end{pmatrix}` translating to ``P={10,45}``, typically for GUI-related processing of on-screen coordinates) or floating-point ones (``{0.0, -1.0, 0.0}`` for a point in 3D space). This is the most natural term mapping, and their internal representation is an homogeneous tuple (i.e. whose elements are all of the corresponding type): either ``integer_point/0`` or ``point/0``.

Points can be of arbitrary dimension (then they are taken in charge by the ``point`` module), or can be specialised for 2D, 3D or 4D (then they belong to the ``point{2,3,4}`` modules).


.. As for vectors, they are to be specified by the user as *lists* of any-coordinates, i.e. integer or floating-point ones, possibly mixed (ex: ``[0.0, -7, 3.22]``); this directly corresponds their internal representation, in order to better accommodate arbitrary dimensions and linear operations.

As for **vectors**, they are to be specified by the user as *lists* of floating-point coordinates (ex: :math:`\vec{V} = \begin{bmatrix} 0.0 \\ -7.3 \\ 3.22\end{bmatrix}` translating to ``V=[0.0, -7.3, 3.22]``); this directly corresponds to their internal representation, in order to better accommodate linear operations.

Vectors can be of arbitrary dimension (then they are taken in charge by the ``vector`` module), or can be specialised for 2D, 3D or 4D (then they belong to the ``vector{2,3,4,}`` modules).


Points and vectors (of arbitrary dimension, or specialised) can be converted both ways, see ``point*:{to,from}_vector/1`` and ``vector*:{to,from}_point/1``.


The **matrices** handled here are often square ones, and their elements are floating-point coordinates as well.

Let's consider a :math:`m × n` matrix (m rows, n columns):

.. math::
 M = \begin{bmatrix}
		a11 & a12 & ... & a1n \\
		a21 & a22 & ... & a2n \\
		... & ... & ... & ... \\
		am1 & am2 & ... & amn \\
	 \end{bmatrix}


Such a matrix may be expressed:

- as one of arbitrary dimension (designated from now on as an "*arbitrary matrix*"), corresponding to the ``matrix:matrix/0`` type; internally such matrices are nested lists: a list of ``m`` rows, each being a list of ``n`` elements, hence defined in `row-major order <https://en.wikipedia.org/wiki/Row-_and_column-major_order>`_ - not column-major one
- if being square and of a well-known dimension among 2, 3 or 4 (special cases defined for convenience and performance), as a value belonging to the ``matrix2/0``, ``matrix3/0``, ``matrix4/0`` types (which are records like ``#matrix4{}``, whose fields are named according to the matrix elements, such as ``m23``); they are designated hereafter as "*specialised matrices*"
- in a symbolic way, such as ``identity_4`` (meaning the identity 4x4 matrix)
- for some dimensions (ex: 4D), extra representations exist (compact matrices made of a 3x3 matix and a vector3)


Taking as an example a 2x2 matrix like:

.. math::
 M = \begin{bmatrix}
		a11 & a12 \\
		a21 & a22 \\
	 \end{bmatrix}

it can be created as an arbitrary ``matrix/0`` with:

.. code:: erlang

 M1 = matrix:new([ [A11,A12], [A21,A22] ])


Alternatively it can be directly created as a specialised 2x2 matrix ``matrix2`` with:

.. code:: erlang

 M2 = matrix2:new(A11, A12, A21, A22)
 % Or:
 M3 = matrix2:new([ [A11,A12], [A21,A22] ])
 % Or even:
 M4 = matrix2:new(M1)


There is a priori little interest in "unspecialising" (i.e. going from specialised to arbitrary matrix) by having:

.. code:: erlang

 M6 = matrix:new(M3)


In practice the actual, internal terms corresponding to all these matrices would be:

.. code:: erlang

 % For arbitrary ones:
 M1 = M2 = [ [A11,A12],
			 [A21,A22] ]

 % For specialised ones:
 M3 = M4 = M5 = #matrix2{ m11=A11, m12=A12,
						  m21=A21, m22=A22 }


Note that:

- we call a container *type-homogeneous* iff all the coordinates that it gathers are all either integer or floating-point ones
- newer elements (ex: matrices, vectors, points) may be:

  - either literally specified, with a term directly corresponding to their internal form
  - or based on a ``new`` operator (ex: ``matrix:new/1``), in which case with a higher-level user-term (ex: a matrix with integer coordinates, in which case they will be automatically converted to floats)
- for clarity and in order to provide them with specified operations (like dot product), we preferred defining vectors as a separate type from the matrix one (even if a vector could be represented as a 1-column matrix)
- by default, for least surprise, coordinates are displayed *not* rounded (refer to the ``printout_{width,precision}`` defines in ``linear.hrl``)
- operations are not implemented defensively, in the sense that a base runtime error will be triggered if a type or a size does not match, rather than being special-cased (anyway generally no useful context could be specifically reported)
- extra runtime checks can be enabled by setting the ``myriad_check_linear`` flag (refer to ``GNUmakevars.inc``)
- for `homogeneous coordinates <https://en.wikipedia.org/wiki/Homogeneous_coordinates#Use_in_computer_graphics_and_computer_vision>`_: any implicit homogeneous `w` coordinate is ``1.0``



Geometric Conventions
.....................

:raw-html:`<center><img src="myriad-space-time-referential.png" id="responsive-image-tiny"></img></center>`
:raw-latex:`\begin{figure}[h] \centering \includegraphics[scale=0.7]{myriad-space-time-referential.png} \end{figure}`

For **space** coordinates, three axes are defined for a global referential:

- abscissa: X axis (in red, ``#FF0000``)
- ordinate: Y axis (in green, ``#008000``)
- depth: Z axis (in blue, ``#0000FF``)

By default, we rely on "Z-up" conventions (the Z axis being vertical and designating altitudes), like modelling software such as `Blender <https://www.blender.org/>`_ [#]_.

.. [#] Unlike many games, for which the Y axis is up, Z being the depth, perpendicular to the screen. Anyway a simple camera transformation is enough to switch conventions.

For each of these dimensions, generally ``1.0`` corresponds to 1 meter, otherwise to 1 `light-second <https://en.wikipedia.org/wiki/Light-second>`_ (i.e. roughly 300 000 km [#]_).

.. [#] Then for more human-sized distances, a scale of one light-nanosecond (10^-9 second) might be more convenient, as it corresponds almost to 30 cm.

For **all angles**, the default unit is the radians, and the positive rotation is counterclockwise.


For **face culling**, front-facing is determined by relying on a counter-clockwise order winding order of triangles (like default OpenGL's `GL_CCW <https://www.khronos.org/opengl/wiki/Face_Culling>`_):

:raw-html:`<center><img src="myriad-culling-conventions.png" id="responsive-image-medium"></img></center>`
:raw-latex:`\begin{figure}[h] \centering \includegraphics[scale=0.4]{myriad-culling-conventions.png} \end{figure}`


..  Examples:
 .. math:: ax^2 + bx + c = 0
 .. :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`

Indeed a triangle enumerating its vertices in counter-clockwise order (``A->B->C``) would define a normal vector :math:`\vec{N}=\overrightarrow{AB}\times\overrightarrow{BC}` pointing towards the outside of a body comprising that triangle.

If :math:`\vec{V}\cdot\vec{N}` (i.e. the dot-product of the view direction vector and of this outward vector product) is:

- strictly negative: then the face is front-facing
- positive: then the face is rear-facing

Said otherwise, front-facing polygons are the ones whose signed area is strictly positive; see also: ``polygon:{get_area,get_signed_area}/1``.

A fourth coordinate besides X, Y and Z could be used, as an extra axis (in yellow, ``#F6DE2D``):

- either for **homogeneous** coordinates, in which case it will be considered to be spatial as well, with the same unit as the three first ones, and preferably designated as ``W``
- or for **time** coordinates, with a single axis defined for a global referential: the ``T`` one, for which ``1.0`` corresponds to 1 second



Related Services
................

Elements of interest can be:

- some support of polygons, in ``polygon.erl``
- a basic support for 2D bounding-boxes (including rectangles, circles and Minimal Enclosing Circle , see ``bounding_boxes.{hrl,erl}``
- elements to export 3D scenes with the `glTf file format`_



Possible Enhancements
.....................

In the future, the most usual spatial types such as ``matrix`` and ``vector`` may be shortened in Myriad-based code as respectively ``m`` and ``v``, the Myriad parse transform being then in charge of expanding accordingly (ex: a in-code shorthand ``m3:new/0`` becoming ``matrix3:new/0`` to the eyes of the compiler).
