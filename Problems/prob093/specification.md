---
Title: Cube Sudoku
Proposer: Patrick Schibli
Category: Games and puzzles
---

Cube Sudoku is a 3D Sudoku variant with six connected 9 × 9 Sudoku grids on the
surface of a cube. Each face is partitioned into nine 3 × 3 boxes. An edge cell
belongs to two faces and a corner cell belongs to three, linking the grids into
a single constraint satisfaction problem.

## Constraints

Each physical cell holds a digit from 1 to 9. Every row, column and 3 × 3 box
on every face must contain all nine digits. A given fixes the digit in its
physical cell and therefore applies on every face containing that cell.

The reference model uses 386 variables, one for each point `(x, y, z)` in
`{0, ..., 8}³` with at least one coordinate equal to 0 or 8. The six faces give
162 row, column and box groups before deduplication. Each of the 12 cube edges
appears twice, leaving 150 distinct `AllDifferent` constraints. A solution is
an assignment satisfying those constraints and all givens.

## Instance format

For the cell order, visit the fixed axes in the order `x`, `y`, `z`, taking
the face at coordinate 0 before the face at coordinate 8 on each axis. On each
face, scan rows and then columns in increasing order. Of the two remaining
axes, the first in `x`, `y`, `z` order is the column coordinate and the second
is the row coordinate. Number cells starting at zero when first encountered.
Reuse the existing index on subsequent encounters. `topology.json` lists the
ordered coordinates and the groups as zero-based cell indices.

`examples.json` contains five training instances, one for each published app
difficulty tier. Each record includes its release ID (`puzzle_id`), its tier
(`app_difficulty`), a 386-digit `givens` string and the corresponding `solution`
string. Both strings use the cell order above. A `0` in `givens` marks a blank
cell. The difficulty tier describes the app's grading of the puzzle. Solver
performance depends on the model and backend.

| App difficulty | Release ID | Givens |
| --- | --- | ---: |
| Easy | `cs-003184e3b4268dccde586ca1` | 178 |
| Medium | `cs-010db12304914241d697a418` | 127 |
| Hard | `cs-001e55ae9bfad7044033cbbe` | 77 |
| Expert | `cs-0096783679b61fb5e6aa593e` | 76 |
| Master | `cs-00061facd4160521fe80624d` | 79 |

The examples and topology are from Patrick Schibli / Dream Factory's
[Cube Sudoku Benchmark v1.0.0](https://doi.org/10.5281/zenodo.24342566),
licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
The full release contains 5,000 puzzles across fixed training, validation and
test splits. The [benchmark methodology and cube topology](https://www.dream-factory.app/cubesudoku/en/datasets)
explain the cell order, the app's difficulty ratings and how puzzle uniqueness
was independently checked with Z3.
