# List tools and Midterms
> TP3 - Semester 1 - Epita - From 10/19/21 to 10/25/21

## Toolbox (list_tools.ml)

### Basics
- length
- nth
- is_pos
- get_max

### Build and Edit
- init_list
- append
- put_list
- init_board
- is_board
- print_board
- get_cell
- put_cell

## Sort by histogram (histo.ml)

- add_occ
- get_hist
- get_sorted
- hist_sort

## Patterns and Matrices

- mat_cross
- mat_cross_diag
- mat_square
```
# print_board (mat_square 6 '*' '.') ;;
*.*.*.
.*.*.*
*.*.*.
.*.*.*
*.*.*.
.*.*.*
 - : unit = ()
```
- mat_pattern_batch
```
# print_board (mat_pattern_batch [(1, 2, 'a'); (4, 1, 'b')] (init_board (5, 5) '*')) ;;
*****
**a**
*****
*****
*b***
 - : unit = ()
```
