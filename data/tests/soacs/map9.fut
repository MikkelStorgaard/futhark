// This test fails if the ISWIM transformation messes up the size
// annotations.
fun [int] combineVs([int] n_row) =
  map(*, zip(n_row, n_row))

fun [[int]] main([int] md_starts, [[int]] md_vols, [[int]] md_drifts) =
  let e_rows = map(fn [int] ([int] x) => map(+ (2), x),
                   map(combineVs, md_vols))
  in  scan( fn [int] ([int] x, [int] y) => map(*, zip(x, y)), md_starts, e_rows )
