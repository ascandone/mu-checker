# μ-calculus checker

A model checker that verifies μ-calculus formulae over CCS programs

```
// A reusable boolean register
BoolRegT(write_t, write_f, read_t, read_f) =
  read_t!.BoolRegT(write_t, write_f, read_t, read_f) +
  write_t?.BoolRegT(write_t, write_f, read_t, read_f) +
  write_f?.BoolRegF(write_t, write_f, read_t, read_f)

BoolRegF(write_t, write_f, read_t, read_f) =
  read_f!.BoolRegF(write_t, write_f, read_t, read_f) +
  write_t?.BoolRegT(write_t, write_f, read_t, read_f) +
  write_f?.BoolRegF(write_t, write_f, read_t, read_f)


// An example property: (deadlock freedom)
@specs nu p . <true> true && [true] p
Main = BoolRefF(w_t, w_f, r_t, r_f)
```
