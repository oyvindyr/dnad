# How dnad generates Fortran

dnad is a set of [fypp](https://fypp.readthedocs.io) macros that write the dual and hyper-dual number types of
forward-mode automatic differentiation, and every operator and intrinsic on them, into the module that uses
them. This document explains the idea, the Python objects the macros take, and how to extend them.

## The idea

### Why macros instead of a library module

A compiler does not inline a procedure from another object file unless IPO/LTO is enabled. In AD code, `u*v`
on two duals is a function call, and the hot loops are made of nothing else. dnad therefore does not ship a
module to `use`. A consumer expands the macros inside its own module, so every operator is a private procedure
of that module, and the compiler can inline it like local code.

Generating every procedure into every consumer would be large, so the consumer says exactly what it needs:
- which **kinds** of dual number (a kind is a set of independent variables: its count, and whether it also
  has a hyper-dual with a hessian),
- which **interfaces** (`+`, `sqrt`, ...),
- a **config** shared by all of them (real kind, stringent NaN checks, purity).

### One object per specific

The generic interface (`interface operator (*)`) and its specifics (`mult__dxy_dxy`, `mult__dxy_r`, ...) are
written by two different macros, in two different parts of the module. They must name the same procedures.
dnad keeps them consistent by building both from the same Python objects:

```
                      dnad.interfaces(...) ──► [[OpInstance, ...], ...] ──► generic(insts)          interface blocks
kinds, config,  ──►
selection             dnad.implementations(...) ─► {family: Family(d, hd)} ──► instances(macro, insts)  procedures
```

An `OpInstance` is one operator for one number type (`*` for `dual__xy_t`). It knows:
- the specific names it adds to the interface (`specifics`),
- every field a template needs to write those procedures (`type_name`, `sn`, `real_kind`, ...).

A template takes that one object, `i`, and reads all of its names from it. No name is assembled in two places.

fmoor uses the same pattern for its own generic routines, and the same two helper macros (see
[dnad_helpers.fypp](#the-two-helper-macros)).

## Using dnad from a module

```fortran
#:include 'dnad_module.fypp'
#:include 'dnad_dual_types.fypp'
#:set kinds = [dnad.Kind('xy', 2, hyper=True)]
#:set config = dnad.Config('dp', stringent=False)
module example_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    private
    public :: dual__xy_t, hdual__xy_t, operator (*), sqrt, fvalue, gradient, hessian, initialize
    $:dnad_dual_type(kinds[0], 'dp', 2)
    $:dnad_module_spec(kinds, config, ['*', 'sqrt'])
contains
    $:dnad_module_impl(kinds, config, ['*', 'sqrt'])
end module
```

Run fypp with dnad importable and its include directories on the path:

```python
import dnad, fypp
options = fypp.FyppOptions()
options.modules = ['dnad']                # `dnad` is a name inside the templates
options.includes = dnad.include_dirs()    # src/dnad and src/dnad/overload_implementations
fypp.Fypp(options).process_file('example_mod.fypp', 'example_mod.f90')
```

On the command line: `fypp -I src/dnad -I src/dnad/overload_implementations -M src -m dnad in.fypp out.f90`.

The result contains, among the getters and `initialize`:

```fortran
    type :: dual__xy_t
        real(dp) :: f = 0  ! functional value
        real(dp) :: g(2) = 0  ! derivatives
    end type
    type :: hdual__xy_t
        type(dual__xy_t) :: d  ! dual number
        real(dp) :: h(2*(2 + 1)/2) = 0  ! Lower triangular of Hessian
    end type
    ...
    interface operator (*)
        !! Multiplication
        module procedure mult__dxy_dxy
        module procedure mult__dxy_i
        module procedure mult__dxy_r
        module procedure mult__i_dxy
        module procedure mult__r_dxy
        module procedure mult__hdxy_hdxy
        ...
    end interface
    interface sqrt
        !! Square root
        module procedure sqrt__dxy
        module procedure sqrt__hdxy
    end interface
contains
    ...
    elemental function mult__dxy_dxy(u, v) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t), intent(in) :: v
        type(dual__xy_t) :: res

        res%f = u%f*v%f
        res%g = u%g*v%f + u%f*v%g
    end function
```

`dnad_module_spec` goes in the specification part and `dnad_module_impl` after `contains`, always with the same
arguments. The type definitions are separate (`dnad_dual_type`), because a consumer usually defines its types
once in a type module and includes the operators in many compute modules.

## The Python API (`src/dnad/__init__.py`)

### `Kind`, `Dual`, `Config`

| Object | Meaning | Fields the templates read |
|---|---|---|
| `Kind(suffix, nderiv, hyper=False, unroll=False)` | One set of `nderiv` independent variables | `suffix`, `nderiv`, `hyper`, `unroll` |
| `Dual(kind, hyper=False)` | The dual or the hyper-dual number of a kind | `sn` (`dxy`, `hdxy`), `type_name` (`dual__xy_t`, `hdual__xy_t`), `gdim` |
| `duals(kinds)` | All duals of `kinds`, then all hyper-duals | |
| `Config(real_kind='dp', stringent=True, pure=True)` | Settings shared by every procedure | `real_kind`, `stringent`, `elemental`, `purity` |

- `hyper=True` adds `hdual__<suffix>_t`: the dual part `d` plus the packed lower triangle `h` of the hessian, in
  the order `do j = 1, n; do i = j, n`.
- `unroll=True` writes the hessian loops of the hyper-dual operators out for `nderiv` instead of looping.
  `Dual.gdim` is `nderiv` then, and `None` otherwise.
- `stringent=True` makes the functions return NaN derivatives where the derivative does not exist (`abs(0)`,
  `sqrt(0)`, ties in `max`). It costs speed, and it adds `negative_one` and `set_NaN` to the module.
- `pure=False` makes the procedures impure, for printing while debugging.

### The operator catalog `OPS`

Each overloadable interface is one `Op`:

```python
Op('*', 'operator (*)', 'mult', 'Multiplication', _BINARY, hyper=True)
#   key  interface      family  doc               signatures  hyper-dual specifics exist
```

- **`key`** is what a selection names: `'*'`, `'sqrt'`. `dnad.all_interfaces` lists them all, in catalog order.
- **`interface`** is the Fortran generic name.
- **`family`** names the implementation macros `<family>_d` and `<family>_hd`. Aliases share a family:
  `abs` and `dabs` are both implemented by `abs_d`, and `cos` and `dcos` by `cos_d`.
- **`doc`** becomes the interface's `!!` line.
- **`signatures`** list the specifics as argument tokens: `'u'` is the number itself, `'r'` a real, `'i'` an
  integer, `'v'` a vector. `_BINARY` is `(u,u), (u,i), (u,r), (i,u), (r,u)`.
- **`hyper`** means hyper-dual specifics exist. Without it, only duals get the operator.

The specific names come from the signatures: `<family>__` and then the tokens joined by `_`, with `u` replaced
by the number's `sn`. So `('r', 'u')` for `hdxy` is `mult__r_hdxy`. A template must define its procedures under
exactly these names, in any order.

### `OpInstance`, `interfaces()` and `implementations()`

`OpInstance(op, num, config)` is the `i` a template receives. It forwards the fields of all three:

| `i.` | From | Example |
|---|---|---|
| `interface`, `doc`, `specifics` | the op (and the number) | `'operator (*)'`, `['Multiplication']`, `['mult__dxy_dxy', ...]` |
| `sn`, `type_name`, `hyper`, `gdim` | the number | `'dxy'`, `'dual__xy_t'`, `False`, `None` |
| `real_kind`, `stringent`, `elemental`, `purity` | the config | `'dp'`, `False`, `'elemental'`, `'pure'` |

- `interfaces(kinds, config, selection=None)` gives one list of `OpInstance` per selected interface, in catalog
  order. Each list is what `generic()` takes.
- `implementations(kinds, config, selection=None)` gives `{family: Family(d, hd)}`, with the dual and the
  hyper-dual instances apart. A family is implemented once, even when two selected keys share it (`abs`,
  `dabs`). A family nothing selected has empty lists, so its macro call emits nothing.
- `selection` is a list of keys or `None` for all. An unknown key raises `KeyError`.

## The fypp side

### The two helper macros

[`dnad_helpers.fypp`](../src/dnad/dnad_helpers.fypp) turns lists of instances into Fortran. An instance is
anything with `interface`, `specifics` and `doc`:

```fortran
#:def generic(insts)       #! interface <insts[0].interface>, !! doc, module procedure for every specific
#:def instances(macro, insts)   #! $:macro(i) for each i
```

`generic([])` emits nothing, so an interface with no instance in this module disappears.

### Interfaces and implementations

[`dnad_overload_interfaces.fypp`](../src/dnad/dnad_overload_interfaces.fypp) is a loop:

```fortran
#:def dnad_overload_interfaces(kinds, config, selection=None)
  #:for insts in dnad.interfaces(kinds, config, selection)
$:generic(insts)
  #:endfor
#:enddef
```

[`dnad_overload_implementations.fypp`](../src/dnad/dnad_overload_implementations.fypp) includes one file per
family and calls each family's macros:

```fortran
#:def dnad_overload_implementations(kinds, config, selection=None)
  #:set impl = dnad.implementations(kinds, config, selection)
$:instances(mult_d, impl['mult'].d)
$:instances(mult_hd, impl['mult'].hd)
...
$:instances(sqrt_d, impl['sqrt'].d)
$:instances(sqrt_hd, impl['sqrt'].hd)
...
#:enddef
```

The comparisons take a single macro for both (`$:instances(eq, impl['eq'].d)` and `.hd`), since they compare
only the function values.

### An implementation macro

Every implementation macro takes one `OpInstance` and writes all of its specifics. A hand-written example,
from [`sqrt.fypp`](../src/dnad/overload_implementations/sqrt.fypp):

```fortran
#:def sqrt_d(i)
    ${i.elemental}$ function sqrt__${i.sn}$(u) result(res)
        type(${i.type_name}$), intent(in) :: u
        type(${i.type_name}$) :: res
        real(${i.real_kind}$) :: t0
      #:if i.stringent
        integer :: i
      #:endif

        t0 = sqrt(u%f)
        res%f = t0
      #:if i.stringent
        if (res%f /= 0) then
            res%g = 0.5_${i.real_kind}$*u%g/t0
        else
            do i = 1, size(u%g)
                if (u%g(i) == 0) then
                    res%g(i) = 0
                else
                    res%g(i) = set_NaN()
                end if
            end do
        end if
      #:else
        res%g = 0.5_${i.real_kind}$*u%g/t0
      #:endif
    end function
#:enddef
```

For `Kind('xy', 2)` with `stringent=False` it expands to:

```fortran
    elemental function sqrt__dxy(u) result(res)
        type(dual__xy_t), intent(in) :: u
        type(dual__xy_t) :: res
        real(dp) :: t0

        t0 = sqrt(u%f)
        res%f = t0
        res%g = 0.5_dp*u%g/t0
    end function
```

The rules for a template:
- Take only `i` (or `kinds`/`config` for the non-operator macros), and read every name from it.
- Branch with fypp on `i.stringent`, `i.gdim` and `i.hyper`, never on a string.
- Define exactly the procedures in `i.specifics`, with those names. A missing or extra one is a compile error
  in the consumer.

### Hand-written and generated templates

Templates in `overload_implementations/` come in two sorts:
- **Hand-written:** `assign`, `pow`, `hypot`, `sqrt`, the comparisons, and `math.fypp` (all the unary intrinsics
  and the array functions).
- **Generated from sympy** by [`src/fortran_gen.py`](../src/fortran_gen.py) (the files ending in `_gen.fypp`):
  `add`, `minus`, `mult`, `div`, `log`. sympy differentiates the operator and prints the gradient and hessian
  expressions into a macro that takes `i`.

`hypot_gen.fypp`, `pow_gen.fypp` and `sqrt_gen.fypp` are generated but never included: the hand-written
versions override them. Do not edit a `_gen.fypp` file by hand. Change `fortran_gen.py` and regenerate.

### Other macros

These take the kinds and the config directly, not instances:

| Macro | File | Writes |
|---|---|---|
| `dnad_dual_type(kind, real_kind, size_dx, statement="")` | `dnad_dual_types.fypp` | `dual__<s>_t`, and `hdual__<s>_t` if hyper |
| `dnad_initialize_interface(kinds)` / `_implementations(kinds, config)` | `dnad_initialize.fypp` | `initialize` |
| `dnad_getter_interfaces(kinds)` / `_implementations(kinds, config)` | `dnad_getters.fypp` | `fvalue`, `gradient`, `gradient_d`, `hessian`, `downgrade` |
| `dnad_jacobi_interfaces(kinds)` / `_implementations(kinds, config)` | `dnad_jacobi.fypp` | `jacobi`, `jacobi_tr`, `jacobi_d`, `jacobi_tr_d` |
| `dnad_chain_duals_interface(chains)` / `_implementations(chains, config)` | `dnad_chain_duals.fypp` | `chain_duals` |
| `dnad_module_spec(...)` / `dnad_module_impl(...)` | `dnad_module.fypp` | all of the above, plus the operators |

`chains` are `(y, x)` pairs of kinds. `chain_duals` turns a number differentiated with respect to kind `y` into
one differentiated with respect to kind `x`, given `y` as a function of `x`. It is hyper-dual only when both
kinds are hyper.

## fypp details that matter here

- **`$:` vs `@:`.** `$:macro(args)` evaluates the arguments as Python. `@:macro(args)` passes them as text.
  Calls that pass objects (`kinds`, `config`, `impl['add'].d`) must use `$:`.
- **Macro names share the namespace of Python builtins.** A macro called `abs` or `max` hides the builtin
  within every template. That is why the families have `_d`/`_hd` suffixes (`abs_d`, `max_d`).
- **`#:include`** searches the directory of the including file first, then the include path. That is why
  `dnad.include_dirs()` lists both `src/dnad` and `overload_implementations`.
- **Indentation of `#:` lines** is free; indent them to follow the Fortran around them.

## Tests

The tests are generated, so that every procedure the module macros can emit is checked:

```
uv run --group dev python test/gen_tests.py    # writes test/dnad_all_*_mod.f90 and test/test_all_*_mod.f90
uv run python test/coverage.py                 # gfortran --coverage + gcov: every line must run
fpm test                                       # the generated tests, and the hand-written ones
```

How [`test/gen_tests.py`](../test/gen_tests.py) works:
- It expands `dnad_module_spec/impl` with all interfaces for three kinds: one looped, and two unrolled with 2
  and 3 derivatives. It does this once with `stringent=True` and once with `stringent=False`.
- It reads the list of specifics from the generated module, and fails if any of them has no check.
- Each check calls the **generic** (`u*v`, `sqrt(u)`), never the specific. The tests therefore do not depend on
  how the specifics are named.
- An input is a second-order Taylor polynomial `u(x) = f + g·x + ½ xᵀHx`. The reference value, gradient and
  hessian of `F(u(x), v(x))` at `x = 0` follow from the second-order chain rule, with the partial derivatives of
  `F` from sympy.
- Each branch of a template gets an input: both signs of `abs`, either argument larger in `max`, and, when
  stringent, the points where the derivative is undefined (NaN expected).

[`test/coverage.py`](../test/coverage.py) builds the generated modules with gcov and reports every procedure
that never ran, and every line in it. It exits nonzero below 100%.

## Recipes

### Add an overloaded function

Example: `cosh` for duals.

1. Add the op to `OPS` in `__init__.py`:
   ```python
   Op('cosh', 'cosh', 'cosh', 'Hyperbolic cosine', _UNARY),
   ```
2. Add the macro to `math.fypp` (or a new file, included from `dnad_overload_implementations.fypp`):
   ```fortran
   #:def cosh_d(i)
       ${i.elemental}$ function cosh__${i.sn}$(u) result(res)
           type(${i.type_name}$), intent(in) :: u
           type(${i.type_name}$) :: res

           res%f = cosh(u%f)
           res%g = u%g*sinh(u%f)
       end function
   #:enddef
   ```
3. Call it in `dnad_overload_implementations`: `$:instances(cosh_d, impl['cosh'].d)`.
4. Add a row to `ELEMENTAL` in `test/gen_tests.py`: the call format, the sympy reference, and the input domains.
   ```python
   'cosh': (('cosh({0})',), lambda a: smooth(sp.cosh, a), [pos]),
   ```
   Then run the three test commands. Generation fails until the new specific has a check, and the coverage gate
   fails until every line runs.

For a hyper-dual version, set `hyper=True` on the op, write `cosh_hd(i)` (handle `i.gdim` for the unrolled
hessian), and call it with `impl['cosh'].hd`.

### Add a signature to an existing operator

Add the tuple to the op's `signatures` and define the procedure under the name it produces in the family's
macro. For a generated family, change `fortran_gen.py` and regenerate.

### Add an alias

An alias is an `Op` with its own key and interface and an existing family, like `dabs` → `abs`. No macro
changes.

## Known limits

- `abs` writes `sign(1.0_dp, …)` and `gradient_d`/`jacobi_d` declare `real(dp)`, so any `real_kind` other than
  `dp` fails to compile. The tests use only `dp`.
- `dnad_display.fypp` (printing) is not part of the module macros and is not tested.
