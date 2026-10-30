''' The types the dnad fypp macros take, and the catalog of overloaded operators and functions.

    kinds = [dnad.Kind('xy', 2, hyper=True)]
    config = dnad.Config('dp', stringent=False)
    $:dnad_module_spec(kinds, config, ['+', '*', 'sqrt'])
'''
import os


class Kind:
    ''' One set of independent variables: gives dual__<suffix>_t, and hdual__<suffix>_t if hyper.
    unroll: write the hessian of the hyper-dual operators out for nderiv instead of looping. '''

    def __init__(self, suffix, nderiv, hyper=False, unroll=False):
        self.suffix = suffix
        self.nderiv = nderiv
        self.hyper = hyper
        self.unroll = unroll

    def __repr__(self):
        return self.suffix


class Dual:
    ''' The dual (hyper=False) or hyper-dual number of a Kind '''

    def __init__(self, kind, hyper=False):
        self.kind = kind
        self.hyper = hyper

    @property
    def sn(self):
        ''' Short name used in procedure names: dten, hdten '''
        return ('hd' if self.hyper else 'd') + self.kind.suffix

    @property
    def type_name(self):
        ''' Fortran type name: dual__ten_t, hdual__ten_t '''
        return ('hdual__' if self.hyper else 'dual__') + self.kind.suffix + '_t'

    @property
    def gdim(self):
        ''' The number of derivatives when the hessian code is unrolled, else None '''
        return self.kind.nderiv if self.kind.unroll else None

    def __repr__(self):
        return self.sn


def duals(kinds):
    ''' All dual numbers of the kinds, then all hyper-dual numbers '''
    return [Dual(k) for k in kinds] + [Dual(k, True) for k in kinds if k.hyper]


class Config:
    ''' Settings shared by all generated procedures.
    stringent: functions return NaN where the derivative does not exist (slower).
    pure: False makes the elemental procedures impure, e.g. to print from them while debugging. '''

    def __init__(self, real_kind='dp', stringent=True, pure=True):
        self.real_kind = real_kind
        self.stringent = stringent
        self.pure = pure

    @property
    def elemental(self):
        return 'elemental' if self.pure else 'impure elemental'

    @property
    def purity(self):
        return 'pure' if self.pure else ''


class Op:
    ''' One overloaded operator or function.
    key: what a selection names ('+', 'sqrt'); interface: the Fortran generic ('operator (+)', 'sqrt');
    family: the implementation macros (<family>_d, <family>_hd), shared by aliases such as abs and dabs;
    signatures: the specifics as argument tokens, 'u' for the dual type, 'r' real, 'i' integer, 'v' a vector
    of the dual type; hyper: hyper-dual specifics exist. '''

    def __init__(self, key, interface, family, doc, signatures, hyper=False):
        self.key = key
        self.interface = interface
        self.family = family
        self.doc = [doc]
        self.signatures = signatures
        self.hyper = hyper


_BINARY = (('u', 'u'), ('u', 'i'), ('u', 'r'), ('i', 'u'), ('r', 'u'))
_UNARY = (('u',),)


def _compare(key, family, what):
    return Op(key, f'operator ({key})', family, f'{what} of the function values', _BINARY, hyper=True)


OPS = [
    Op('=', 'assignment (=)', 'assign', 'Assign an integer or a real to a dual number, or the function value to them',
       (('u', 'i'), ('u', 'r'), ('i', 'u'), ('r', 'u')), hyper=True),
    Op('+', 'operator (+)', 'add', 'Unary plus and addition', _UNARY + _BINARY, hyper=True),
    Op('-', 'operator (-)', 'minus', 'Negation and subtraction', _UNARY + _BINARY, hyper=True),
    Op('*', 'operator (*)', 'mult', 'Multiplication', _BINARY, hyper=True),
    Op('/', 'operator (/)', 'div', 'Division',
       (('u', 'u'), ('u', 'r'), ('r', 'u'), ('u', 'i'), ('i', 'u')), hyper=True),
    Op('**', 'operator (**)', 'pow', 'Power', (('u', 'i'), ('u', 'r'), ('u', 'u')), hyper=True),
    Op('hypot', 'hypot', 'hypot', 'Euclidean distance sqrt(x**2 + y**2)',
       (('u', 'u'), ('u', 'r'), ('r', 'u')), hyper=True),
    _compare('==', 'eq', 'Equality'),
    _compare('<=', 'le', 'Less than or equal'),
    _compare('<', 'lt', 'Less than'),
    _compare('>=', 'ge', 'Greater than or equal'),
    _compare('>', 'gt', 'Greater than'),
    _compare('/=', 'ne', 'Inequality'),
    Op('abs', 'abs', 'abs', 'Absolute value', _UNARY),
    Op('dabs', 'dabs', 'abs', 'Absolute value (old Fortran name)', _UNARY),
    Op('acos', 'acos', 'acos', 'Arccosine', _UNARY),
    Op('asin', 'asin', 'asin', 'Arcsine', _UNARY),
    Op('atan', 'atan', 'atan', 'Arctangent', _UNARY),
    Op('atan2', 'atan2', 'atan2', 'Arctangent of y/x in the quadrant of (x, y)', _UNARY),
    Op('cos', 'cos', 'cos', 'Cosine', _UNARY),
    Op('dcos', 'dcos', 'cos', 'Cosine (old Fortran name)', _UNARY),
    Op('dot_product', 'dot_product', 'dot_product', 'Dot product of two dual vectors', (('u', 'u'),)),
    Op('exp', 'exp', 'exp', 'Exponential', _UNARY),
    Op('int', 'int', 'int', 'Integer part of the function value', _UNARY),
    Op('log', 'log', 'log', 'Natural logarithm', _UNARY, hyper=True),
    Op('log10', 'log10', 'log10', 'Base-10 logarithm', _UNARY),
    Op('matmul', 'matmul', 'matmul', 'Matrix product of dual matrices and vectors',
       (('u', 'u'), ('u', 'v'), ('v', 'u'))),
    Op('max', 'max', 'max', 'The larger of two numbers', (('u', 'u'), ('u', 'i'), ('u', 'r'), ('r', 'u'))),
    Op('dmax1', 'dmax1', 'dmax1', 'The largest of two to five dual numbers', (('u', 'u'),)),
    Op('maxval', 'maxval', 'maxval', 'The largest element of a dual vector', _UNARY),
    Op('min', 'min', 'min', 'The smaller of two numbers', (('u', 'u'), ('u', 'r'))),
    Op('dmin1', 'dmin1', 'dmin1', 'The smallest of two to four dual numbers', (('u', 'u'),)),
    Op('minval', 'minval', 'minval', 'The smallest element of a dual vector', _UNARY),
    Op('nint', 'nint', 'nint', 'Nearest integer to the function value', _UNARY),
    Op('sign', 'sign', 'sign', 'abs(a) with the sign of b', (('u', 'u'), ('r', 'u'))),
    Op('sin', 'sin', 'sin', 'Sine', _UNARY),
    Op('dsin', 'dsin', 'sin', 'Sine (old Fortran name)', _UNARY),
    Op('tan', 'tan', 'tan', 'Tangent', _UNARY),
    Op('dtan', 'dtan', 'tan', 'Tangent (old Fortran name)', _UNARY),
    Op('sqrt', 'sqrt', 'sqrt', 'Square root', _UNARY, hyper=True),
    Op('sum', 'sum', 'sum', 'Sum of a dual vector', _UNARY),
    Op('maxloc', 'maxloc', 'maxloc', 'Location of the largest element of a dual vector', _UNARY),
]

all_interfaces = [op.key for op in OPS]


class OpInstance:
    ''' An Op for one dual or hyper-dual number: what generic() and the implementation macros read '''

    def __init__(self, op, num, config):
        self.op = op
        self.num = num
        self.config = config

    @property
    def interface(self):
        return self.op.interface

    @property
    def doc(self):
        return self.op.doc

    @property
    def specifics(self):
        return [f'{self.op.family}__' + '_'.join(self.num.sn if t == 'u' else t for t in sig)
                for sig in self.op.signatures]

    # What the implementation macros read
    @property
    def sn(self):
        return self.num.sn

    @property
    def type_name(self):
        return self.num.type_name

    @property
    def hyper(self):
        return self.num.hyper

    @property
    def gdim(self):
        return self.num.gdim

    @property
    def real_kind(self):
        return self.config.real_kind

    @property
    def stringent(self):
        return self.config.stringent

    @property
    def elemental(self):
        return self.config.elemental

    @property
    def purity(self):
        return self.config.purity


def _selected(selection):
    if selection is None:
        return OPS
    unknown = set(selection) - set(all_interfaces)
    if unknown:
        raise KeyError(f'dnad has no interface {", ".join(sorted(unknown))}')
    return [op for op in OPS if op.key in selection]


def _instances(op, kinds, config):
    return [OpInstance(op, num, config) for num in duals(kinds) if op.hyper or not num.hyper]


def interfaces(kinds, config, selection=None):
    ''' One list of OpInstance per selected interface, in catalog order '''
    return [_instances(op, kinds, config) for op in _selected(selection)]


class Family:
    def __init__(self, d, hd):
        self.d = d
        self.hd = hd


def implementations(kinds, config, selection=None):
    ''' family -> Family(d, hd): the OpInstances the family's <family>_d and <family>_hd macros take. A family is
    implemented once even when several selected interfaces share it. '''
    selected = {op.family for op in _selected(selection)}
    out = {}
    for op in OPS:
        if op.family in out:
            continue
        insts = _instances(op, kinds, config) if op.family in selected else []
        out[op.family] = Family([i for i in insts if not i.hyper], [i for i in insts if i.hyper])
    return out


def include_dirs():
    '''Directories to pass to fypp as include paths (-I).'''
    here = os.path.dirname(os.path.abspath(__file__))
    return [here, os.path.join(here, 'overload_implementations')]
