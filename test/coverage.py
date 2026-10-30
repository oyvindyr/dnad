''' Build the generated tests with gfortran --coverage, run them, and report every line of the generated dnad
modules that never ran, by procedure. Exits non-zero unless the tests pass and the coverage is 100%.

    uv run --group dev python test/gen_tests.py
    uv run python test/coverage.py
'''
import glob
import os
import re
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
BUILD = os.path.join(HERE, '..', 'build', 'coverage')
CONFIGS = ['stringent', 'lenient']

DRIVER = '''program coverage
    use test_all_stringent_mod, only: test_all_stringent
    use test_all_lenient_mod, only: test_all_lenient
    implicit none
    logical :: is_ok(2)
    is_ok(1) = test_all_stringent()
    is_ok(2) = test_all_lenient()
    if (.not. all(is_ok)) error stop 1
end program
'''


def run(cmd):
    subprocess.run(cmd, cwd=BUILD, check=True)


def build_and_run():
    shutil.rmtree(BUILD, ignore_errors=True)
    os.makedirs(BUILD)
    with open(os.path.join(BUILD, 'driver.f90'), 'w') as f:
        f.write(DRIVER)
    sources = [os.path.join(HERE, f'{p}_all_{c}_mod.f90') for c in CONFIGS for p in ('dnad', 'test')] + ['driver.f90']
    flags = ['-O0', '--coverage', '-ffree-line-length-none']
    objects = []
    for src in sources:
        obj = os.path.splitext(os.path.basename(src))[0] + '.o'
        run(['gfortran', *flags, '-c', src, '-o', obj])
        objects.append(obj)
    run(['gfortran', '--coverage', *objects, '-o', 'coverage.exe'])
    return subprocess.run([os.path.join(BUILD, 'coverage.exe')], cwd=BUILD).returncode == 0


def procedure_spans(lines):
    spans, name, start = [], None, 0
    for i, line in enumerate(lines, 1):
        m = re.match(r'\s*(?:(?:impure\s+|pure\s+)?elemental\s+|pure\s+)?(?:function|subroutine)\s+(\w+)\(', line)
        if m:
            name, start = m.group(1), i
        elif name and re.match(r'\s*end\s+(function|subroutine)', line):
            spans.append((name, start, i))
            name = None
    return spans


def report(config):
    src = os.path.abspath(os.path.join(HERE, f'dnad_all_{config}_mod.f90'))
    run(['gcov', '-o', BUILD, src])
    gcov = glob.glob(os.path.join(BUILD, f'dnad_all_{config}_mod.f90.gcov'))[0]
    counts = {}
    for line in open(gcov):
        parts = line.split(':', 2)
        if len(parts) < 3 or not parts[1].strip().isdigit():
            continue
        count, lineno = parts[0].strip(), int(parts[1])
        if count != '-' and lineno > 0:
            counts[lineno] = count
    lines = open(src).read().split('\n')
    missed_lines = missed_procs = total_lines = total_procs = 0
    for name, start, end in procedure_spans(lines):
        executable = [n for n in range(start, end + 1) if n in counts]
        missed = [n for n in executable if counts[n].startswith(('#', '='))]
        total_procs += 1
        total_lines += len(executable)
        missed_lines += len(missed)
        if missed:
            entered = len(missed) < len(executable)
            missed_procs += not entered
            print(f'  {name}: ' + ('lines ' + ', '.join(map(str, missed)) if entered else 'never called'))
    print(f'{config}: {total_procs - missed_procs}/{total_procs} procedures, '
          f'{total_lines - missed_lines}/{total_lines} lines covered')
    return missed_lines == 0


if __name__ == '__main__':
    passed = build_and_run()
    covered = all([report(c) for c in CONFIGS])
    if not passed:
        print('tests failed')
    sys.exit(0 if passed and covered else 1)
