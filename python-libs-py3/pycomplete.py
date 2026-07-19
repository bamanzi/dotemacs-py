
"""
Python dot expression completion using Pymacs.

This almost certainly needs work, but if you add

    (require 'pycomplete)

to your .xemacs/init.el file (untried w/ GNU Emacs so far) and have Pymacs
installed, when you hit M-TAB it will try to complete the dot expression
before point.  For example, given this import at the top of the file:

    import time

typing "time.cl" then hitting M-TAB should complete "time.clock".

This is unlikely to be done the way Emacs completion ought to be done, but
it's a start.  Perhaps someone with more Emacs mojo can take this stuff and
do it right.

See pycomplete.el for the Emacs Lisp side of things.
"""

# Author:     Skip Montanaro
# Maintainer: skip@pobox.com
# Created:    Oct 2004
# Keywords:   python pymacs emacs

# This software is provided as-is, without express or implied warranty.
# Permission to use, copy, modify, distribute or sell this software, without
# fee, for any purpose and by any individual or organization, is hereby
# granted, provided that the above copyright notice and this paragraph
# appear in all copies.

# Along with pycomplete.el this file allows programmers to complete Python
# symbols within the current buffer.

import sys
import os
import os.path

try:
    x = set
except NameError:
    from sets import Set as set
else:
    del x

def get_all_completions(s, imports=None):
    """Return contextual completion of s (string of >= zero chars).

    If given, imports is a list of import statements to be executed first.
    """
    if type(s)==type(b''):
        s = s.decode('utf-8')
    
    locald = {}
    if imports is not None:
        for stmt in imports:
            try:
                exec(stmt.strip(), globals(), locald)
            except TypeError:
                raise TypeError("invalid type: %s" % stmt)
            except Exception:
                continue

    dots = s.split(".")
    if not s or len(dots) == 1:
        keys = set()
        keys.update(locald.keys())
        keys.update(globals().keys())
        try:
            import builtins     # python 3.x
        except:
            import __builtin__ as buitins
        keys.update(dir(builtins))
        keys = list(keys)
        keys.sort()
        if s:
            return [k for k in keys if k.startswith(s)]
        else:
            return keys

    sym = None
    for i in range(1, len(dots)):
        s = ".".join(dots[:i])
        if not s:
            continue
        try:
            sym = eval(s, globals(), locald)
        except NameError:
            try:
                sym = __import__(s, globals(), locald, [])
            except ImportError:
                return []
            except AttributeError:
                try:
                    sym = __import__(s, globals(), locald, [])
                except ImportError:
                    pass            
    if sym is not None:
        s = dots[-1]
        return [k for k in dir(sym) if k.startswith(s)]

def get_all_completions_for_ac(s, imports=None):
    """Return contextual completion of s (string of >= zero chars).
    If given, imports is a list of import statements to be executed first.

    Different from `get_all_completions', this one would return ["os.path.isabs",
    "os.path.isfile", "os.path.isdir", "os.path.islink"] for `os.path.is", while
    the original would return ["isab", "isfile", "isdir", "islink"]. This behaviour
    is assumed to be compliant with `auto-complete' rather than `complete-symbol'
    
    """
    if type(s)==type(b''):
        s = s.decode('utf-8')
    
    completions = get_all_completions(s, imports)
    dots = s.split(".")
    prefix = ".".join(dots[0:-1])
    return [ (prefix + "." + k) for k in completions] #modified by EEPY
    
    
def pycomplete(s, imports=None, cwd=None):
    if type(s)==type(b''):
        s = s.decode('utf-8')
    if not s:
        return ''
    if cwd and os.path.isdir(cwd):
        os.chdir(cwd)
    completions = get_all_completions(s, imports)
    dots = s.split(".")
    result = os.path.commonprefix([k[len(dots[-1]):] for k in completions])
    if ""==result:
        return completions
    else:
        return [result,]

def exec_lines(lines):
    """Exec LINES in pymacs' global namespace, so that later completion would have better inspection.
    """
    if type(lines)==type(b''):
        lines = lines.decode('utf-8')
    
    if lines.startswith(" "):
        lines = 'if True:\n' + lines
    exec(lines, globals())
    

if __name__ == "__main__":
    print("<empty> ->", pycomplete(""))
    print("sys.get ->", pycomplete("sys.get"))
    print("sy ->", pycomplete("sy"))    
    print("sy (sys in context) ->", pycomplete("sy", imports=["import sys"]))
    print("foo. ->", pycomplete("foo."))
    
    print("Enc (email * imported) ->", end='')
    print(pycomplete("Enc", imports=["from email import *"]))

    print("E (email * imported) ->", end='')
    print(pycomplete("E", imports=["from email import *"]))

    print("Enc ->", pycomplete("Enc"))
    print("E ->", pycomplete("E"))

import inspect
from io import StringIO

from Pymacs import lisp
sys.path.append('.')


def pyhelp(s, imports=None, cwd=None):
    """Return object description"""    
    if not s:
        return ''
    if type(s)==type(b''):
        s = s.decode('utf-8')
    
    if cwd and os.path.isdir(cwd):
        os.chdir(cwd)
    _import_modules(imports, globals(), None)
    return _getdoc(s)

def pysignature(s):
    """Return info about function parameters"""
    if type(s)==type(b''):
        s = s.decode('utf-8')
    
    f = None
    try:
        f = eval(s)
    except Exception as ex:
        return "%s" % ex
    if inspect.ismethod(f):
        f = f.__func__
    if not inspect.isroutine(f):
        return ''
    doc = inspect.getdoc(f)
    if doc:
        doc = doc.splitlines()[0]
    if inspect.isfunction(f):
        (args, varargs, varkw, defaults) = inspect.getargspec(f)
        spec = inspect.formatargspec(args,varargs,varkw,defaults)
        return ('%s %s: %s' % (f.__name__, spec, doc))
    else:
        return ('%s: %s' % (f.__name__, doc))

def _getdoc(s):
    """Return string printed by `help` function"""
    obj = None
    try:
        obj = eval(s)
    except Exception as ex:
        return "%s" % ex
    out = StringIO()
    old = sys.stdout
    sys.stdout = out
    help(obj)
    sys.stdout = old
    return out.getvalue()

def _import_modules(imports, dglobals, dlocals):
    """If given, execute import statements"""
    if imports is not None:
        for stmt in imports:
            try:
                exec(stmt.strip(), dglobals, dlocals)
            except TypeError:
                raise TypeError('invalid type: %s' % stmt)
            except:
                continue

# Local Variables :
# pymacs-auto-reload : t
# End :
