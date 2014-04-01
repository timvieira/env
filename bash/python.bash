#
# Configuration specific to python
#

# ack thru all the python code on my hard drive
function pysearch {
    locate -0 '*.py' |xargs -0 ack --color --group "$@"
}

# Show pythonpath
alias pypath="python -c 'import sys; print sys.path' | tr ',' '\n' | grep -v 'egg'"

# cd to the directory containing specified python module
function cdpy {
    cd `python -c "import os; import $1; print os.path.dirname($1.__file__)"`
}

# edit python module by name
function vpy {
    python -m arsenal.debug.edit "$@"
}


function prof-callgraph {

  name="/tmp/prof-callgraph"
  python -m cProfile -o "$name.pstats" "$@"
  gprof2dot.py -f pstats "$name.pstats" | dot -Tsvg -o "$name.svg"
  shutup-and-disown google-chrome "$name.svg"

  echo
  yellow "how to read results"
  yellow "==================="
  echo "
+----------------------------------+
|   function name : module name    |
| total time including sub-calls % |  total time including sub-calls %
|    (self execution time %)       |------------------------------------>
|  total number of self calls      |
+----------------------------------+
"

}
