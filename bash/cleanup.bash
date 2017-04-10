# Recursively find files in cwd that match an extension, then remove the
# extension.
function find-ext-rm-ext {
    for f in `find . -name '*.'$1`; do
        echo $f |sed "s/\.$1\$//";
    done
}

# Remove extensions based on the existence of a root file, e.g., tex => pdf log.
function remove-exts-associated-with-ext {
    base=$1
    shift
    rest="$@"
    for f in `find-ext-rm-ext $base`; do
        for g in `echo $rest`; do
            rm -f $f.$g
        done
    done
}

function tmpfiles {
    find $1 -name '*~'
}

function pyclean {
    rm -f `find . -name "*.pyc"`
    rm -f `find . -name "*$py.class"`  # jython
}

# TODO: remove executables associate with file... e.g. hw4.hs has {hw4.hi, hw4.0, hw4}
#function haskell-clean {
#    find . -name '*.hi' -exec rm -f {} \;
#    find . -name '*.o'  -exec rm -f {} \;
#}

# remove org-mode's LaTeX output files
function org-clean {
    remove-exts-associated-with-ext org tex $TEX_CRUFT
}

# clean up tex derived files
TEX_CRUFT="log aux blg bbl dvi fls fdb_latexmk out"
function tex-clean {
    remove-exts-associated-with-ext tex $TEX_CRUFT
    rm -f pdflatex*.fls
}

function cython-clean {
    remove-exts-associated-with-ext pyx html cpp c so
    remove-exts-associated-with-ext pxy html cpp c so
}

# clean up derived files.
function clean {
    tex-clean
    org-clean
    pyclean
    cython-clean
}
