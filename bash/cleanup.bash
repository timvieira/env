function tmpfiles {
    find $1 -name '*~'
}

function pyclean {
    rm -f `find . -name "*.pyc"`
    rm -f `find . -name "*$py.class"`  # jython
}

# TODO: remove executables associate with file... e.g. hw4.hs has {hw4.hi, hw4.0, hw4}
function haskell-clean {
    find . -name '*.hi' -exec rm -f {} \;
    find . -name '*.o'  -exec rm -f {} \;
}

# remove org-mode's LaTeX output files
function org-clean {
    org-export-files | xargs -0 rm -f
}

function org-export-files {
    find . -name '*.pdf' -print0 |xargs -0 ack --print0 --files-with-matches 'Creator\(Emacs Org-mode version'
    find . -name '*.tex' -print0 |xargs -0 ack --print0 --files-with-matches 'pdfcreator=\{Emacs Org-mode version'
}

# clean up tex derived files
# TODO: do we want this to be recursive? I suppose tex projects rarely are?
function tex-clean {
    rm -f "*.log" "*.aux" "*.blg" "*.bbl" "*.dvi"
}

# clean up derived files.
function clean {
    tex-clean
    org-clean
    pyclean
    haskell-clean
}