#
# Utilities for working with pdfs
#
# TODO: might want to include utils from skid, e.g. `make-searchable-with-ocr`

# convert {ppt, odf} to pdf
alias to-pdf='libreoffice --headless --invisible --convert-to pdf'

# convert djvu to pdf
function djvu2pdf {
    ddjvu -format=pdf -quality=85 -verbose "$1" "$1.pdf"
}

# concatenate pdfs
function pdf-concat {
    out='output.pdf'
    gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE="$out" -dBATCH "$@"
    echo "wrote to $out"
}


# pdf-extract-pages -- this function uses 3 arguments:
#   $1 is the first page of the range to extract
#   $2 is the last page of the range to extract
#   $3 is the input file
#   output file will be named "inputfile_pXX-pYY.pdf"
function pdf-extract-pages {
    gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
       -dFirstPage=${1} \
       -dLastPage=${2} \
       -sOutputFile=${3%.pdf}_p${1}-p${2}.pdf \
       ${3}
}
