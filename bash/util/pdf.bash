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
function concat-pdfs {
    out='output.pdf'
    gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE="$out" -dBATCH "$@"
    echo "wrote to $out"
}
