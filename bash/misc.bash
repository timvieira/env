#
# Miscellaneous utilties. Many of which are wrappers around other programs to
# make common tasks easier.
#

export PATH="/home/timv/projects/latex2unicode/bin:$PATH"

function graphviz {
    out=$1.svg
    green output file: $out
    cat $1 |dot -Tsvg > $out
    shutup-and-disown "google-chrome $out"
}

alias gnome-do-restart='(pkill9 gnome-do && shutup-and-disown gnome-do) >& /dev/null'

function o {
    # gnome-open; xdg-open    # unity equivalent of gnome-open
    xdg-open "$@" 2>/dev/null >/dev/null
}

alias tetris="shutup-and-disown google-chrome $ENV/tetris.html 2>/dev/null"

function pkill9 {
    # Skip the grep command
    ps aux |grep -v grep |grep "$@"
    pids=`ps aux |grep -v grep |grep "$@" |linepy 'ids=[]' 'ids.append(split[1])' 'print(" ".join(ids))'`
    echo "$pids"
    kill -9 $pids
}

alias poweroff-display='sleep 1 && xset dpms force off'

# Remap: capslock -> control
function remap-ctrl-key {
    setxkbmap -layout us -option ctrl:nocaps
}

# https://linuxconfig.org/how-to-disable-middle-mouse-button-click-paste-on-ubuntu-20-04-focal-fossa-linux
function disable-middle-click-paste {
    gsettings set org.gnome.desktop.interface gtk-enable-primary-paste false
}
function enable-middle-click-paste {
    gsettings set org.gnome.desktop.interface gtk-enable-primary-paste true
}

function disable-touchpad {
    xinput list \
        |grep -i touchpad \
        |linepy '
if line:
    [x] = re.findall("id=(\d+)", line)
    os.system("xinput set-prop %s \"Device Enabled\" 0" % x)
else:
    print("device not found")
'
}

function enable-touchpad {
    xinput list \
        |grep -i touchpad \
        |linepy '
if line:
    [x] = re.findall("id=(\d+)", line)
    os.system("xinput set-prop %s \"Device Enabled\" 1" % x)
else:
    print("device not found")
'
}

function compare-lines {
    hs=""
    for f in `echo $@`; do
        h=/tmp/`pyhash $f`
        hs="$hs $h"
        cat $f |sort > $h   # timv: option for uniq (i.e. set difference)?
    done
    meld $hs
}

function compare-uniq-lines {
    hs=""
    for f in `echo $@`; do
        h=/tmp/`pyhash $f`
        hs="$hs $h"
        cat $f |sort |uniq > $h
    done
    meld $hs
}

alias pyhash="python -c 'import sys, hashlib; print(hashlib.sha1(str().join(sys.argv[1:]) or raw_input()).hexdigest())'"

# re-execute a command periodically, in an infinite loop
function ghetto-refresh {
    if [[ "$#" -ne "2" ]]; then
        echo "ghetto-refresh <rate> <cmd>"
        return
    fi
    while [ 1 ]; do
        $2
        sleep $1
        clear
    done
}


# kill a process after a number of seconds
# usage: doalarm <seconds to wait> program arg arg ...
function doalarm { perl -e 'alarm shift; exec @ARGV' "$@"; }


# quickly search the first page of multiple pdfs
function ack-pdf-recursive {
    ack-pdfs "$1" `find -name '*.pdf'`
}

function ack-pdfs {
    pdfs=`echo "${@:2}"`
    for f in `echo $pdfs`; do
        # extract text and search only the first page (for efficiency)
        out=$((pdftotext -l 1 "$f" - |ack "$@") 2>/dev/null)
        if [[ $? -eq 0 ]]; then
            green "$f"
            echo "$out"
            echo
        fi
    done
}


function pdf-to-svg {
    if [[ "$#" -ne "1" ]]; then
        echo "pdf-to-svg <pdf>"
        return
    fi
    inkscape --without-gui --file="$1" --export-plain-svg="$1.svg"
}

function pdf-to-png {
    if [[ "$#" -ne "1" ]]; then
        echo "pdf-to-png <pdf>"
        return
    fi
    gs -dSAFER -dBATCH -dNOPAUSE -sDEVICE=png256 -r300 -dTextAlphaBits=4 -o "$1_%04d.png" -f "$1"
}


function meld-nocolor {
    if [[ "$#" -ne "2" ]]; then
        echo "Error: $0 expected two files as input."
        return
    fi

    local A=/tmp/`basename "$1"`
    local B=/tmp/`basename "$2"`
    cat "$1" | nocolor > "$A"
    cat "$2" | nocolor > "$B"

    echo "writing nocolor versions of each file to temp files"
    echo "$1 -> $A"
    echo "$2 -> $B"

    meld "$A" "$B"
}

function unicode-lookup {
    ack -i "$@" ~/projects/env/bin/unicode-lookup/unicode.db.txt
}
