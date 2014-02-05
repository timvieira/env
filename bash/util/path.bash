# prepend to path environment variable
function add-path {
    for d in `echo $@`; do
        export PATH=$d:$PATH
    done
}
function add-pypath {
    for d in `echo $@`; do
        export PYTHONPATH=$d:$PYTHONPATH
    done
}
function add-classpath {
    for d in `echo $@`; do
        export CLASSPATH=$d:$CLASSPATH
    done
}

