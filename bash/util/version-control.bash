# todo: use locate instead.
function find-repos {
    locate "*/.hg"
    locate "*/.git"
}

# try to find repositories which have changes which might need to be pushed
#function hg-changed-repos {
#    cd
#    repos=`find -name '.hg' -type d -exec dirname {} \;`
#    for line in $repos; do
#        cd $line
#        echo -n "$line -- "
#        MODIFIED=$(hg st -m)
#        if [ "$MODIFIED" != "" ]; then
#            red modified
#        else
#            outgoing=$(doalarm 3 hg outgoing |grep "no changes found")
#            if [[ $outgoing != "no changes found" ]]; then
#                cyan outgoing
#            else
#                yellow ok
#            fi
#        fi
#        cd ~
#    done
#}

# more on log formatting http://hgbook.red-bean.com/read/customizing-the-output-of-mercurial.html
#alias hgtree="hg log --template '{rev} {node|short} {author|user}: {desc} ({date|age})\n'"
alias hgchangelog="hg log --style changelog"
alias hgserve="o http://localhost:8000 && hg serve"   # serve and open
#alias hg-dummy-ci='hg ci -m "()"'
#alias hg-dummy-push='hg ci -m "()" && hg push'

# run pop open kdiff3 and open editor
function hg-diff-ci {
    for f in $(hg st -m -n $(hg root)); do   # use relative paths
        echo $f
        hg kdiff3 $f 2>/dev/null &
        hg ci $f
    done
}


alias gittree='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %s %cr"'
alias gittree-who='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %cn %s %cr"'
alias gittree-when='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %cn %s %ci"'
##
#function get_git_branch {
#    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\ \[\1\]/'
#}
#PS1="\h:\W \u\[\033[0;32m\]\$(get_git_branch) \[\033[0m\]\$ "
##
