# ssh aliases
#alias gargamel='ssh tvieira2@gargamel.cs.uiuc.edu'
#alias smeagol='ssh tvieira2@smeagol.cs.uiuc.edu'
#alias jasper='ssh timv@jasper.cs.umass.edu'
#alias vinci8='ssh timv@vinci8.cs.umass.edu'
#alias dali='ssh timv@dalisrv.cs.umass.edu'
#alias loki='ssh timv@loki.cs.umass.edu'
alias ugradx='ssh timv@ugradx.cs.jhu.edu'
alias clsp='ssh -X timv@login.clsp.jhu.edu'
alias coe='ssh -X tvieira@test1.hltcoe.jhu.edu'


# Add ssh public key to remote machine
function push-public-key {
    if [[ "$#" -ne "1" ]]; then
        echo -e "usage: push-public-key <user@dest>"
        return
    fi
    publickey=`cat ~/.ssh/id_rsa.pub`
    # make sure you set the appropriate permissions!
    ssh "$1" "mkdir -p ~/.ssh/ \
              && touch .ssh/authorized_keys \
              && chmod 600 .ssh/authorized_keys \
              && echo $publickey >> .ssh/authorized_keys \
              && cat .ssh/authorized_keys"
}
