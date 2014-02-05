# Audio Conversion utilities

function m4a2wav {
    for i in *.m4a; do
        mplayer -ao pcm "$i" -ao pcm:file="${i%.m4a}.wav"
    done
}

function wav2mp3 {
    for i in *.wav; do
        lame -h -b 192 "$i" "${i%.wav}.mp3"
    done
}

function m4a2mp3 {
    m4a2wav
    wav2mp3
    #rm *.wav
    echo "There are probably some temporary wav files you can delete."
    echo "Currenly, m4a2mp3 will *not* delete these for you."
}
