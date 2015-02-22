post_init_hook=()
pre_init_hook=()

[[ -f ${ZSH}/local/local.zsh ]] && source ${ZSH}/local/local.zsh

for f in $pre_init_hook; do
    $f
done

source ${ZSH}/body.zsh

for f in $post_init_hook; do
    $f
done
