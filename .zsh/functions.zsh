for func in ${ZSH}/functions/*.zsh; do
    # shellcheck disable=SC1090
    source "${func}"
done
