function rbenv_shell
  set -l vers $argv[1]

  switch "$vers"
    case '--complete'
      echo '--unset'
      echo 'system'
      exec rbenv-versions --bare
      return
    case '--unset'
      set -e RBENV_VERSION
      return 1
    case ''
      if [ -z "$RBENV_VERSION" ]
        echo "rbenv: no shell-specific version configured" >&2
        return 1
      else
        echo "$RBENV_VERSION"
        return
      end
    case '*'
      rbenv prefix "$vers" > /dev/null
      set -g -x RBENV_VERSION "$vers"
  end
end
