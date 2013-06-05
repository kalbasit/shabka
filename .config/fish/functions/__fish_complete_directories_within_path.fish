function __fish_complete_directories_within_path -d "Complete using directories within a path" --argument path comp
  eval "set paths $path/$comp*/"

  for dir in $paths
    printf "%s\t%s\n" (basename $dir) (dirname  $dir)
  end
end
