with import ../util;

[] ++ (map (path: import path) (recImport ./.))
