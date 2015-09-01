yasnippet-numpy-style
=====================

yasnippet snippets for docstring in [numpy coding style](https://github.com/numpy/numpy/blob/master/doc/HOWTO_DOCUMENT.rst.txt).

#Installation
At first, install [yasnippet](https://github.com/capitaomorte/yasnippet).

Then:
```shell
$ git clone https://github.com/marubu/yasnippet-numpy-style.git /tmp/python-mode
$ cp /tmp/python-mode/* ~/.emacs.d/snippets/python-mode/
```

#Demo
##Parse arguments
Demonstration of snippet `parameters` is shown below.
* This snippet parses dummy argument part. (supports `class` and `def`)

![alt text](images/snippet_parse.gif)

##Successive expansion
It is possible to combine the above snippet with snippet `defwithdoc` which is for function with docstring.  
To use this, you need to write following emacs lisp in `~/.emacs.d/init.el`.
```
(setq yas-triggers-in-field t)
```
Demonstration of successive expansion is shown below.
* Expand snippet by only tab key.

![alt text](images/snippet_successive.gif)
