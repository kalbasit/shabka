" Vim syntax file
" Language:	FLUKA
" Maintainer:	Vasilis.Vlachoudis AT cern.ch
" Last Change:	2003 Oct 7
" URL:
"
" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  set expandtab
  finish
endif

set expandtab
"syn case ignore

" Integers (Error)
syn match flukaNumber	excludenl "\<[-+]\=\d\+\>" containedin=flukaWhat

" Floating point numbers
syn match flukaFloat	excludenl "[-+]\=\d\+\." containedin=flukaWhat
syn match flukaFloat	excludenl "[-+]\=\.\d\+" containedin=flukaWhat
syn match flukaFloat	excludenl "[-+]\=\d\+\.\d\+" containedin=flukaWhat
syn match flukaFloat	excludenl "[-+]\=\d\+[eEdD][-+]\=\d\+" containedin=flukaWhat
syn match flukaFloat	excludenl "[-+]\=\d\+\.[eEdD][-+]\=\d\+" containedin=flukaWhat
syn match flukaFloat	excludenl "[-+]\=\d\+\.\d\+[eEdD][-+]\=\d\+" containedin=flukaWhat
syn match flukaFloat	excludenl "[-+]\=\.\d\+[eEdD][-+]\=\d\+" containedin=flukaWhat

" Identifier
"syn match flukaIdentifier excludenl "\<\i\+\>"

"syn region flukaGeom	start="/^GEOBEGIN\>/1" end="/^GEOEND\>/-1" contains=flukaPrim,flukaPrimAny,flukaEND,flukaOR,flukaComment,flukaWhat

" Considered keywords when used together in a phrase and begin a clause
syn match flukaTitleCard	excludenl "^TITLE\>" containedin=flukaTitle
syn match flukaTitle		"^TITLE\>\s*\_..*$" contains=flukaTitleCard

syn match flukaCard	excludenl "^ACCURACY\>"
syn match flukaCard	excludenl "^ASSIGNMA[ tT]"
syn match flukaCard	excludenl "^BEAM\>"
syn match flukaCard	excludenl "^BEAMPOS\>"
syn match flukaCard	excludenl "^BIASING\>"
syn match flukaCard	excludenl "^COMMENT\>"
syn match flukaCard	excludenl "^COMPOUND\>"
syn match flukaCard	excludenl "^DEFAULTS\>"
syn match flukaCard	excludenl "^DELTARAY\>"
syn match flukaCard	excludenl "^DETECT\>"
syn match flukaCard	excludenl "^DISCARD\>"
syn match flukaCard	excludenl "^DUMPTHEM\>"
syn match flukaCard	excludenl "^ELCFIELD\>"
syn match flukaCard	excludenl "^EMF\>"
syn match flukaCard	excludenl "^HI-PROPE\>"
syn match flukaCard	excludenl "^EMF-BIAS\>"
syn match flukaCard	excludenl "^EMFCUT\>"
syn match flukaCard	excludenl "^EMFFIX\>"
syn match flukaCard	excludenl "^EMFFLUO\>"
syn match flukaCard	excludenl "^EMFRAY\>"
syn match flukaCard	excludenl "^EVENTBIN\>"
syn match flukaCard	excludenl "^EVENTDAT\>"
syn match flukaCard	excludenl "^EVENTYPE\>"
syn match flukaCard	excludenl "^EVXTEST\>"
syn match flukaCard	excludenl "^EXPTRANS\>"
syn match flukaCard	excludenl "^EXTRAWEI\>"
syn match flukaCard	excludenl "^FLUKAFIX\>"
syn match flukaCard	excludenl "^FREE\>"
syn match flukaCard	excludenl "^GEOBEGIN\>"
syn match flukaCard	excludenl "^GEOEND\>"
syn match flukaCard	excludenl "^GLOBAL\>"
syn match flukaCard	excludenl "^LAM-BIAS\>"
syn match flukaCard	excludenl "^LANDAU\>"
syn match flukaCard	excludenl "^LOW-BIAS\>"
syn match flukaCard	excludenl "^LOW-DOWN\>"
syn match flukaCard	excludenl "^LOW-MAT\>"
syn match flukaCard	excludenl "^LOW-NEUT\>"
syn match flukaCard	excludenl "^MATERIAL\>"
syn match flukaCard	excludenl "^MAT-PROP\>"
syn match flukaCard	excludenl "^MGNFIELD\>"
syn match flukaCard	excludenl "^MULSOPT\>"
syn match flukaCard	excludenl "^MUPHOTON\>"
syn match flukaCard	excludenl "^OPEN\>"
syn match flukaCard	excludenl "^OPT-PROD\>"
syn match flukaCard	excludenl "^OPT-PROP\>"
syn match flukaCard	excludenl "^PAIRBREM\>"
syn match flukaCard	excludenl "^PART-THR\>"
syn match flukaCard	excludenl "^PHOTONUC\>"
syn match flukaCard	excludenl "^PHYSICS\>"
syn match flukaCard	excludenl "^PLOTGEOM\>"
syn match flukaCard	excludenl "^POLARIZA\>"
syn match flukaCard	excludenl "^RANDOMIZ[ eE]"
syn match flukaCard	excludenl "^RESNUCLE[ iI]"
syn match flukaCard	excludenl "^ROT-DEFI[ nN][ iI]"
syn match flukaCard	excludenl "^ROTPRBIN\>"
syn match flukaCard	excludenl "^SCORE\>"
syn match flukaCard	excludenl "^SOURCE\>"
syn match flukaCard	excludenl "^START\>"
syn match flukaCard	excludenl "^STEPSIZE\>"
syn match flukaCard	excludenl "^STERNHEI[ mM][ eE]"
syn match flukaCard	excludenl "^TCQUENCH\>"
syn match flukaCard	excludenl "^THRESHOL[ dD]"
syn match flukaCard	excludenl "^TIME-CUT\>"
syn match flukaCard	excludenl "^USRBDX\>"
syn match flukaCard	excludenl "^USRBIN\>"
syn match flukaCard	excludenl "^USRCOLL\>"
syn match flukaCard	excludenl "^USRICALL\>"
syn match flukaCard	excludenl "^USROCALL\>"
syn match flukaCard	excludenl "^USRTRACK\>"
syn match flukaCard	excludenl "^USRYIELD\>"
syn match flukaCard	excludenl "^WW-FACTO[ rR]"
syn match flukaCard	excludenl "^WW-PROFIl[ eE]"
syn match flukaCard	excludenl "^WW-THRES[ hH]"

syn match flukaStopCard	excludenl "^STOP\>"

" Comments
syn match flukaComment		excludenl "^\*.*$"

syn keyword flukaTodo contained	TODO FIXME XXX

syn match flukaWhat1	excludenl "\%11v.\{0,10}" contains=flukaFloat,flukaNumber
syn match flukaWhat2	excludenl "\%21v.\{0,10}" contains=flukaFloat,flukaNumber
syn match flukaWhat3	excludenl "\%31v.\{0,10}" contains=flukaFloat,flukaNumber
syn match flukaWhat4	excludenl "\%41v.\{0,10}" contains=flukaFloat,flukaNumber
syn match flukaWhat5	excludenl "\%51v.\{0,10}" contains=flukaFloat,flukaNumber
syn match flukaWhat6	excludenl "\%61v.\{0,10}" contains=flukaFloat,flukaNumber

syn match flukaSDUM	excludenl ".\%>71v" contains=CONTAINED,flukaGeom,flukaComment
"syn match flukaWhatLine excludenl ".\%>11v" contains=flukaWhat,flukaSDUM

syn match flukaPrim	excludenl "^  ARB " "contained
syn match flukaPrim	excludenl "^  BOX " "contained
syn match flukaPrim	excludenl "^  ELL " "contained
syn match flukaPrim	excludenl "^  PLA " "contained
syn match flukaPrim	excludenl "^  RAW " "contained
syn match flukaPrim	excludenl "^  RCC " "contained
syn match flukaPrim	excludenl "^  REC " "contained
syn match flukaPrim	excludenl "^  RPP " "contained
syn match flukaPrim	excludenl "^  SPH " "contained
syn match flukaPrim	excludenl "^  TRC " "contained
syn match flukaPrim	excludenl "^  WED " "contained
syn match flukaPrim	excludenl "^  XCC " "contained
syn match flukaPrim	excludenl "^  XEC " "contained
syn match flukaPrim	excludenl "^  XYP " "contained
syn match flukaPrim	excludenl "^  XZP " "contained
syn match flukaPrim	excludenl "^  YCC " "contained
syn match flukaPrim	excludenl "^  YEC " "contained
syn match flukaPrim	excludenl "^  YZP " "contained
syn match flukaPrim	excludenl "^  ZCC " "contained
syn match flukaPrim	excludenl "^  ZEC " "contained
syn match flukaPrimAny	excludenl "^  ... " "contained

syn match flukaEnd      excludenl "^  END" "contained

syn match flukaOR	excludenl "\%11vOR" "contained
syn match flukaOR	excludenl "\%18vOR" "contained
syn match flukaOR	excludenl "\%25vOR" "contained
syn match flukaOR	excludenl "\%32vOR" "contained
syn match flukaOR	excludenl "\%39vOR" "contained
syn match flukaOR	excludenl "\%46vOR" "contained
syn match flukaOR	excludenl "\%53vOR" "contained
syn match flukaOR	excludenl "\%60vOR" "contained
syn match flukaOR	excludenl "\%67vOR" "contained

"syn region flukaGeom	start="^GEOBEGIN\>" end="^GEOEND\>" contains=flukaPrim,flukaPrimAny,flukaEND,flukaOR,flukaComment,flukaWhat

syn match flukaTab		"\t"
syn match flukaEmptyLine	"^$"
syn match flukaEmptyLine	"^\s*$"

syn match flukaGeoTitle		"^GEOBEGIN\>.*\_..*$" contains=flukaCard,flukaNumber,flukaFloat,flukaSDUM,flukaTab

" The default highlighting.
hi def link flukaIdentifier	Normal
hi def link flukaTitle		Title
hi def link flukaTitleCard	Statement
hi def link flukaGeoTitle	String
hi def link flukaCard		Statement
hi def link flukaPrim		Function
hi def link flukaPrimAny	Function
hi def link flukaComment	Comment
hi def link flukaEnd		Special
hi def link flukaStopCard	Special
hi def link flukaOR		Type
hi def link flukaSDUM		Type
"hi flukaFloat	guifg=#000000
"hi def link flukaIdentifier	Identifier

hi def link flukaTab		Error
hi def link flukaEmptyLine	Error
hi def link flukaNumber		WarningMsg

hi def link flukaFloat		Normal

"hi flukaWhat1 guibg=#ffffff
"hi flukaWhat2 guibg=#f0f0f0
"hi flukaWhat3 guibg=#ffffff
"hi flukaWhat4 guibg=#f0f0f0
"hi flukaWhat5 guibg=#ffffff
"hi flukaWhat6 guibg=#f0f0f0

"hi def link flukaFloatNoDec	Float
"hi def link flukaFloatIniDec	Float
"hi def link flukaFloatEndDec	Float
"hi def link flukaFloatDExp	Float

let b:current_syntax = "fluka"
