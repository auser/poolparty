let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <S-Tab> 
inoremap <C-Tab> 	
imap <S-Del> <Plug>Jumper
imap <silent> <Plug>IMAP_JumpBack =IMAP_Jumpfunc('b', 0)
imap <silent> <Plug>IMAP_JumpForward =IMAP_Jumpfunc('', 0)
map! <D-v> *
vmap <NL> <Plug>IMAP_JumpForward
nmap <NL> <Plug>IMAP_JumpForward
vmap <silent> ,cy "0ygv,cc
nmap <silent> ,cy "0Y,cc
map ,n :execute 'NERDTreeToggle ' . getcwd()
vnoremap ,,, :call TagSelection() 
nmap ,,, viw,,,
map ,t :Etabs 
vmap ,r !rtranslate -f fr -t en %
vmap ,y y:call system("pbcopy", getreg("\""))
map ,gf gf
map ,da :r !date +\%F
noremap / :call SearchCompleteStart()/
map E el 
xmap S <Plug>VSurround
vmap [% [%m'gv``
nmap \sv <Plug>SVNVimDiff
nmap \su <Plug>SVNUpdate
nmap \sr <Plug>SVNResolved
nmap \sp <Plug>SVNPropedit
nmap \si <Plug>SVNInfo
nmap \ss <Plug>SVNStatus
nmap \sw <Plug>SVNReview
nmap \sq <Plug>SVNRevert
nmap \sl <Plug>SVNLog
nmap \sg <Plug>SVNGotoOriginal
nmap \sd <Plug>SVNDiff
nmap \sc <Plug>SVNCommit
nmap \sG <Plug>SVNClearAndGotoOriginal
nmap \sn <Plug>SVNAnnotate
nmap \sa <Plug>SVNAdd
map <silent> \mm :ShowMarksPlaceMark
map <silent> \ma :ShowMarksClearAll
map <silent> \mh :ShowMarksClearMark
map <silent> \mo :ShowMarksOn
map <silent> \mt :ShowMarksToggle
nmap <silent> \ups :call Perl_Handle()
nmap <silent> \lps :call Perl_Handle()
nmap \so <Plug>DBOrientationToggle
nmap \sh <Plug>DBHistory
nmap \slv <Plug>DBListView
nmap \slp <Plug>DBListProcedure
nmap \slt <Plug>DBListTable
vmap <silent> \slc :exec 'DBListColumn '.DB_getVisualBlock()
nmap \slc <Plug>DBListColumn
nmap \sbp <Plug>DBPromptForBufferParameters
nmap \sdpa <Plug>DBDescribeProcedureAskName
vmap <silent> \sdp :exec 'DBDescribeProcedure '.DB_getVisualBlock()
nmap \sdp <Plug>DBDescribeProcedure
nmap \sdta <Plug>DBDescribeTableAskName
vmap <silent> \sdt :exec 'DBDescribeTable '.DB_getVisualBlock()
nmap \sdt <Plug>DBDescribeTable
vmap <silent> \sT :exec 'DBSelectFromTableTopX '.DB_getVisualBlock()
nmap \sT <Plug>DBSelectFromTableTopX
nmap \sta <Plug>DBSelectFromTableAskName
nmap \stw <Plug>DBSelectFromTableWithWhere
vmap <silent> \st :exec 'DBSelectFromTable '.DB_getVisualBlock()
nmap \st <Plug>DBSelectFromTable
nmap <silent> \sel :.,.DBExecRangeSQL
nmap <silent> \sea :1,$DBExecRangeSQL
nmap \sE <Plug>DBExecSQLUnderCursorTopX
nmap \se <Plug>DBExecSQLUnderCursor
vmap \sE <Plug>DBExecVisualSQLTopX
vmap \se <Plug>DBExecVisualSQL
map \rwp <Plug>RestoreWinPosn
map \swp <Plug>SaveWinPosn
vmap <silent> \Htd :<BS><BS><BS>ma'>\Htd
vmap <silent> \tt :<BS><BS><BS>ma'>\tt
vmap <silent> \tp@ :<BS><BS><BS>ma'>\tp@
vmap <silent> \tsq :<BS><BS><BS>ma'>\tsq
vmap <silent> \tsp :<BS><BS><BS>ma'>\tsp
vmap <silent> \tml :<BS><BS><BS>ma'>\tml
vmap <silent> \tab :<BS><BS><BS>ma'>\tab
vmap <silent> \t@ :<BS><BS><BS>ma'>\t@
vmap <silent> \t? :<BS><BS><BS>ma'>\t?
vmap <silent> \t= :<BS><BS><BS>ma'>\t=
vmap <silent> \t< :<BS><BS><BS>ma'>\t<
vmap <silent> \t; :<BS><BS><BS>ma'>\t;
vmap <silent> \t: :<BS><BS><BS>ma'>\t:
vmap <silent> \ts, :<BS><BS><BS>ma'>\ts,
vmap <silent> \t, :<BS><BS><BS>ma'>\t,
vmap <silent> \t| :<BS><BS><BS>ma'>\t|
vmap <silent> \anum :B s/\(\d\)\s\+\(-\=[.,]\=\d\)/\1@\2/ge:AlignCtrl mp0P0gv:Align [.,@]:'<,'>s/\([-0-9.,]*\)\(\s\+\)\([.,]\)/\2\1\3/ge:'<,'>s/@/ /ge
vmap <silent> \afnc :<BS><BS><BS>ma'>\afnc
vmap <silent> \adef :<BS><BS><BS>ma'>\adef
vmap <silent> \adec :<BS><BS><BS>ma'>\adec
vmap <silent> \ascom :<BS><BS><BS>ma'>\ascom
vmap <silent> \aocom :<BS><BS><BS>ma'>\aocom
vmap <silent> \acom :<BS><BS><BS>ma'>\acom
vmap <silent> \abox :<BS><BS><BS>ma'>\abox
vmap <silent> \a= :<BS><BS><BS>ma'>\a=
vmap <silent> \a< :<BS><BS><BS>ma'>\a<
vmap <silent> \a, :<BS><BS><BS>ma'>\a,
vmap <silent> \a? :<BS><BS><BS>ma'>\a?
vmap <silent> \Tsp :<BS><BS><BS>ma'>\Tsp
vmap <silent> \T@ :<BS><BS><BS>ma'>\T@
vmap <silent> \T= :<BS><BS><BS>ma'>\T=
vmap <silent> \T< :<BS><BS><BS>ma'>\T<
vmap <silent> \T: :<BS><BS><BS>ma'>\T:
vmap <silent> \Ts, :<BS><BS><BS>ma'>\Ts,
vmap <silent> \T, :<BS><BS><BS>ma'>\T,
vmap <silent> \T| :<BS><BS><BS>ma'>\T|
map <silent> \tdW@ :AlignCtrl v ^\s*/[/*]:AlignCtrl mWp1P1=l @:'a,.Align
map <silent> \tW@ :AlignCtrl mWp1P1=l @:'a,.Align
nmap <silent> \t@ :AlignCtrl mIp1P1=l @:'a,.Align
omap <silent> \t@ :AlignCtrl mIp1P1=l @:'a,.Align
nmap <silent> \aocom :AlignPush:AlignCtrl g /[*/]\acom:AlignPop
omap <silent> \aocom :AlignPush:AlignCtrl g /[*/]\acom:AlignPop
vnoremap \kw :s/ \+$//
nnoremap \kw :1,$s/ \+$//
map \t :FuzzyFinderTextMate
map \ra <Plug>RailsAlternate
vmap ]% ]%m'gv``
vmap a% [%v]%
nmap cs <Plug>Csurround
nmap ds <Plug>Dsurround
nmap gx <Plug>NetrwBrowseX
map gl gt
map gh gT
map gf 
map rl :set paste:r !pbpaste:set nopaste
xmap s <Plug>Vsurround
map td :tabclose 
map tn :tabnew 
map th :tabprev 
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <silent> <Plug>SVNCommitDiff :SVNCommitDiff
nnoremap <silent> <Plug>SVNVimDiff :SVNVimDiff
nnoremap <silent> <Plug>SVNUpdate :SVNUpdate
nnoremap <silent> <Plug>SVNResolved :SVNResolved
nnoremap <silent> <Plug>SVNPropedit :SVNPropedit
nnoremap <silent> <Plug>SVNInfo :SVNInfo
nnoremap <silent> <Plug>SVNStatus :SVNStatus
nnoremap <silent> <Plug>SVNReview :SVNReview
nnoremap <silent> <Plug>SVNRevert :SVNRevert
nnoremap <silent> <Plug>SVNLog :SVNLog
nnoremap <silent> <Plug>SVNClearAndGotoOriginal :SVNGotoOriginal!
nnoremap <silent> <Plug>SVNGotoOriginal :SVNGotoOriginal
nnoremap <silent> <Plug>SVNDiff :SVNDiff
nnoremap <silent> <Plug>SVNCommit :SVNCommit
nnoremap <silent> <Plug>SVNAnnotate :SVNAnnotate
nnoremap <silent> <Plug>SVNAdd :SVNAdd
vmap <silent> <Plug>IMAP_JumpBack `<i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpBack "_<Del>i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpForward "_<Del>i=IMAP_Jumpfunc('', 0)
nmap <silent> <Plug>IMAP_JumpBack i=IMAP_Jumpfunc('b', 0)
nmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
nmap <silent> <Plug>RestoreWinPosn :call RestoreWinPosn()
nmap <silent> <Plug>SaveWinPosn :call SaveWinPosn()
nmap <SNR>26_WE <Plug>AlignMapsWrapperEnd
nmap <SNR>26_WS <Plug>AlignMapsWrapperStart
nnoremap <F7> :TlistToggle
map <Nul> @:
map <F4> :emenu 
vmap <BS> "-d
vmap <D-x> "*d
vmap <D-c> "*y
vmap <D-v> "-d"*P
nmap <D-v> "*P
cnoremap  <Home>
cnoremap  <Left>
cnoremap  <Del>
cnoremap  <End>
cnoremap  <Right>
imap S <Plug>ISurround
imap s <Plug>Isurround
cnoremap  =expand("<cword>")
imap 	 
imap <NL> <Plug>IMAP_JumpForward
cnoremap  <Down>
cnoremap  <Up>
imap  <Plug>Isurround
imap  =CtrlXPP()
cnoremap  =expand("<cWORD>")
cnoremap  
cnoremap j <S-Down>
cnoremap k <S-Up>
cnoremap E <C-Right>
cnoremap W <C-Right>
cnoremap B <C-Left>
cnoremap e <S-Right>
cnoremap w <S-Right>
cnoremap b <S-Left>
cmap ;\ \(\)<Left><Left>
map! ^O {^M}^[O^T
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set cpoptions=ABceFsv$
set directory=~/.tmp/vim
set display=lastline
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set formatoptions=tcqln
set grepprg=ack
set helplang=en
set history=1000
set listchars=tab:⎜\ ,trail:°
set omnifunc=rubycomplete#Complete
set ruler
set scrolloff=4
set shell=/bin/bash\ -l
set shiftwidth=2
set showmatch
set smarttab
set tabpagemax=50
set tabstop=4
set wildcharm=26
set wildmenu
set wildmode=longest,list
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/programming/ruby/poolparty/examples
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +0 fairchild_chef.rb
badd +0 fairchild.rb
args fairchild_chef.rb
edit fairchild_chef.rb
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
imap <buffer>  =RubyEndToken()
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal comments=:#
setlocal commentstring=#\ %s
setlocal complete=.,w,b,u,t,i
setlocal completefunc=<SNR>42_RCT_completion
setlocal nocopyindent
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*#\\s*define
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'ruby'
setlocal filetype=ruby
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=marker
setlocal foldmethod=marker
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcqln
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=^\\s*\\<\\(load\\|w*require\\)\\>
setlocal includeexpr=substitute(substitute(v:fname,'::','/','g'),'$','.rb','')
setlocal indentexpr=GetRubyIndent()
setlocal indentkeys=0{,0},0),0],!^F,o,O,e,=end,=elsif,=when,=ensure,=rescue,==begin,==end
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=ri
setlocal nolinebreak
setlocal nolisp
set list
setlocal list
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
setlocal nonumber
setlocal numberwidth=4
setlocal omnifunc=rubycomplete#Complete
setlocal path=.,/usr/local/lib/ruby/gems/1.8,/usr/local/lib/ruby/gems/1.8/gems,/usr/local/lib/ruby/gems/1.8/gems/mysql-2.7,/usr/local/lib/ruby/gems/1.8,/usr/local/lib/ruby/gems/1.8/gems,/usr/local/lib/ruby/gems/1.8/gems/mysql-2.7,/usr/local/lib/ruby/site_ruby/1.8,/usr/local/lib/ruby/site_ruby/1.8/i686-darwin9.6.0,/usr/local/lib/ruby/site_ruby,/usr/local/lib/ruby/1.8,/usr/local/lib/ruby/1.8/i686-darwin9.6.0,,/usr/local/lib/ruby/gems/1.8/gems/ParseTree-3.0.3/lib,/usr/local/lib/ruby/gems/1.8/gems/ParseTree-3.0.3/test,/usr/local/lib/ruby/gems/1.8/gems/RedCloth-4.1.9/ext,/usr/local/lib/ruby/gems/1.8/gems/RedCloth-4.1.9/lib,/usr/local/lib/ruby/gems/1.8/gems/RedCloth-4.1.9/lib/case_sensitive_require,/usr/local/lib/ruby/gems/1.8/gems/RubyInline-3.8.1/lib,/usr/local/lib/ruby/gems/1.8/gems/ZenTest-4.0.0/lib,/usr/local/lib/ruby/gems/1.8/gems/activesupport-2.3.2/lib,/usr/local/lib/ruby/gems/1.8/gems/auser-dslify-0.0.5/lib,/usr/local/lib/ruby/gems/1.8/gems/auser-parenting-0.0.1/lib,/usr/local/lib/ruby/gems/1.8/gems/cgi_multipart_eof_f
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=.rb
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'ruby'
setlocal syntax=ruby
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
let s:l = 1 - ((0 * winheight(0) + 19) / 39)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
tabedit fairchild.rb
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
imap <buffer>  =RubyEndToken()
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal comments=:#
setlocal commentstring=#\ %s
setlocal complete=.,w,b,u,t,i
setlocal completefunc=<SNR>42_RCT_completion
setlocal nocopyindent
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*#\\s*define
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'ruby'
setlocal filetype=ruby
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=marker
setlocal foldmethod=marker
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcqln
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=^\\s*\\<\\(load\\|w*require\\)\\>
setlocal includeexpr=substitute(substitute(v:fname,'::','/','g'),'$','.rb','')
setlocal indentexpr=GetRubyIndent()
setlocal indentkeys=0{,0},0),0],!^F,o,O,e,=end,=elsif,=when,=ensure,=rescue,==begin,==end
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=ri
setlocal nolinebreak
setlocal nolisp
set list
setlocal list
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
setlocal nonumber
setlocal numberwidth=4
setlocal omnifunc=rubycomplete#Complete
setlocal path=.,/usr/local/lib/ruby/gems/1.8,/usr/local/lib/ruby/gems/1.8/gems,/usr/local/lib/ruby/gems/1.8/gems/mysql-2.7,/usr/local/lib/ruby/gems/1.8,/usr/local/lib/ruby/gems/1.8/gems,/usr/local/lib/ruby/gems/1.8/gems/mysql-2.7,/usr/local/lib/ruby/site_ruby/1.8,/usr/local/lib/ruby/site_ruby/1.8/i686-darwin9.6.0,/usr/local/lib/ruby/site_ruby,/usr/local/lib/ruby/1.8,/usr/local/lib/ruby/1.8/i686-darwin9.6.0,,/usr/local/lib/ruby/gems/1.8/gems/ParseTree-3.0.3/lib,/usr/local/lib/ruby/gems/1.8/gems/ParseTree-3.0.3/test,/usr/local/lib/ruby/gems/1.8/gems/RedCloth-4.1.9/ext,/usr/local/lib/ruby/gems/1.8/gems/RedCloth-4.1.9/lib,/usr/local/lib/ruby/gems/1.8/gems/RedCloth-4.1.9/lib/case_sensitive_require,/usr/local/lib/ruby/gems/1.8/gems/RubyInline-3.8.1/lib,/usr/local/lib/ruby/gems/1.8/gems/ZenTest-4.0.0/lib,/usr/local/lib/ruby/gems/1.8/gems/activesupport-2.3.2/lib,/usr/local/lib/ruby/gems/1.8/gems/auser-dslify-0.0.5/lib,/usr/local/lib/ruby/gems/1.8/gems/auser-parenting-0.0.1/lib,/usr/local/lib/ruby/gems/1.8/gems/cgi_multipart_eof_f
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=.rb
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'ruby'
setlocal syntax=ruby
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
let s:l = 1 - ((0 * winheight(0) + 19) / 38)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
tabnext 2
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
