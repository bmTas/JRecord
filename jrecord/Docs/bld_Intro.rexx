/* Rexx 

	Rexx Program to build documentation
*/
	dir = "../Docs/"

	if  ISUNIX() then do
	    say 'Unix ...'
	    regina = 'rexx '
	    instalation = '/home/bm/Work/RecordEditor/Instalation/'
	    b2h='/home/bm/Work/Rexx/B2H/B2H.REXX'
	    sep= '/'
	    Copy = 'cp'
	    del='rm'
	    html = 'htm'
	    rename='mv'
	    q='"'
	end; else do
	    regina = '"C:\Regina\regina.exe"'
	    regina = "regina"
	    instalation = 'F:\Work\RecordEditor\Build\Instalation\' /*'E:\Work\RecordEdit\Instalation\'*/
	    b2h = 'F:\Work\Rexx\B2H\B2H.REXX'
	    sep ='\'
	    copy='Copy'
	    del='del'
	    html = 'htm'
	    rename = 'rename'
	    q=''
	end

	
	regina b2h '"JRecordIntro.dcf (HTMPEXT='html 'HTMLEXT='html 'LOG=Copy.log QUIET)"'

	

