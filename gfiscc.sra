$PBExportHeader$gfiscc.sra
$PBExportComments$[GFISCC] GFI system control center application
forward
global type gfiscc from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
n_syscon				gnv_syscon

end variables
global type gfiscc from application
string appname = "gfiscc"
string displayname = "GFISCC"
end type
global gfiscc gfiscc

type prototypes
FUNCTION boolean SetCursorPos(long x, long y) LIBRARY "user32"
FUNCTION boolean ClientToScreen(ulong hwnd, ref blob ablb) LIBRARY "user32"
FUNCTION long GetModuleFileName(ulong aul_hwnd, ref string as_file, long al_size) LIBRARY "kernel32" ALIAS FOR "GetModuleFileNameA"

end prototypes

forward prototypes
public function boolean of_setsyscon ()
end prototypes

public function boolean of_setsyscon ();RETURN f_setsyscon()
end function

on gfiscc.create
appname="gfiscc"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on gfiscc.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event close;TRY
	if IsValid(gnv_syscon) then
		if UpperBound(gnv_syscon.ids_parm)>0 then
			if IsValid(gnv_syscon.ids_parm[1]) then DESTROY gnv_syscon.ids_parm[1]
		end if
		DESTROY gnv_syscon
	end if
CATCH(RuntimeError lre)
END TRY

end event

event open;//////////////////////////////////////////////////////////////
//
// Event open
//
// Description: Genfare system control center
//
//////////////////////////////////////////////////////////////
ulong		lul
string	ls

ls=Space(255)
GetModuleFileName(Handle(This),ls,255)
ls=Upper(Trim(ls))
if Pos(ls,"\")>0 then ls=Mid(ls,LastPos(ls,"\")+1)

if ls="GFICVIEW.EXE" then
	DisplayName="GFICView"

	ls=" "+Upper(CommandParm())+" "
	if Pos(ls," -RVS ")>0 then
		ls="RVS"
	elseif Pos(ls," -INRVS ")>0 then
		ls="INRVS"
	elseif Pos(ls," -MNT ")>0 then
		ls="MNT"
	elseif Pos(ls," -INMNT ")>0 then
		ls="INMNT"
	elseif Pos(ls," -INSVC ")>0 then
		ls="INSVC"
	elseif Pos(ls," -OOS ")>0 then
		ls="OOS"
	elseif Pos(ls," -SEC ")>0 then
		ls="SEC"
	elseif Pos(ls," -DIS ")>0 then
		ls="DIS"
	else
		DisplayName="GFISCC"
	end if
elseif Pos(" "+Upper(CommandParm())+" "," -TPEM ")>0 then
	DisplayName="GFIPEMON"
end if

gnv_syscon=CREATE n_syscon
if Not gnv_syscon.of_init() then RETURN
gnv_syscon.of_setbatch(FALSE)

gnv_syscon.is_parm[1]=ls

ls="2.05.10"
if DisplayName="GFICView" then
	gnv_syscon.of_setparm("Genfare TVM Condition Viewer",ls)
elseif DisplayName="GFIPEMON" then
	gnv_syscon.of_setparm("Genfare PEM Status Monitor",ls)
	lul=f_findwin(gnv_syscon.of_gettitle()+" [Version "+gnv_syscon.of_getversion()+"]")
	if lul>0 then
		Send(lul,1024,1,0)
		RETURN
	end if
	gnv_syscon.ib_showlic=TRUE
	gnv_syscon.ib_RequireAUT=TRUE
else
	gnv_syscon.of_setparm("SPX Genfare System Control Center",ls)
	lul=f_findwin(gnv_syscon.of_gettitle()+" [Version "+gnv_syscon.of_getversion()+"]")
	if lul>0 then
		Send(lul,1024,1,0)
		RETURN
	end if
	gnv_syscon.ib_showlic=TRUE
	gnv_syscon.ib_RequireAUT=TRUE
end if

RegistryGet(gnv_syscon.of_getregkey()+"\Global","System Type",Regulong!,lul)
if lul=2 then
	f_createdir(gnv_syscon.of_getgfidir()+"001\dat")
	f_createdir(gnv_syscon.of_getgfidir()+"001\dat.arc")
	f_createdir(gnv_syscon.of_getgfidir()+"001\dat.err")
	f_createdir(gnv_syscon.of_getgfidir()+"001\dat.prc")
	f_createdir(gnv_syscon.of_getgfidir()+"001\dat.snd")
	f_createdir(gnv_syscon.of_getgfidir()+"cnf\TVM")
elseif lul=4 then
else
	MessageBox(gnv_syscon.of_gettitle(),"The "+gnv_syscon.of_gettitle()+" supports SPX Genfare Network Manager server and workstation only.",StopSign!)
	RETURN
end if


if Not f_checkbasever(gnv_syscon.of_getversion()) then RETURN

if DisplayName="GFICView" then
else
	if Not gnv_syscon.of_splash(TRUE) then RETURN
end if

if gnv_syscon.of_connect()<0 then RETURN

if Not gnv_syscon.of_checkdb("GFI") then RETURN


f_inituda()												// initialize User Defined Actions section in GFI.INI

if DisplayName="GFICView" then
	Open(w_gfiscc_view)
elseif DisplayName="GFIPEMON" then
	if f_buildeqlistds()>0 then Open(w_gfipemon)
else
//	if Not f_upgrade() then RETURN

	gnv_syscon.is_parm[1]=Trim(f_findmap())	// get map file name into is_parm[1]
	if gnv_syscon.is_parm[1]<>"" then
	else
		ls=gnv_syscon.of_getregkey2()+"\Custom\"+DisplayName
		if RegistryGet(ls,"No Map Warned",RegString!,ls)>0 and f_istrue(ls) then
		else
			RegistrySet(ls,"No Map Warned",RegString!,"Yes")
			MessageBox(gnv_syscon.of_gettitle(),Message.StringParm+"~r~n~r~nA blank backdrop will be used for the map view.",Exclamation!)
		end if
	end if
	gnv_syscon.is_parm[2]=Trim(f_findlogo())	// get logo file

	if f_buildeqlistds()>0 then Open(w_gfiscc)
end if

RETURN

f_getledx(0,0,0)

end event

event systemerror;f_systemerror(DisplayName)

end event

