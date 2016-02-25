$PBExportHeader$w_gfipemon.srw
$PBExportComments$[GFISCC] GFI PEM status monitor window
forward
global type w_gfipemon from window
end type
type mdi_1 from mdiclient within w_gfipemon
end type
type dw_eq_cache from datawindow within w_gfipemon
end type
type st_bar from statictext within w_gfipemon
end type
type dw_eq from datawindow within w_gfipemon
end type
type gb_toolbar from groupbox within w_gfipemon
end type
type dw_sum from datawindow within w_gfipemon
end type
type gb_sum from groupbox within w_gfipemon
end type
type gb_eq from groupbox within w_gfipemon
end type
type dw_sys from datawindow within w_gfipemon
end type
type gb_sys from groupbox within w_gfipemon
end type
end forward

global type w_gfipemon from window
boolean visible = false
integer width = 3419
integer height = 2760
boolean titlebar = true
string title = "Map Equipment"
string menuname = "m_gfiscc"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = mdihelp!
long backcolor = 67108864
string icon = "AppIcon!"
event ue_menuevent ( string as_action )
event ue_control pbm_custom01
mdi_1 mdi_1
dw_eq_cache dw_eq_cache
st_bar st_bar
dw_eq dw_eq
gb_toolbar gb_toolbar
dw_sum dw_sum
gb_sum gb_sum
gb_eq gb_eq
dw_sys dw_sys
gb_sys gb_sys
end type
global w_gfipemon w_gfipemon

type prototypes
FUNCTION boolean GetClientRect(ulong hwnd, ref blob rect) LIBRARY "user32"
FUNCTION ulong LoadIcon(long z, string file) LIBRARY "user32" ALIAS FOR "LoadIconA"
FUNCTION ulong LoadImage(long z, string iconfile, uint typ, long cx, long cy, uint load) LIBRARY "user32" ALIAS FOR "LoadImageA"
FUNCTION Boolean DestroyIcon(ulong hicon) LIBRARY "user32"

end prototypes

type variables
PROTECTED:
string				is_key, is_selected, is_clicked, is_selected_sorted, is_icon, is_disk_warn[2]={"90%","95%"}

datastore			ids_sort
m_gfiscc_popup		im_pop

n_tooltip			inv_tooltip					// tooltip object
int					ii_tID[2]					// tooltip ID
string				is_tdwo[2]					// tooltip datawindow object (prevent repeated tooltip calls)

date					id_today[2]
boolean				ib_hide_eq, ib_working, ib_resizing, ib_showrev, ib_simple, ib_filter
long					il_logo_h, il_clicked_pos[2], il_pos[2]
string				is_led[0 to 4]={"g","b","y","r","w"}
ulong					iul_led[0 to 4], iul_fmon, iul_evmon
string				is_status[0 to 4]={"Operating normally",&
												 "In revenue/maintenance service, or attention needed (Priority 3 alarm)",&
												 "Malfunctioning, out of service, or offline (Priority 2 alarm)",&
												 "Security alert in effect (Priority 1 alarm)",&
												 "Disconnected or not communicating"}
boolean				ib_nomap, ib_movemap
string				is_eq[]

long					il_CPU3= -3001				// 3 seconds timer
long					il_CPU60= -60001.0		// 1 minute timer

int					ii_reconn					// database connection failure count

PUBLIC:
boolean				ib_closing

end variables

forward prototypes
public subroutine of_setpopmenu ()
public subroutine of_showtip (string as_eq)
public subroutine of_select (string as_eq)
public subroutine of_showselected (boolean ab_show)
public subroutine of_barmove (long al_y)
public function boolean of_reconn ()
public subroutine of_sorteqlist ()
public subroutine of_setmenustatus ()
public subroutine of_init_menu ()
public function string of_geticon ()
public subroutine of_refresh_eq_dtl ()
public subroutine of_showrev ()
public subroutine of_settip (string as_eq)
public subroutine of_seteqstatus ()
public subroutine of_arrange ()
public function boolean of_buildlistdw ()
public subroutine of_showeqmenu (string as_eq)
public subroutine of_filter (string as_filter)
public subroutine of_download (string as_eq)
public subroutine of_selectall ()
public subroutine of_relist ()
public function boolean of_isallowed (string as_item)
end prototypes

event ue_menuevent(string as_action);long		ll
string	ls, ls_file
m_gfiscc	lm
ulong		lul[]
menu		lm_uda[10]

as_action=Upper(Trim(as_action))
choose case as_action
	case "SIMPLE"
		ib_simple=Not ib_simple
		of_relist()

	//////////////////////////////////////////////////////////////
	// File menu items
	//////////////////////////////////////////////////////////////
	case "POLL", "RESET"
		f_eq_cmd(as_action,is_selected)
	case "CMDINSVC", "CMDOOS", "CMDREBOOT", "CMDSHUTDOWN"
		f_eq_cmd(Mid(as_action,4),is_selected)
	case "FILEFS", "FILECFG", "FILESND", "FILEALL"
		f_eq_sendfile(Mid(as_action,5),is_selected)
	case "LOGVIEW"
		ls=gnv_syscon.of_getlogdir()+String(Today(),"yyyymmdd")+".stl"
		if FileExists(ls) then
			Run("logview.exe "+ls)
		else
			Run("logview.exe")
		end if
	case "GFIFMON"
		if gnv_syscon.inv_shell.of_run("gfifmon.exe -1000",lul) then iul_fmon=lul[3]
	case "GFIEVMON"
		if gnv_syscon.inv_shell.of_run("gfievmon.exe -500",lul) then iul_evmon=lul[3]
	case "RUN"
		Open(w_run,This)
	case "CHPWD"
		Open(w_pwd,This)
		if Message.DoubleParm=1 then f_slog("LOGIN","Password changed")
	case "SCREEN"
		f_printwindow(This)
	case "EXIT"
		Post Close(This)

	case "SELECTALL"
		of_selectall()

	//////////////////////////////////////////////////////////////
	// Edit menu items
	//////////////////////////////////////////////////////////////
	case "EDITCFS"
		if gnv_syscon.of_isallowed("CFS") then
			Run("gficfs.exe")
		else
			if gnv_syscon.ib_log_err then f_log(f_getmsg({"gfibase","428","Security violation - the user ~"%1~" does not have access to the %2 application",gnv_syscon.of_getuserid(),"Common Fare Structure Editor"}))
			gnv_syscon.of_msgbox({"gfibase","428","Security violation - the user ~"%1~" does not have access to the %2 application",gnv_syscon.of_getuserid(),"Common Fare Structure Editor"},".")
		end if

	case "EDITAUTO"
		if gnv_syscon.of_isallowed("AUTO") then
			Run("gfiedit.exe -tauto"+f_getlogin())
		else
			if gnv_syscon.ib_log_err then f_log(f_getmsg({"gfibase","428","Security violation - the user ~"%1~" does not have access to the %2 application",gnv_syscon.of_getuserid(),"Autoload Editor"}))
			gnv_syscon.of_msgbox({"gfibase","428","Security violation - the user ~"%1~" does not have access to the %2 application",gnv_syscon.of_getuserid(),"Autoload Editor"},".")
		end if

	case "EDITLOC", "EDITEQ", "EDITLIST", "EDITDEF", "EDITALM", "EDITMOD", "EDITMNT", "EDITFIC"
		Run("vipedit.exe -t"+Mid(as_action,5)+f_getlogin())

	//////////////////////////////////////////////////////////////
	// Tool menu items
	//////////////////////////////////////////////////////////////
	case "QUERY"
		Run("gfisql.exe"+f_getlogin())
	case "GFIDBA"
		Run("gfidba.exe")

	//////////////////////////////////////////////////////////////
	// Report menu items
	//////////////////////////////////////////////////////////////
	case "RPTPSS", "RPTBCT", "RPTDPT", "RPTDTR", "RPTLCR", "RPTCRR", "RPTMIR",&
		  "RPTSMR", "RPTAMI", "RPTXCP", "RPTSCR", "RPTEMP", "RPT3PB", "RPTMMR",&
		  "RPTFIC", "RPTEVL", "RPTVCR", "RPTSCH", "RPTSRG", "RPTMAF", "RPTSSM"
		Run("gfirpt.exe -t"+Mid(as_action,4)+f_getlogin())

	//////////////////////////////////////////////////////////////
	// Help menu items
	//////////////////////////////////////////////////////////////
	case "HELP"
		if FileExists(gnv_syscon.of_getgfidir()+"hlp\viphelp.pdf") then
			f_help(gnv_syscon.of_getgfidir()+"hlp\viphelp.pdf")
		else
			f_help("")
		end if
	case "ABOUT"
		gnv_syscon.of_about()

	case "UDA1", "UDA2", "UDA3", "UDA4", "UDA5", "UDA6", "UDA7", "UDA8", "UDA9", "UDA10"
		lm=MenuID
		lm_uda={lm.m_tool.m_uda.m_uda1,lm.m_tool.m_uda.m_uda2,lm.m_tool.m_uda.m_uda3,lm.m_tool.m_uda.m_uda4,lm.m_tool.m_uda.m_uda5,lm.m_tool.m_uda.m_uda6,lm.m_tool.m_uda.m_uda7,lm.m_tool.m_uda.m_uda8,lm.m_tool.m_uda.m_uda9,lm.m_tool.m_uda.m_uda10}

		ls=Trim(lm_uda[Integer(Mid(as_action,4))].Tag)
		choose case Upper(ls)
			case "RINIT"
				Run("gficmd.exe -l"+gnv_syscon.of_getlabel()+" -tINIT")
			case "GFIDIR"
				Run("explorer.exe /e,/select,"+gnv_syscon.of_getgfidir()+"cnf")
			case "GFIINI"
				Run("notepad.exe ~""+gnv_syscon.of_getgfidir()+"cnf\gfi.ini~"")
			case "INITTAB"
				Run("notepad.exe ~""+gnv_syscon.of_getgfidir()+"cnf\inittab~"")
			case else
				if f_run(ls) then f_slog("ACTIVITY","Launch user defined action: "+ls)
		end choose

	case else
		if as_action="STATUSPRINT" then
			f_printdw(dw_eq)
		elseif as_action="LISTPRINT" then
			f_printdw(dw_sys)

		//////////////////////////////////////////////////////////////
		// Popup menu items - equipment
		//////////////////////////////////////////////////////////////
		elseif Left(as_action,2)="EQ" then
			as_action=Mid(as_action,3)
			ll=Pos(as_action,"PEM")
			if ll>0 then
				ls=Mid(as_action,ll)+","
				as_action=Left(as_action,ll - 1)
				choose case as_action
					case "POLL", "RESET"
						f_eq_cmd(as_action,ls)
					case "CMDINSVC", "CMDOOS", "CMDREBOOT", "CMDSHUTDOWN"
						f_eq_cmd(Mid(as_action,4),ls)
					case "FILEFS", "FILECFG", "FILESND", "FILEALL"
						f_eq_sendfile(Mid(as_action,5),ls)
					case "DOWNLOAD"
						of_download(ls)
				end choose
			end if

		//////////////////////////////////////////////////////////////
		// Filter menu items
		//////////////////////////////////////////////////////////////
		elseif Left(as_action,6)="FILTER" then
			of_filter(Mid(as_action,7))

		elseif as_action<>"" then
			MessageBox(gnv_syscon.of_gettitle(),"Internal error: menu item "+as_action+" is not supported. "+gnv_syscon.is_suffix,StopSign!)
		else
			MessageBox(gnv_syscon.of_gettitle(),"Internal error: an undefined menu item was received. "+gnv_syscon.is_suffix,StopSign!)
		end if

end choose

end event

event ue_control;if wparam=1 then								// (1,1) - close this window from another program
	if lparam=1 then
		Post Close(This)
		RETURN 1
	else
		if WindowState=Minimized! then WindowState=Normal!
	end if
end if

if lparam=0 then
	SetPosition(TopMost!)
	Post SetPosition(NoTopMost!)
end if

RETURN 0

end event

public subroutine of_setpopmenu ();//////////////////////////////////////////////////////////////
//
// Subroutine of_setpopmenu()
//
// Description: customize popup menu for PEM
//
//////////////////////////////////////////////////////////////
string	ls

im_pop=CREATE m_gfiscc_popup				// instantiate the popup menu

ls=im_pop.m_list.m_listfilter.m_listfilter0.Text
im_pop.m_list.m_listfilter.m_listfilter0.Text=f_replace(ls,"TVM","PEM")
ls=im_pop.m_list.m_listfilter.m_listfilter1.Text
im_pop.m_list.m_listfilter.m_listfilter1.Text=f_replace(ls,"TVM","PEM")
ls=im_pop.m_list.m_listfilter.m_listfilter2.Text
im_pop.m_list.m_listfilter.m_listfilter2.Text=f_replace(ls,"TVM","PEM")
ls=im_pop.m_list.m_listfilter.m_listfilter3.Text
im_pop.m_list.m_listfilter.m_listfilter3.Text=f_replace(ls,"TVM","PEM")
ls=im_pop.m_list.m_listfilter.m_listfilter4.Text
im_pop.m_list.m_listfilter.m_listfilter4.Text=f_replace(ls,"TVM","PEM")
ls=im_pop.m_list.m_listfilter.m_listfilterall.Text
im_pop.m_list.m_listfilter.m_listfilterall.Text=f_replace(ls,"TVM","PEM")

end subroutine

public subroutine of_showtip (string as_eq);//////////////////////////////////////////////////////////////
//
// Subroutine of_showtip(string as_eq)
//
// Description: display tooltip for the specified equipment
//
// Argument: as_eq - equipment object name such as pem123
//
//////////////////////////////////////////////////////////////
long		ll[2]
blob{8}	lblb
											// get equipment's absolute map position
ll={Long(dw_sys.Describe("p_"+as_eq+".x")),Long(dw_sys.Describe("p_"+as_eq+".y"))}
											// relative to dw by subtracting scrolling factor; adjust by 8/16 pixels is to point to the center of the equipment icon (16x16/32x32)
ll={ll[1]+16 - Long(dw_sys.Describe("datawindow.horizontalscrollposition")),&
	 ll[2]+16 - Long(dw_sys.Describe("datawindow.verticalscrollposition"))}

BlobEdit(lblb,1,ll[1])
BlobEdit(lblb,5,ll[2])
ClientToScreen(Handle(dw_sys),lblb)	// convert to screen coordinates
											// set cursor based on the screen coordinates
SetCursorPos(Long(BlobMid(lblb,1,4)),Long(BlobMid(lblb,5)))
											// show tooltip
inv_tooltip.il_height_adjustment=0
of_settip(as_eq)
inv_tooltip.of_setdw(dw_sys,ii_tID[2],"p_"+as_eq)

end subroutine

public subroutine of_select (string as_eq);//////////////////////////////////////////////////////////////
//
// Subroutine of_select(string as_eq)
//
// Description: toggle selection of the specified equipment
//
// Argument: as_eq - equipment object name such as pem123_1
//
//////////////////////////////////////////////////////////////
as_eq=Trim(as_eq)
if as_eq<>"" then
	if Pos(is_selected,as_eq+",")>0 then	// selected, deselect it
		is_selected=f_replace(is_selected,as_eq+",","")
		dw_sys.Modify("t_"+as_eq+".visible=0")
	else												// not selected, select it
		is_selected+=as_eq+","
		dw_sys.Modify("t_"+as_eq+".x="+String(Long(dw_sys.Describe("p_"+as_eq+".x"))+11)+&
						 " t_"+as_eq+".y="+String(Long(dw_sys.Describe("p_"+as_eq+".y"))+3)+&
						 " t_"+as_eq+".visible=1")
	end if
end if

end subroutine

public subroutine of_showselected (boolean ab_show);//////////////////////////////////////////////////////////////
//
// Subroutine of_showselected(boolean ab_show)
//
// Description: show or hide selected signs
//
// Argument: ab_show - TRUE: show; FALSE: hide
//
//////////////////////////////////////////////////////////////
string	ls, ls_eq[]
int		li

if is_selected<>"" then		// is_selected contains comma delimited equipments end with a comma such as pem123,pem124,pem125,
	if ab_show then
		for li=f_ss2a(is_selected,ls_eq) to 1 step -1
			if ls_eq[li]<>"" then
				ls+=" t_"+ls_eq[li]+".x="+String(Long(dw_sys.Describe("p_"+ls_eq[li]+".x"))+11)+&
					 " t_"+ls_eq[li]+".y="+String(Long(dw_sys.Describe("p_"+ls_eq[li]+".y"))+3)+&
					 " t_"+ls_eq[li]+".visible=1"
			end if
		next
		dw_sys.Modify(Mid(ls,2))
	else
		ls="t_"+f_replace(is_selected,",",".visible=0 t_")
		ls=Left(ls,Len(ls) - 3)
		dw_sys.Modify(ls)
	end if
end if

end subroutine

public subroutine of_barmove (long al_y);//////////////////////////////////////////////////////////////
//
// Subroutine of_barmove(long al_y)
//
// Description: bar move resizing
//
// Argument: al_y - bar Y
//
//////////////////////////////////////////////////////////////
long	ll
int	li

ll=WorkSpaceHeight() - 24 - mdi_1.MicroHelpHeight + gb_toolbar.Y
al_y=Max(Min(al_y,ll+4),580)

li=gb_sum.Height - dw_sum.Height		// height difference between groupbox and datawindow

gb_sum.Height=al_y - 4 - gb_sum.Y
dw_sum.Height=gb_sum.Height - li

gb_sys.Height=gb_sum.Height
dw_sys.Height=dw_sum.Height

gb_eq.Y=al_y+12
if gb_eq.Y<ll then						// detail section is within the working area
	gb_eq.Height=ll - gb_eq.Y
	if Not gb_eq.Visible then gb_eq.Visible=TRUE

	if gb_eq.Height>li then				// groupbox has room for datawindow
		dw_eq.Y=gb_eq.Y+68
		dw_eq.Height=gb_eq.Height - li
		if Not dw_eq.Visible then dw_eq.Visible=TRUE
	else										// groupbox has no room for datawindow
		if dw_eq.Visible then dw_eq.Visible=FALSE
	end if
	ib_hide_eq=FALSE
else											// detail section has been squeezed out
	if gb_eq.Visible then gb_eq.Visible=FALSE
	if dw_eq.Visible then dw_eq.Visible=FALSE
	ib_hide_eq=TRUE
end if

st_bar.Y=al_y								// move the bar to position

end subroutine

public function boolean of_reconn ();//////////////////////////////////////////////////////////////
//
// Function boolean of_reconn()
//
// Description: make sure database connection is established
//
// Return: TRUE - database connection is ready; FALSE - error
//
//////////////////////////////////////////////////////////////
boolean	lb, lb_log[2]

if ii_reconn>0 then
	if CPU() - il_CPU60>60000.0 then	// 1 minute timer
		lb_log={gnv_syscon.ib_log_err,gnv_syscon.ib_log_db}
		gnv_syscon.ib_log_err=FALSE
		gnv_syscon.ib_log_db=FALSE

		if gnv_syscon.of_reconnect()<0 then
			ii_reconn++
			if ii_reconn>10 then
				if gnv_syscon.ib_log_db then f_log2("The database connection could not be restored after more than 10 attempts; the program is now exiting ...")
				Post Close(This)
			end if
		else
			lb=TRUE
			ii_reconn=0
		end if

		gnv_syscon.ib_log_err=lb_log[1]
		gnv_syscon.ib_log_db=lb_log[2]
	end if

elseif gnv_syscon.of_reconnect()<0 then
	ii_reconn=1
else
	lb=TRUE
end if

RETURN lb

end function

public subroutine of_sorteqlist ();//////////////////////////////////////////////////////////////
//
// Subroutine of_sorteqlist()
//
// Description: sort a comma delimited PEM list and remove station/duplicates
//
//////////////////////////////////////////////////////////////
string	ls
int		li

if is_selected<>"" then					// for single item, skip the logic (pem123_2)
	if Pos(is_selected,",")=LastPos(is_selected,",") then
		if gnv_syscon.ib_parm[1] then	// for duplicate equipment number among locations, 123 [2]
			ls=Mid(Replace(is_selected,Pos(is_selected,"_"),1," ["),4)
			is_selected_sorted=Left(ls,Len(ls) - 1)+"]"
		else									// e.g. 123
			is_selected_sorted=Mid(Left(is_selected,Pos(is_selected,"_") - 1),4)
		end if
		RETURN
	end if

	if Not IsValid(ids_sort) then
		ids_sort=CREATE datastore
		ids_sort.Create(SQLCA.SyntaxFromSQL("SELECT eq_n, loc_n FROM gfi_eq","",ls))
		ids_sort.SetSort("eq_n D, loc_n D")
	end if

	ls=Left(is_selected,Len(is_selected) - 1)
	ls=f_replace(f_replace(f_replace(ls,"pem",""),"_","~t"),",","~r~n")
	ids_sort.Reset()
	ids_sort.ImportString(ls)
	ids_sort.Sort()

	ls=""
	for li=ids_sort.RowCount() to 1 step -1
		ls+=", "+String(ids_sort.GetItemNumber(li,"eq_n"))
		if gnv_syscon.ib_parm[1] then ls+=" ["+String(ids_sort.GetItemNumber(li,"loc_n"))+"]"
	next
	is_selected_sorted=Mid(ls,3)
	ids_sort.Reset()
else
	is_selected_sorted=""
end if

end subroutine

public subroutine of_setmenustatus ();//////////////////////////////////////////////////////////////
//
// Subroutine of_setmenustatus()
//
// Description: set menu item status
//
//////////////////////////////////////////////////////////////
m_gfiscc	lm
boolean	lb

lm=MenuID
if is_selected<>"" then lb=TRUE

if lm.m_file.m_poll.Visible then lm.m_file.m_poll.Enabled=lb

if lm.m_file.m_sendcmd.Visible then
	lm.m_file.m_sendcmd.Enabled=lb
	lm.m_file.m_sendcmd.m_cmdinsvc.Enabled=lb
	lm.m_file.m_sendcmd.m_cmdoos.Enabled=lb
	lm.m_file.m_sendcmd.m_cmdreboot.Enabled=lb
	lm.m_file.m_sendcmd.m_cmdshutdown.Enabled=lb
end if

if lm.m_file.m_sendfile.Visible then
	lm.m_file.m_sendfile.Enabled=lb
	lm.m_file.m_sendfile.m_filecfg.Enabled=lb
	lm.m_file.m_sendfile.m_filesnd.Enabled=lb
	lm.m_file.m_sendfile.m_fileall.Enabled=lb
end if

end subroutine

public subroutine of_init_menu ();//////////////////////////////////////////////////////////////
//
// Subroutine of_init_menu()
//
// Description: initialize menu
//
//////////////////////////////////////////////////////////////
m_gfiscc	lm

lm=MenuID
lm.m_file.m_view.Visible=FALSE
lm.m_file.m_view.ToolbarItemVisible=FALSE
lm.m_file.m_pem.Visible=FALSE
lm.m_file.m_pem.ToolbarItemVisible=FALSE
lm.m_file.m_file_bar0.Visible=FALSE

if Not gnv_syscon.of_isallowed("Poll") then
	lm.m_file.m_poll.Visible=FALSE
	lm.m_file.m_poll.ToolbarItemVisible=FALSE
end if

if gnv_syscon.of_isallowed("Control") then
else
	lm.m_file.m_sendcmd.Visible=FALSE
	lm.m_file.m_sendcmd.m_cmdinsvc.ToolbarItemVisible=FALSE
	lm.m_file.m_sendcmd.m_cmdoos.ToolbarItemVisible=FALSE
	lm.m_file.m_sendcmd.m_cmdreboot.ToolbarItemVisible=FALSE
	lm.m_file.m_sendcmd.m_cmdshutdown.ToolbarItemVisible=FALSE

	lm.m_file.m_sendfile.Visible=FALSE
	lm.m_file.m_sendfile.m_filecfg.ToolbarItemVisible=FALSE
	lm.m_file.m_sendfile.m_filesnd.ToolbarItemVisible=FALSE
	lm.m_file.m_sendfile.m_fileall.ToolbarItemVisible=FALSE

	if Not lm.m_file.m_poll.Visible then
		lm.m_file.m_file_bar1.Visible=FALSE
	end if
end if

if Not gnv_syscon.of_isallowed("Logview") then
	lm.m_file.m_logview.Visible=FALSE
	lm.m_file.m_logview.ToolbarItemVisible=FALSE
	lm.m_file.m_gfifmon.Visible=FALSE
	lm.m_file.m_gfifmon.ToolbarItemVisible=FALSE
	lm.m_file.m_gfievmon.Visible=FALSE
	lm.m_file.m_gfievmon.ToolbarItemVisible=FALSE
	lm.m_file.m_file_bar2.Visible=FALSE
end if

if f_getlogin()="" then
	lm.m_file.m_chpwd.Visible=FALSE
	lm.m_file.m_file_bar4.Visible=FALSE
end if

lm.m_edit.m_editlite.Visible=FALSE
lm.m_edit.m_edit_1.Visible=FALSE

lm.m_tool.m_mapeq.Visible=FALSE
lm.m_tool.m_mapeq.ToolbarItemVisible=FALSE
lm.m_tool.m_tool_bar1.Visible=FALSE

if Not gnv_syscon.of_isallowed("Query") then
	lm.m_tool.m_query.Visible=FALSE
	lm.m_tool.m_query.ToolbarItemVisible=FALSE
end if

if f_istrue(gnv_syscon.of_getappparm("Show Infraction")) then
else
	lm.m_edit.m_editfic.Visible=FALSE
	lm.m_report.m_fic.Visible=FALSE
end if

lm.m_help.m_about.Text="&About "+gnv_syscon.of_gettitle()
lm.m_help.m_about.MicroHelp="About "+gnv_syscon.of_gettitle()
lm.m_help.m_about.ToolbarItemText=lm.m_help.m_about.MicroHelp

end subroutine

public function string of_geticon ();//////////////////////////////////////////////////////////////
//
// Function string of_geticon()
//
// Description: construct icon formula
//
// Return: icon formula used by map datawindow
//
//////////////////////////////////////////////////////////////
string			ls
int				li
constant int	IMAGE_ICON=1, LR_LOADFROMFILE=16, LR_DEFAULTSIZE=64, LR_VGACOLOR=128

if is_icon<>"" then
else
	is_icon="bitmap('led_'+case(<COL> when 0 then 'g' when 1 then 'b' when 2 then 'y' when 3 then 'r' when 4 then 'w' else 'w')+'.gif')"

	DECLARE cur CURSOR FOR
		SELECT st.code, co.name FROM gfi_lst co, gfi_lst st
		WHERE co.type='COLOR' AND co.class=0
		  AND st.type='EQ STATUS' AND st.code BETWEEN 0 AND 4 AND co.code=st.class;

	OPEN cur;
	if SQLCA.SQLCode=0 then
		do														// customize led color
			FETCH cur INTO :li, :ls;
			if SQLCA.SQLCode=0 then
				ls=Lower(Trim(ls))
				if is_led[li]=ls then
				else
					is_icon=f_replace(is_icon,String(li)+" then '"+is_led[li]+"'",String(li)+" then '"+ls+"'")
					is_led[li]=ls
				end if
			end if
		loop while SQLCA.SQLCode=0
		CLOSE cur;
	end if

//	for li=0 to 4
//		iul_led[li]=LoadIcon(0,Long(32517,0))
//		iul_led[li]=LoadIcon(0,"led32_"+is_led[li]+".ico")
//		iul_led[li]=LoadImage(0,"led_"+is_led[li]+".ico",IMAGE_ICON,0,0,LR_LOADFROMFILE)
//	next
end if

RETURN is_icon

end function

public subroutine of_refresh_eq_dtl ();//////////////////////////////////////////////////////////////
//
// Subroutine of_refresh_eq_dtl()
//
// Description: refresh PEM detail section
//
//////////////////////////////////////////////////////////////
string	ls, ls_eq
int		li, li_loc, li_eq, li_aut
long		ll

if is_selected_sorted<>"" then
	ls="PEM Operation Detail ("+is_selected_sorted+")"
else
	ls="PEM Operation Detail"
end if
if gb_eq.Text<>ls then gb_eq.Text=ls

li_aut=f_getaut()
dw_eq_cache.Reset()
dw_eq_cache.SetTransObject(SQLCA)

ls=is_selected							// object list: pem123_1,pem101_2,
do
	li=Pos(ls,",")
	if li>0 then
		ls_eq=Mid(Left(ls,li - 1),4)
		li_loc=Integer(Mid(ls_eq,Pos(ls_eq,"_")+1))
		li_eq=Integer(Left(ls_eq,Pos(ls_eq,"_") - 1))
		ls=Mid(ls,li+1)

		if li_loc>=0 and li_eq>0 then
			dw_eq_cache.Retrieve(li_aut,li_loc,2,li_eq)
		end if
	end if

	Yield()
	if ib_closing then RETURN
loop while li>0

ls=dw_eq_cache.Describe("datawindow.data")
if ls=dw_eq.Describe("datawindow.data") then RETURN

dw_eq.SetRedraw(FALSE)

li=dw_eq.GetRow()
if li>0 then
	li_loc=dw_eq.GetItemNumber(li,"loc_n")
	li_eq=dw_eq.GetItemNumber(li,"eq_n")
else
	li_loc=0
	li_eq=0
end if
ll=Long(dw_eq.Describe("datawindow.VerticalScrollPosition"))

dw_eq.Reset()
dw_eq.ImportString(ls)
dw_eq.Sort()

li=dw_eq.Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,dw_eq.RowCount()+1)
if li>0 then
	dw_eq.ScrollToRow(li)
	if ll>0 then dw_eq.Modify("datawindow.VerticalScrollPosition="+String(ll))
end if

dw_eq.SetRedraw(TRUE)

end subroutine

public subroutine of_showrev ();//////////////////////////////////////////////////////////////
//
// Subroutine of_showrev()
//
// Description: protect/unprotect revenue fields
//
//////////////////////////////////////////////////////////////
string	ls

if ib_showrev then
	ls="total.format=~"'[general]'~tif(upper(name)='REV','\ ','[general]')~" p_protect.visible=~"0~tif(upper(name)='REV',1,0)~""
else
	ls="total.format='[general]' p_protect.visible=0"
end if
dw_sum.Modify(ls)
ib_showrev=Not ib_showrev

end subroutine

public subroutine of_settip (string as_eq);//////////////////////////////////////////////////////////////
//
// Subroutine of_settip(string as_eq)
//
// Description: set equipment tooltip text
//
// Argument: as_eq - equipment object name
//
//////////////////////////////////////////////////////////////
int		li, li_loc, li_eq, li_limit, li_type
date		ld[2]
long		ll[5]
dec{2}	ldec
string	ls, ls_type, ls_ID, ls_log
datetime	ldt

li=dw_sys.GetItemNumber(1,as_eq)
if li>=0 and li<=4 then
else
	li=4
end if

ls=is_status[li]

if iul_led[li]>0 then
	li=iul_led[li]
else
	choose case li
		case 0
			li=1
		case 1
			li=2
		case else
			li=3
	end choose
end if

// equipment label is stored in t_<eq>'s tag field
inv_tooltip.of_settitle(dw_sys.Describe("t_"+as_eq+".tag"),li)

li_loc=Integer(Mid(as_eq,LastPos(as_eq,"_")+1))
li_eq=Integer(Mid(Left(as_eq,Pos(as_eq,"_") - 1),4))

ld[1]=Today()
ld[2]=RelativeDate(ld[1],1)

SELECT SUM((CASE WHEN type IN (319, 320) THEN 0.0 ELSE price END)),
		 COUNT(*), SUM((CASE WHEN type=302 THEN 1 ELSE 0 END)),
		 SUM((CASE WHEN type IN (303, 312) THEN 1 ELSE 0 END)),
		 SUM((CASE WHEN type=316 THEN 1 ELSE 0 END)),
		 SUM((CASE WHEN tr_seq=0 AND type IN (302, 303, 304, 305, 307, 308, 311, 312, 314, 316, 317, 318, 319, 320) THEN 1 ELSE 0 END))
INTO :ldec, :ll[5], :ll[1], :ll[2], :ll[3], :ll[4]
FROM vnd_tr
WHERE loc_n=:li_loc AND eq_type=2 AND eq_n=:li_eq AND ts>=:ld[1] AND ts<:ld[2];
if SQLCA.SQLCode=0 and ll[5]>0 then
	ls+="<rn><rn>[Today's Summary]<rn>"

	if ib_showrev then ls+="<rn>Revenue:<t><t>"+String(ldec,"$#0.00")

	ls+="<rn>Cards issued:<t><t>"+String(ll[1],"#,##0")
	ls+="<rn>Cards recharged:<t>"+String(ll[2],"#,##0")
	ls+="<rn>Cards validated:<t><t>"+String(ll[3],"#,##0")
	ls+="<rn>Customers served:<t>"+String(ll[4],"#,##0")
end if

li_limit=Integer(gnv_syscon.of_getappparm("Last N Alarm"))
if li_limit>0 then
	SELECT MAX(ts) INTO :ldt FROM vnd_ev
	WHERE loc_n=:li_loc AND eq_type=2 AND eq_n=:li_eq;
	if SQLCA.SQLCode=0 and ldt>DateTime(1900-01-01) then
		ld[1]=Date(ldt)

		DECLARE cur CURSOR FOR
			SELECT ts, ev.type, name, n, mod_type, mod_pos, mod_id, userid, log
			FROM vnd_ev ev, gfi_lst evt
			WHERE loc_n=:li_loc AND eq_type=2 AND eq_n=:li_eq AND ts>=:ld[1]
			  AND evt.type='EV TYPE' AND class>1 AND ev.type=evt.code
			ORDER BY ts DESC, ev_id DESC;
		OPEN cur;
		if SQLCA.SQLCode=0 then
			ls+="<rn><rn>[Last "+String(li_limit)+" Events]<rn>"

			for li=1 to li_limit
				FETCH cur INTO :ldt,:li_type,:ls_type,:ll[1],:ll[2],:ll[3],:ls_ID,:ll[4],:ls_log;
				if SQLCA.SQLCode=0 then
					ls+="<rn>"+String(ldt,"yyyy-mm-dd hh:mm:ss")+" ["+String(li_type)+"] "+ls_type
					ls_ID=Trim(ls_ID)
					if ls_ID<>"" and ls_ID<>"0" then
						ls_ID=": "+ls_ID
					else
						ls_ID=""
					end if

					choose case ll[2]
						case 1
							ls+=" (Hopper"+String(ll[3]+1)+ls_ID+")"
						case 2
							ls+=" (TRiM"+String(ll[3])+ls_ID+")"
						case 3
							ls+=" (Cashbox"+ls_ID+")"
						case 4
							ls+=" (Bill Stacker"+ls_ID+")"
						case 5
							ls+=" (Coin Tekpak"+ls_ID+")"
						case 6
							ls+=" (Bill Tekpak"+ls_ID+")"
					end choose

					choose case li_type
						case 591, 602, 570 to 572
							ls+=" ("+f_replace(String(ll[1]*9.0/500.0+32.0,"#0.##")+" ",". "," ")+"캟)"
						case 598
							choose case ll[1]
								case 1
									ls+=" (Local)"
								case 2
									ls+=" (Remote)"
								case 3
									ls+=" (Power loss)"
								case 4
									ls+=" (Temperature)"
								case 5
									ls+=" (Remote reboot)"
							end choose
						case 704
							choose case ll[1]
								case 0
									ls+=" (Sensor missing)"
								case 1
									ls+=" (Closed)"
								case 2
									ls+=" (Open)"
								case 3
									ls+=" (Partially open)"
								case else
									ls+=" (Unknown)"
							end choose
						case 548, 740
							choose case ll[1]
								case 0
									ls+=" (Sensor missing)"
								case 1
									ls+=" (Bad sensor)"
								case 2
									ls+=" (Closed)"
								case 3
									ls+=" (Open)"
								case 4
									ls+=" (Misaligned)"
								case else
									ls+=" (Unknown)"
							end choose
						case 406, 417, 418, 419, 426, 468, 701, 702, 703, 715, 721, 735
							if ll[1]=1 then ls+=" (Condition cleared)"
						case 730
							if ll[1]>0 then ls+=" ("+String(ll[1])+")"
						case is<900
							if ll[4]>0 then ls+=" (User ID: "+String(ll[4])+")"
						case 952
							choose case ll[1]
								case 0
									ls+=" (OK)"
								case 1
									ls+=" ("+is_disk_warn[1]+" full)"
								case 2
									ls+=" ("+is_disk_warn[2]+" full)"
								case else
									ls+=" (Status unknown)"
							end choose
						case 900, 901, 902, 903, 993 to 997
							if ls_ID<>"" then ls+=" (User ID"+ls_ID+")"
					end choose

					ls_log=Trim(ls_log)
					if ls_log<>"" then ls+=" ("+ls_log+")"
				else
					EXIT
				end if
			next
			CLOSE cur;
		end if
	end if
end if

if dw_sys.Describe("p_"+as_eq+".tag")=ls then
else
	dw_sys.Modify("p_"+as_eq+".tag=~""+ls+"~"")
end if

end subroutine

public subroutine of_seteqstatus ();//////////////////////////////////////////////////////////////
//
// Subroutine of_seteqstatus()
//
// Description: set equipment status in map datawindow
//
//////////////////////////////////////////////////////////////
int		li, li_row
string	ls, ls_IP
boolean	lb

for li_row=gnv_syscon.ids_parm[1].RowCount() to 1 step -1
	Yield()
	if ib_closing then RETURN

	ls="pem"+String(gnv_syscon.ids_parm[1].GetItemNumber(li_row,"eq_n"))+"_"+String(gnv_syscon.ids_parm[1].GetItemNumber(li_row,"loc_n"))
	li=gnv_syscon.ids_parm[1].GetItemNumber(li_row,"status")
	if li>=0 and li<=4 then
	else
		li=4
	end if

	if Match(dw_sys.Describe("p_"+ls+".x"),"^[0-9]+$") then
		if li=dw_sys.GetItemNumber(1,ls) then
		else
			dw_sys.SetItem(1,ls,li)

			ls_IP=Trim(gnv_syscon.ids_parm[1].GetItemString(li_row,"ip"))
			if ls_IP<>"" then
				dw_sys.Modify("t_"+ls+".tag=~""+gnv_syscon.ids_parm[1].GetItemString(li_row,"label")+" ("+ls_IP+")~"")
			else
				dw_sys.Modify("t_"+ls+".tag=~""+gnv_syscon.ids_parm[1].GetItemString(li_row,"label")+"~"")
			end if
			lb=TRUE
		end if
	end if
next

if lb then
	if ib_simple and ib_filter then of_arrange()
	dw_sys.SetRedraw(TRUE)
end if

end subroutine

public subroutine of_arrange ();//////////////////////////////////////////////////////////////
//
// Subroutine of_arrange()
//
// Description: arrange equipment icon locations so that they fit best in the list view
//
//////////////////////////////////////////////////////////////
string	ls, ls_eq[], ls_loc, ls2
long		ll_width, ll[2]={11, 4}, ll_gb_width, ll_eq_x
blob{16}	lblb
int		li, li_cnt, li_eq

GetClientRect(Handle(dw_sys),lblb)
ll_width=Long(BlobMid(lblb,9,4))		// datawindow client area width

if ib_simple then
	// the first equipment is at (5,4); the horizontal equipment gap is 1; the vertical equipment gap is 6
	li_cnt=f_ss2a(is_eq[1],ls_eq)
	if li_cnt>0 then
		ll={5,4}
		for li_eq=1 to li_cnt
			ls2=dw_sys.Describe("p_"+ls_eq[li_eq]+".visible")
			if ls2="1" then
			elseif Pos(ls2,"~t")>0 then
				if dw_sys.Describe("evaluate('"+Mid(ls2,Pos(ls2,"~t")+1)+"',1)")="1" then
				else
					CONTINUE
				end if
			else
				CONTINUE
			end if

			ls+=" p_"+ls_eq[li_eq]+".x="+String(ll[1])+" p_"+ls_eq[li_eq]+".y="+String(ll[2])+&
				 " "+ls_eq[li_eq]+"_t.x="+String(ll[1])+" "+ls_eq[li_eq]+"_t.y="+String(ll[2]+33)
			ll[1]+=33							// next position
			if ll[1]+33>ll_width then ll={5,ll[2]+53}
		next
	end if
else
	// the first groupbox starts at (11,4); its min size is 208x70 which holds 6 equipments
	// the first equipment has a gap of 5 to the left edge of the groupbox
	// the last equipment has a gap of 6 to the right edge of the groupbox
	// groupbox width=10+33n where n is the number of PEMs for the station
	// groupbox vertical gap is 6; horizontal gap is 11
	// equipment is 17 down from top of groupbox
	for li=1 to UpperBound(is_eq)
		li_cnt=f_ss2a(is_eq[li],ls_eq)
		if li_cnt>0 then
			if li_cnt>7 then
				ll_gb_width=10+33*(li_cnt - 1)
			else
				ll_gb_width=208
			end if								// if exceeding max width, wrap
			if ll[1]+ll_gb_width>ll_width then ll={11,ll[2]+76}

			ls_loc=ls_eq[1]
			ls+=" gb_"+ls_loc+".x="+String(ll[1])+" gb_"+ls_loc+".y="+String(ll[2])
			if ll_gb_width>208 then ls+=" gb_"+ls_loc+".width="+String(ll_gb_width)

			ll_eq_x=ll[1]+5
			for li_eq=2 to li_cnt
				ls2="pem"+ls_eq[li_eq]+"_"+ls_loc
				ls+=" p_"+ls2+".x="+String(ll_eq_x)+" p_"+ls2+".y="+String(ll[2]+17)+&
					 " "+ls2+"_t.x="+String(ll_eq_x)+" "+ls2+"_t.y="+String(ll[2]+50)
				ll_eq_x+=33
			next

			ll[1]+=ll_gb_width+11
		end if
	next
end if

if ls<>"" then
	if ib_simple then
		ls+=" datawindow.detail.height="+String(ll[2]+49)
	else
		ls+=" datawindow.detail.height="+String(ll[2]+72)
	end if
	dw_sys.Modify(Mid(ls,2))
	of_showselected(TRUE)
end if

end subroutine

public function boolean of_buildlistdw ();//////////////////////////////////////////////////////////////
//
// Function boolean of_buildlistdw()
//
// Description: construct list datawindow
//
// Return: TRUE - successful; FALSE - error
//
//////////////////////////////////////////////////////////////
string	ls, ls_eq, ls_loc, ls_icon
long		ll
int		li_loc= -9999, li_aut, li, li_eq

ls=f_replace(f_replace(f_replace(f_getdwhdr(),"<TITLE>","Equipment List"),&
	"gfihand.cur","Arrow!"),"<DBH>","500")+f_getdwcol()
														// use 32x32 icon in list view
ls_icon=f_replace(of_geticon(),"led_","led32_")
														// sort data by location and equipment number, location 0 at bottom
if ib_simple then
	gnv_syscon.ids_parm[1].SetSort("eq_n D")
else
	gnv_syscon.ids_parm[1].SetSort("if(loc_n>0,0,1) D, loc_n D, eq_n D")
end if
gnv_syscon.ids_parm[1].Sort()

is_eq={""}											// equipment list by location

li_aut=f_getaut()
for ll=gnv_syscon.ids_parm[1].RowCount() to 1 step -1
	if ib_simple then
		li_loc=gnv_syscon.ids_parm[1].GetItemNumber(ll,"loc_n")
	elseif li_loc<>gnv_syscon.ids_parm[1].GetItemNumber(ll,"loc_n") then
		li_loc=gnv_syscon.ids_parm[1].GetItemNumber(ll,"loc_n")
		if li_loc>0 then
			SELECT name INTO :ls_loc FROM gfi_lst WHERE type='EQ LOC' AND class=:li_aut AND code=:li_loc;
			if SQLCA.SQLCode=0 and Trim(ls_loc)<>"" then
				ls_loc="Station "+String(li_loc)+" - "+Trim(ls_loc)
			else
				ls_loc="Station "+String(li_loc)
			end if
		else
			ls_loc="Not Stationed"
		end if										// each station has a groupbox
		ls+="groupbox(band=detail text=~""+ls_loc+"~" border='5' color='8388608' x='11' y='4' height='70' width='208' name=gb_"+String(li_loc)+" visible='1' font.face='Microsoft Sans Serif' font.height='-8' font.weight='400' font.family='2' font.pitch='2' font.charset='0' background.mode='1' background.color='553648127')~r~n"

		li_eq++										// each equipment list string begins with station number
		is_eq[li_eq]=String(li_loc)
	end if

	li=gnv_syscon.ids_parm[1].GetItemNumber(ll,"status")
	if li>=0 and li<=4 then
	else
		li=4
	end if

	ls_eq=String(gnv_syscon.ids_parm[1].GetItemNumber(ll,"eq_n"))
	if ib_simple then
		is_eq[1]+=",tvm"+ls_eq+"_"+String(li_loc)
	else
		is_eq[li_eq]+=","+ls_eq
	end if

	ls_eq="pem"+ls_eq+"_"+String(li_loc)
	ls+="compute(band=detail alignment='0' expression=~""+f_replace(ls_icon,"<COL>",ls_eq)+"~" border='0' color='8388608' x='5' y='5' height='32' width='32' format='[GENERAL]' html.valueishtml='0' name=p_"+ls_eq+" tag=~""+is_status[li]+"~" visible='1' font.face='Microsoft Sans Serif' font.height='-8' font.weight='400' font.family='2' font.pitch='2' font.charset='0' background.mode='1' background.color='536870912')~r~n"+&
		 "text(band=detail alignment='2' text='a' border='0' color='255~tif("+ls_eq+"=3,65280,255)' x='5' y='5' height='19' width='12' html.valueishtml='0' name=t_"+ls_eq+" tag=~""+gnv_syscon.ids_parm[1].GetItemString(ll,"label")+"~" visible='0' font.face='Webdings' font.height='-18' font.weight='400' font.family='1' font.pitch='2' font.charset='2' background.mode='1' background.color='553648127')~r~n"+&
		 "text(band=detail alignment='2' text='"+String(gnv_syscon.ids_parm[1].GetItemNumber(ll,"eq_n"))+"' border='0' color='8388608' x='5' y='5' height='14' width='32' html.valueishtml='0' name="+ls_eq+"_t visible='1' font.face='Microsoft Sans Serif' font.height='-8' font.weight='400' font.family='2' font.pitch='2' font.charset='0' background.mode='1' background.color='536870912')~r~n"
next
gnv_syscon.ids_parm[1].SetSort("eq_n D")
gnv_syscon.ids_parm[1].Sort()

if ib_simple then is_eq[1]=Mid(is_eq[1],2)

if dw_sys.Create(ls+f_getdwftr(),ls)>0 then
	dw_sys.InsertRow(0)
	of_arrange()
	RETURN TRUE
else
	MessageBox(gnv_syscon.of_gettitle(),"Internal error: creating equipment list datawindow failed. "+gnv_syscon.is_suffix+"~r~n~r~n"+ls,StopSign!)
	RETURN FALSE
end if

end function

public subroutine of_showeqmenu (string as_eq);//////////////////////////////////////////////////////////////
//
// Subroutine of_showeqmenu(string as_eq)
//
// Description: show equipment popup menu
//
// Argument: as_eq - equipment object name such as pem123_1
//
//////////////////////////////////////////////////////////////
string	ls
boolean	lb[3]
int		li_loc, li_eq, li

lb={gnv_syscon.of_isallowed("Poll"),gnv_syscon.of_isallowed("Control"),gnv_syscon.of_isallowed("Download")}
if lb[1] or lb[2] or lb[3] then	 
else
	RETURN
end if

as_eq=Upper(as_eq)
if gnv_syscon.ib_parm[1] then		// duplicate equipment number among locations
	ls=Replace(Replace(as_eq,Pos(as_eq,"_"),1," ["),4,0," ")+"]"
else
	ls=Replace(Left(as_eq,Pos(as_eq,"_") - 1),4,0," ")
end if
im_pop.m_eq.m_reset.Text="Reset "+ls+" Status"
im_pop.m_eq.m_reset.Tag="EQRESET"+as_eq
im_pop.m_eq.m_reset.MicroHelp="Reset "+ls+" status to normal"

li_loc=Integer(Mid(as_eq,LastPos(as_eq,"_")+1))
li_eq=Integer(Mid(Left(as_eq,Pos(as_eq,"_") - 1),4))

if lb[2] then
	if dw_sys.GetItemNumber(1,as_eq)=0 then	// already normal
		im_pop.m_eq.m_reset.Enabled=FALSE
	else
		SELECT 0 INTO :SQLCA.SQLNRows FROM gfi_eq_status
		WHERE loc_n=:li_loc AND eq_type=2 AND eq_n=:li_eq AND st_id=0;
		im_pop.m_eq.m_reset.Enabled=(SQLCA.SQLCode=0)
	end if
else
	im_pop.m_eq.m_reset.Enabled=FALSE
end if

im_pop.m_eq.m_poll.Text="Poll "+ls
im_pop.m_eq.m_poll.Tag="EQPOLL"+as_eq
im_pop.m_eq.m_poll.MicroHelp="Poll "+ls+" for status and transactions"
im_pop.m_eq.m_poll.Enabled=lb[1] 

im_pop.m_eq.m_sendcmd.Text="Send Command to "+ls
im_pop.m_eq.m_sendcmd.m_insvc.Tag="EQCMDINSVC"+as_eq
im_pop.m_eq.m_sendcmd.m_insvc.MicroHelp="Set "+ls+" into service mode"
im_pop.m_eq.m_sendcmd.m_oos.Tag="EQCMDOOS"+as_eq
im_pop.m_eq.m_sendcmd.m_oos.MicroHelp="Take "+ls+" out of service"
im_pop.m_eq.m_sendcmd.m_reboot.Tag="EQCMDREBOOT"+as_eq
im_pop.m_eq.m_sendcmd.m_reboot.MicroHelp="Reboot "+ls
im_pop.m_eq.m_sendcmd.m_shutdown.Tag="EQCMDSHUTDOWN"+as_eq
im_pop.m_eq.m_sendcmd.m_shutdown.MicroHelp="Shutdown "+ls
im_pop.m_eq.m_sendcmd.Enabled=lb[2] 

im_pop.m_eq.m_sendfile.Text="Send File to "+ls
im_pop.m_eq.m_sendfile.m_cfg.Tag="EQFILECFG"+as_eq
im_pop.m_eq.m_sendfile.m_cfg.MicroHelp="Send configuration settings to "+ls
im_pop.m_eq.m_sendfile.m_snd.Tag="EQFILESND"+as_eq
im_pop.m_eq.m_sendfile.m_snd.MicroHelp="Send sound file(s) to "+ls
im_pop.m_eq.m_sendfile.m_all.Tag="EQFILEALL"+as_eq
im_pop.m_eq.m_sendfile.m_all.MicroHelp="Send all configuration files to "+ls
im_pop.m_eq.m_sendfile.Enabled=lb[2] 

if lb[3] then
	li=gnv_syscon.ids_parm[1].Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,gnv_syscon.ids_parm[1].RowCount()+1)
	if li>0 then
		if Trim(gnv_syscon.ids_parm[1].GetItemString(li,"ip"))<>"" then
		else
			lb[3]=FALSE
		end if
	else
		lb[3]=FALSE
	end if
end if

im_pop.m_eq.m_download.Text="Download log files from "+ls
im_pop.m_eq.m_download.Tag="EQDOWNLOAD"+as_eq
im_pop.m_eq.m_download.MicroHelp="Download log files from "+ls
im_pop.m_eq.m_download.Enabled=lb[3] 

im_pop.m_eq.PopMenu(This.PointerX(),This.PointerY())

end subroutine

public subroutine of_filter (string as_filter);//////////////////////////////////////////////////////////////
//
// Subroutine of_filter(string as_filter)
//
// Description: filter equipment list and map datawindows by status
//
// Argument: as_filter - filter
//
//////////////////////////////////////////////////////////////
string	ls[2], ls_eq[], ls2
int		li, li_cnt, li_status

li_cnt=f_ss2a(f_replace(dw_sys.Describe("datawindow.objects"),"~t",","),ls_eq)
if Upper(as_filter)="ALL" then
	for li=1 to li_cnt
		if Match(ls_eq[li],"^pem[0-9]+\_[0-9]+$") then
			if dw_sys.Describe("p_"+ls_eq[li]+".visible")="1" then CONTINUE

			ls[1]+=" p_"+ls_eq[li]+".visible=1"
			ls[2]+=" p_"+ls_eq[li]+".visible=1 "+ls_eq[li]+"_t.visible=1"
		end if
	next
	ib_filter=FALSE
else
	li_status=Integer(as_filter)
	for li=1 to li_cnt
		if Match(ls_eq[li],"^pem[0-9]+\_[0-9]+$") then
			ls2="'1~tif("+ls_eq[li]+"="+as_filter+",1,0)'"

			ls[1]+=" p_"+ls_eq[li]+".visible="+ls2
			ls[2]+=" p_"+ls_eq[li]+".visible="+ls2+" "+ls_eq[li]+"_t.visible="+ls2

			if dw_sys.GetItemNumber(1,ls_eq[li])=li_status then
			else
				is_selected=f_replace(is_selected,ls_eq[li]+",","")
				ls[1]+=" t_"+ls_eq[li]+".visible=0"
				ls[2]+=" t_"+ls_eq[li]+".visible=0"
			end if
		end if
	next
	ib_filter=TRUE
end if

choose case as_filter
	case "0"
		as_filter=" (Filter: Normal)"
	case "1"
		as_filter=" (Filter: Servicing/Attention)"
	case "2"
		as_filter=" (Filter: Malfunction/OOS)"
	case "3"
		as_filter=" (Filter: Security Alert)"
	case "4"
		as_filter=" (Filter: Disconnected)"
	case else
		as_filter=""
end choose

gb_sys.Text="PEM Operation Status"+as_filter

if ls[1]<>"" then
	dw_sys.Modify(Mid(ls[2],2))

	if ib_simple then of_arrange()

	of_showselected((is_selected<>""))
	of_sorteqlist()
	of_setmenustatus()

	if Not ib_working then
		ib_working=TRUE
		if of_reconn() then
			of_refresh_eq_dtl()
		end if
		ib_working=FALSE
	end if
end if

end subroutine

public subroutine of_download (string as_eq);int		li, li_loc, li_eq
string	ls

as_eq=Mid(as_eq,4)
as_eq=Left(as_eq,Len(as_eq) - 1)

li_loc=Integer(Mid(as_eq,Pos(as_eq,"_")+1))
li_eq=Integer(Left(as_eq,Pos(as_eq,"_") - 1))

li=gnv_syscon.ids_parm[1].Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,gnv_syscon.ids_parm[1].RowCount()+1)
if li>0 then
	ls=Trim(gnv_syscon.ids_parm[1].GetItemString(li,"ip"))
	if ls<>"" then
		ls="gficmd.exe -teqlog -lGFIPEMON -s"+ls

		as_eq=Trim(gnv_syscon.of_getappparm("PEM UID"))
		if as_eq<>"" then
			ls+=" -u"+as_eq

			as_eq=Trim(gnv_syscon.of_getappparm("PEM PWD"))
			if as_eq<>"" then ls+=" -p"+as_eq
		else
			ls+=" -uroot -p"+Lower("GFI")+Lower("Genfare")
		end if
		ls+=" -d/home/TVM/Demo"

		Run(ls)
	end if
end if

end subroutine

public subroutine of_selectall ();//////////////////////////////////////////////////////////////
//
// Subroutine of_selectall()
//
// Description: select all equipments
//
//////////////////////////////////////////////////////////////
string	ls_eq[], ls_selected, ls, ls2
int		li, li_cnt

li_cnt=f_ss2a(f_replace(dw_sys.Describe("datawindow.objects"),"~t",","),ls_eq)
for li=1 to li_cnt
	if Match(ls_eq[li],"^pem[0-9]+\_[0-9]+$") then
		if ib_filter then
			ls2=dw_sys.Describe("p_"+ls_eq[li]+".visible")
			if ls2="1" then
			elseif Pos(ls2,"~t")>0 then
				if dw_sys.Describe("evaluate('"+Mid(ls2,Pos(ls2,"~t")+1)+"',1)")="1" then
				else
					ls+=" t_"+ls_eq[li]+".visible=0"
					CONTINUE
				end if
			else
				ls+=" t_"+ls_eq[li]+".visible=0"
				CONTINUE
			end if
		end if

		ls_selected+=ls_eq[li]+","
		ls+=" t_"+ls_eq[li]+".x="+String(Long(dw_sys.Describe("p_"+ls_eq[li]+".x"))+11)+&
			 " t_"+ls_eq[li]+".y="+String(Long(dw_sys.Describe("p_"+ls_eq[li]+".y"))+3)+&
			 " t_"+ls_eq[li]+".visible=1"
	end if
next

is_selected=ls_selected
dw_sys.Modify(Mid(ls,2))

SetRedraw(TRUE)

of_sorteqlist()
of_setmenustatus()

if Not ib_working then
	ib_working=TRUE
	if of_reconn() then
		of_refresh_eq_dtl()
	end if
	ib_working=FALSE
end if

end subroutine

public subroutine of_relist ();dw_sys.SetRedraw(FALSE)

dw_sys.Reset()
if Not of_buildlistdw() then GOTO ERR

if ii_tID[4]>0 then inv_tooltip.of_deltool(dw_sys,ii_tID[4])
ii_tID[4]=inv_tooltip.of_addtool(dw_sys,"")

if Not ib_working then
	ib_working=TRUE
	if of_reconn() then
		of_seteqstatus()
		of_showselected(TRUE)
	end if
	ib_working=FALSE
end if

dw_sys.SetRedraw(TRUE)
RETURN

ERR:
dw_sys.SetRedraw(TRUE)
Post Close(This)

end subroutine

public function boolean of_isallowed (string as_item);string	ls
int		li

if f_isGFIAdmin() then RETURN TRUE

if Not gnv_syscon.of_ischecksecurity() then RETURN TRUE

li=gnv_syscon.of_reconnect_security()
if li<0 then RETURN FALSE

as_item=Lower(Trim(as_item))
if as_item<>"" then
	ls=gnv_syscon.of_getuserID()
else
	RETURN FALSE
end if

SELECT DISTINCT 0 INTO :gnv_syscon.SQLNRows FROM security_setting s, security_grp g, security_grp_usr u
WHERE application='gfirpt' AND item=:as_item AND active='Y' AND user_name=:ls AND s.group_name=g.group_name AND g.group_name=u.group_name USING gnv_syscon;
if gnv_syscon.SQLCode=0 then
	if li>0 then gnv_syscon.of_disconnect("DBA")
	RETURN TRUE
else
	if li>0 then gnv_syscon.of_disconnect("DBA")
	RETURN FALSE
end if

end function

on w_gfipemon.create
if this.MenuName = "m_gfiscc" then this.MenuID = create m_gfiscc
this.mdi_1=create mdi_1
this.dw_eq_cache=create dw_eq_cache
this.st_bar=create st_bar
this.dw_eq=create dw_eq
this.gb_toolbar=create gb_toolbar
this.dw_sum=create dw_sum
this.gb_sum=create gb_sum
this.gb_eq=create gb_eq
this.dw_sys=create dw_sys
this.gb_sys=create gb_sys
this.Control[]={this.mdi_1,&
this.dw_eq_cache,&
this.st_bar,&
this.dw_eq,&
this.gb_toolbar,&
this.dw_sum,&
this.gb_sum,&
this.gb_eq,&
this.dw_sys,&
this.gb_sys}
end on

on w_gfipemon.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.mdi_1)
destroy(this.dw_eq_cache)
destroy(this.st_bar)
destroy(this.dw_eq)
destroy(this.gb_toolbar)
destroy(this.dw_sum)
destroy(this.gb_sum)
destroy(this.gb_eq)
destroy(this.dw_sys)
destroy(this.gb_sys)
end on

event resize;boolean	lb
long		ll_w_sys

if sizetype=1 then RETURN				// minimized

ib_resizing=TRUE

newwidth=Max(newwidth,2999)			// set minimum size
newheight=Max(newheight,2500) - 24 - mdi_1.microhelpheight

SetRedraw(FALSE)

ll_w_sys=dw_sys.Width

gb_sys.Width=newwidth - gb_sys.X - gb_sum.X
dw_sys.Width=gb_sys.Width - (gb_sum.Width - dw_sum.Width)

gb_eq.Width=newwidth - 2*gb_eq.X
dw_eq.Width=gb_eq.Width - (gb_sum.Width - dw_sum.Width)

st_bar.Width=gb_eq.Width

lb=ib_hide_eq
if ib_hide_eq then						// equipment detail section is hidden (squeezed out of view)
	of_barmove(newheight+4)
else
	of_barmove(st_bar.Y)
end if
ib_hide_eq=lb
												// if list view width changes, rearrange equipments
if dw_sys.Width<>ll_w_sys then of_arrange()

SetRedraw(TRUE)

ib_resizing=FALSE

end event

event open;//////////////////////////////////////////////////////////////
//
// Description: the Genfare PEM status monitor allows users to
//					 monitor PEM status and remotely control
//					 the equipments
//
//////////////////////////////////////////////////////////////
string	ls, ls_parm[]
ulong		lul

Title=gnv_syscon.of_gettitle()+" [Version "+gnv_syscon.of_getversion()+"]"
if f_getlogin()<>"" then
	choose case Integer(Trim(gnv_syscon.of_getglobalparm("Show User")))
		case 0
		case 1
			Title+=" - "+gnv_syscon.of_getuserID()
		case 2
			Title+=" - "+Trim(gnv_syscon.of_getusername())
		case else
			Title+=" - "+Trim(gnv_syscon.of_getusername())+" ("+gnv_syscon.of_getuserID()+")"
	end choose
end if

is_key=gnv_syscon.of_getregkey2()+"\Custom\"+GetApplication().DisplayName

Resize(4594,3336)						// default size

f_win_restore(is_key,This)

if RegistryGet(is_key,"Bar",RegString!,ls)>0 and Long(ls)>0 then of_barmove(Long(ls))

if RegistryGet(is_key,"Initial View",RegString!,ls)>0 and Upper(Trim(ls))="SIMPLE" then ib_simple=TRUE

if Not of_buildlistdw() then GOTO ERR

f_init_menu(MenuID)

of_setpopmenu()						// dynamically create menu items for equipments
of_init_menu()

if RegistryGet(is_key,"Selected PEM",RegString!,ls)>0 and Trim(ls)<>"" then
	is_selected=Trim(ls)
	of_sorteqlist()
end if

of_setmenustatus()

ii_tID[1]=inv_tooltip.of_addtool(dw_eq,"")
ii_tID[2]=inv_tooltip.of_addtool(dw_sys,"")
inv_tooltip.of_setdelay(3,250)	// set initial delay to 250 ms
inv_tooltip.of_setdelay(2,10000)	// set duration to 10 seconds

if Not gnv_syscon.of_isallowed("Show Revenue") then ib_showrev=TRUE
of_showrev()

id_today[2]=Today()
gb_sum.Text="Transit Summary for "+String(id_today[2],f_getdateformat())
dw_sum.SetTransObject(SQLCA)
dw_sum.Retrieve(f_getaut(),2)

f_seteqdiskparm(dw_eq)

of_refresh_eq_dtl()

of_seteqstatus()

of_showselected(TRUE)

if RegistryGet(is_key,"GFIFMON",Regulong!,iul_fmon)>0 and iul_fmon>0 then
	if gnv_syscon.inv_shell.of_isprocrunning(iul_fmon) then
	else
		ls="Genfare File Monitor - "+gnv_syscon.of_getgfidir()+"log\"+String(Today(),"yyyymmdd")+".stl"
		lul=f_findwin(ls)
		if lul>0 then
			Send(lul,1024,1,0)
		else
			Post Event ue_menuevent("GFIFMON")
		end if
	end if
end if

if RegistryGet(is_key,"GFIEVMON",Regulong!,iul_evmon)>0 and iul_evmon>0 then
	if gnv_syscon.inv_shell.of_isprocrunning(iul_evmon) then
	else
		lul=f_findwin("Genfare Event Monitor")
		if lul>0 then
			Send(lul,1024,1,0)
		else
			Post Event ue_menuevent("GFIEVMON")
		end if
	end if
end if

if gnv_syscon.inv_shell.of_isprocrunning("gfialert") then
elseif RegistryGet(is_key,"Alert",RegString!,ls)>0 and f_istrue(ls) then
	Post Run("gfialert.exe")
end if

f_geteqdiskparm(ls_parm)
if ls_parm[1]<>"90%" or ls_parm[2]<>"95%" then is_disk_warn={ls_parm[1],ls_parm[2]}

Timer(1)

dw_sys.Show()

Post Show()

RETURN

ERR:
Post Close(This)

end event

event close;if IsValid(ids_sort) then DESTROY ids_sort
if IsValid(im_pop) then DESTROY im_pop

if ib_simple then
	RegistrySet(is_key,"Initial View",RegString!,"Simple")
else
	RegistryDelete(is_key,"Initial View")
end if

if is_selected<>"" then
	RegistrySet(is_key,"Selected PEM",RegString!,is_selected)
else
	RegistryDelete(is_key,"Selected PEM")
end if

if iul_fmon>0 then
	if gnv_syscon.inv_shell.of_isprocrunning(iul_fmon) then
		RegistrySet(is_key,"GFIFMON",Regulong!,iul_fmon)
	else
		RegistryDelete(is_key,"GFIFMON")
	end if
else
	RegistryDelete(is_key,"GFIFMON")
end if

if iul_evmon>0 then
	if gnv_syscon.inv_shell.of_isprocrunning(iul_evmon) then
		RegistrySet(is_key,"GFIEVMON",Regulong!,iul_evmon)
	else
		RegistryDelete(is_key,"GFIEVMON")
	end if
else
	RegistryDelete(is_key,"GFIEVMON")
end if

f_win_save(is_key,This)
RegistrySet(is_key,"Bar",RegString!,String(st_bar.Y))

if gnv_syscon.inv_shell.of_isprocrunning("gfialert") then
	RegistrySet(is_key,"Alert",RegString!,"Yes")
else
	RegistryDelete(is_key,"Alert")
end if

end event

event timer;long		ll
int		li, li_row
string	ls

if ib_working or (Not Visible) or ib_resizing then RETURN

ib_working=TRUE

id_today[2]=Today()

ll=CPU()
choose case ll - il_CPU3				// 3-second timer
	case is<0
		il_CPU3=ll
	case is>3000
		il_CPU3=ll

		if of_reconn() then
			if id_today[1]=id_today[2] then
			else
				gb_sum.Text="Transit Summary for "+String(id_today[2],f_getdateformat())
				id_today[1]=id_today[2]
			end if

			dw_sum.SetRedraw(FALSE)
			dw_sum.SetTransObject(SQLCA)
			dw_sum.Retrieve(f_getaut(),2)
			dw_sum.SetRedraw(TRUE)

			if ib_closing then GOTO DONE

			of_refresh_eq_dtl()
			if ib_closing then GOTO DONE

			gnv_syscon.ids_parm[1].SetTransObject(SQLCA)
			if gnv_syscon.ids_parm[1].Retrieve(f_getaut())>0 then
				of_seteqstatus()
			end if
			if ib_closing then GOTO DONE
		end if

end choose

choose case ll - il_CPU60				// 1-minute timer
	case is<0
		il_CPU60=ll
	case is>Long(60000)
		il_CPU60=ll
		if Not ib_closing then GarbageCollect()
end choose

DONE:
ib_working=FALSE

end event

event closequery;ib_closing=TRUE

Timer(0)										// stop timer event

Yield(); Yield(); Yield()

RETURN 0

end event

type mdi_1 from mdiclient within w_gfipemon
long BackColor=268435456
end type

type dw_eq_cache from datawindow within w_gfipemon
integer x = 1051
integer width = 105
integer height = 84
boolean enabled = false
string dataobject = "d_gfiscc_eq_dtl_cache"
borderstyle borderstyle = stylelowered!
end type

event constructor;Hide()

end event

event retrievestart;RETURN 2									// do not reset

end event

type st_bar from statictext within w_gfipemon
event mousemove pbm_mousemove
event lbuttondown pbm_lbuttondown
event lbuttonup pbm_lbuttonup
integer x = 41
integer y = 1420
integer width = 3305
integer height = 12
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
string pointer = "SizeNS!"
long textcolor = 134217749
long backcolor = 255
string text = "같같같같같같같같같같같같같같같"
alignment alignment = center!
boolean focusrectangle = false
end type

event mousemove;long	ll

if KeyDown(KeyLeftButton!) then
	ll=WorkSpaceHeight() - 24 - mdi_1.MicroHelpHeight + gb_toolbar.Y
	if Y<>Max(Min(Parent.PointerY(),ll+4),580) then
		ib_resizing=TRUE
		SetRedraw(FALSE)
		of_barmove(Parent.PointerY())
		SetRedraw(TRUE)
		ib_resizing=FALSE
	end if
end if

end event

event constructor;X=41
BackColor=Parent.BackColor
end event

type dw_eq from datawindow within w_gfipemon
event ue_mousemove pbm_dwnmousemove
integer x = 87
integer y = 1500
integer width = 3209
integer height = 988
integer taborder = 30
string dataobject = "d_gfiscc_pem_dtl"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event ue_mousemove;string	ls

ls=dwo.name
choose case ls
	case "p_back", "p_next"
		if ls<>is_tdwo[1] and ii_tID[1]>0 then
			inv_tooltip.il_height_adjustment=0
			inv_tooltip.of_settitle("")
			inv_tooltip.of_setdw(This,ii_tID[1],ls)
		end if
end choose

is_tdwo[1]=ls

end event

event constructor;string	ls

X=87

if gnv_syscon.of_isallowed("Show Revenue") then
	Modify("inv_trim1_t.alignment='2~tif(flags>=0,2,0)' inv_trim2_t.alignment='2~tif(flags>=0,2,0)' inv_trim3_t.alignment='1~tif(flags>=0,1,0)'")
else
	ls="gb_inv inv_mod_t inv_5_t inv_10_t inv_25_t inv_50_t inv_100_t inv_200_t inv_500_t inv_1000_t inv_2000_t inv_5000_t inv_10000_t inv_total_t2 inv_token_t inv_sp_t inv_unknown_t "+&
		"inv_hpr1_t hpr1_ct inv_total_hpr1 hpr1_token inv_bg1 inv_hpr2_t hpr2_ct inv_total_hpr2 hpr2_token "+&
		"inv_bst_t stkr_b1 stkr_b2 stkr_b5 stkr_b10 stkr_b20 stkr_b50 stkr_b100 inv_total_btkr inv_bg2 "+&
		"inv_cmt_t cmt_c5 cmt_c10 cmt_c25 cmt_c50 cmt_c100 inv_total_cmt cmt_token cmt_sp "+&
		"inv_cbx_t cbx_c5 cbx_c10 cbx_c25 cbx_c100 inv_total_cbx cbx_token cbx_sp inv_bg3 "+&
		"inv_bar1 inv_total_t inv_total_c5 inv_total_c10 inv_total_c25 inv_total_c50 inv_total_c100 inv_total_b2 inv_total_b5 inv_total_b10 inv_total_b20 inv_total_b50 inv_total_b100 inv_total inv_total_token inv_total_sp inv_bg4 "+&
		"inv_trim0_t inv_trim1_t inv_trim2_t inv_trim3_t inv_bg5 "
	if Modify("datawindow.detail.height=1716 "+RightTrim(f_replace(ls," ",".visible=0 ")))<>"" then f_log2("Hiding inventory section failed")
end if

end event

event rbuttondown;if RowCount()>0 then im_pop.m_status.PopMenu(Parent.PointerX(),Parent.PointerY())

end event

event clicked;choose case dwo.name
	case "p_back"
		if row>1 then Post ScrollToRow(row - 1)
	case "p_next"
		if row<RowCount() then Post ScrollToRow(row+1)
end choose

end event

type gb_toolbar from groupbox within w_gfipemon
integer y = 108
integer width = 8000
integer height = 12
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
end type

event constructor;Move(0,108)
Resize(8000,12)

end event

type dw_sum from datawindow within w_gfipemon
integer x = 87
integer y = 204
integer width = 837
integer height = 1164
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_gfiscc_sum"
borderstyle borderstyle = stylelowered!
end type

event constructor;Move(87,204)					// enforce position and size
Width=837

Modify("label.pointer='' total.pointer=''")

end event

event retrievestart;Reset()
ImportString("1~t0~r~n2~t0")	// header rows
RETURN 2								// do not reset again

end event

event doubleclicked;if row>0 then
	choose case dwo.name
		case "label", "total", "p_protect"
			if gnv_syscon.of_isallowed("Show Revenue") and Upper(GetItemString(row,"name"))="REV" then
				of_showrev()
			end if
	end choose
end if

end event

type gb_sum from groupbox within w_gfipemon
integer x = 41
integer y = 136
integer width = 933
integer height = 1280
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 8388608
long backcolor = 67108864
string text = "Transit Summary"
end type

event constructor;Move(41,136)					// enforce position and size
Width=933

end event

type gb_eq from groupbox within w_gfipemon
integer x = 41
integer y = 1432
integer width = 3305
integer height = 1104
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 8388608
long backcolor = 67108864
string text = "PEM Operation Detail"
end type

event constructor;X=41

end event

type dw_sys from datawindow within w_gfipemon
event ue_mousemove pbm_dwnmousemove
integer x = 1061
integer y = 204
integer width = 2235
integer height = 1164
integer taborder = 10
boolean bringtotop = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event ue_mousemove;string	ls

ls=dwo.name
if Match(ls,"^[pt]_pem") then ls=Mid(ls,3)

if Match(ls,"^pem") then
	if Right(ls,2)="_t" then ls=Left(ls,Len(ls) - 2)
	if ls<>is_tdwo[2] and ii_tID[2]>0 then
		inv_tooltip.il_height_adjustment=0
		of_settip(ls)
		inv_tooltip.of_setdw(This,ii_tID[2],"p_"+ls)
	end if

end if

is_tdwo[2]=ls

end event

event clicked;string	ls, ls_selected

SetRedraw(FALSE)

ls_selected=is_selected

ls=String(dwo.name)					// get clicked object; if clicked on "selected check", remove prefix
if Match(ls,"^[tp]_pem") then ls=Mid(ls,3)

if Match(ls,"^pem") then			// click an equipment
	if Right(ls,2)="_t" then ls=Left(ls,Len(ls) - 2)

	if KeyDown(KeyControl!) then	// control click - toggle the selection
		of_select(ls)
	else									// deselect all, and toggle the section for the clicked equipment
		of_showselected(FALSE)
		if Pos(is_selected,ls+",")>0 then
			is_selected=""
		else
			is_selected=""
			of_select(ls)
		end if
	end if
else										// if click anything other than equipments, deselect all
	of_showselected(FALSE)
	is_selected=""
end if

SetRedraw(TRUE)

if ls_selected<>is_selected then
	of_sorteqlist()
	of_setmenustatus()

	if Not ib_working then
		ib_working=TRUE
		if of_reconn() then
			of_refresh_eq_dtl()
		end if
		ib_working=FALSE
	end if
end if

end event

event rbuttondown;string	ls

ls=dwo.name
if Match(ls,"pem[0-9]+\_[0-9]+") then
	of_showeqmenu(f_replace(Mid(ls,Pos(ls,"pem")),"_t",""))
else
	if ib_simple then
		im_pop.m_list.m_listtype.Text="List by station"
	else
		im_pop.m_list.m_listtype.Text="Simple list"
	end if
	im_pop.m_list.PopMenu(Parent.PointerX(),Parent.PointerY())
end if

end event

event constructor;Move(1061,204)					// enforce position

end event

type gb_sys from groupbox within w_gfipemon
integer x = 1015
integer y = 136
integer width = 2331
integer height = 1280
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 8388608
long backcolor = 67108864
string text = "PEM Operation Status"
end type

event constructor;Move(1015,136)					// enforce position

end event

