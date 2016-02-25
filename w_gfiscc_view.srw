$PBExportHeader$w_gfiscc_view.srw
$PBExportComments$Condition view window
forward
global type w_gfiscc_view from window
end type
type st_bar from statictext within w_gfiscc_view
end type
type dw_ev from datawindow within w_gfiscc_view
end type
type dw_dtl from datawindow within w_gfiscc_view
end type
type dw_eq from datawindow within w_gfiscc_view
end type
type gb_eq from groupbox within w_gfiscc_view
end type
end forward

global type w_gfiscc_view from window
boolean visible = false
integer width = 2967
integer height = 1812
boolean titlebar = true
string title = "SPX Genfare System Control Center - Condition View"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
event ue_control pbm_custom01
event ue_menuevent ( string as_action )
st_bar st_bar
dw_ev dw_ev
dw_dtl dw_dtl
dw_eq dw_eq
gb_eq gb_eq
end type
global w_gfiscc_view w_gfiscc_view

type variables
PROTECTED:
boolean				ib_resizing, ib_closing, ib_working
m_gfiscc_popup		im_pop
string				is_key
datastore			ids_eq, ids_dtl, ids_ev
int					ii_loc, ii_eq, ii_rev, ii_limit=200
datetime				idt_ev
long					il_id

end variables

forward prototypes
public subroutine of_barmove (long al_y)
public function boolean of_init ()
public subroutine of_refresh_eq ()
public subroutine of_refresh_dtl ()
public subroutine of_refresh_ev ()
public subroutine of_download (string as_eq)
end prototypes

event ue_control;if wparam=1 then
	if lparam=1 then
		Close(This)
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

event ue_menuevent(string as_action);int		li
string	ls

as_action=Upper(Trim(as_action))
choose case as_action
	case "STATUSPRINT"
		f_printdw(dw_dtl)

	case else
		if Left(as_action,2)="EQ" then
			as_action=Mid(as_action,3)
			li=Pos(as_action,"TVM")
			if li>0 then
				ls=Mid(as_action,li)+","
				as_action=Left(as_action,li - 1)
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
		end if

end choose

end event

public subroutine of_barmove (long al_y);//////////////////////////////////////////////////////////////
//
// Subroutine of_barmove(long al_y)
//
// Description: bar move resizing
//
// Argument: al_y - bar Y
//
//////////////////////////////////////////////////////////////
long	ll[2]

ll={dw_eq.Y,dw_eq.Height}
al_y=Max(Min(al_y,ll[1]+ll[2]+8),ll[1] - 28)

dw_dtl.Height=Max(al_y - 8 - ll[1],0)
dw_ev.Y=al_y+28
dw_ev.Height=ll[1]+ll[2] - al_y - 28

st_bar.Y=al_y								// move the bar to position

end subroutine

public function boolean of_init ();string				ls_type, ls, ls_SQL, ls_f="fnbitwise"
ulong					lul
datawindowchild	ldwc

if gnv_syscon.of_getDBMS()="MSS" then ls_f="dbo."+ls_f

ls_SQL="SELECT eq.loc_n, eq.eq_n, eq_label FROM gfi_eq eq, gfi_eq_status es "+&
		 "WHERE eq.loc_n IN (SELECT code FROM gfi_lst WHERE type='EQ LOC' AND class="+String(f_getaut())+") "+&
		   "AND eq.eq_type=1 AND eq.loc_n=es.loc_n AND eq.eq_type=es.eq_type AND eq.eq_n=es.eq_n AND es.st_id=0 "+&
			"AND "+ls_f+"('&',flags,134217728)=0 AND "

choose case gnv_syscon.is_parm[1]
	case "RVS"
		ls="Need revenue service"
		ls_SQL+=ls_f+"('&',srv_flags,1)>0"
	case "INRVS"
		ls="In revenue service"
		ls_SQL+=ls_f+"('&',srv_flags,4)>0"
	case "MNT"
		ls="Need maintenance"
		ls_SQL+=ls_f+"('&',srv_flags,2)>0"
	case "INMNT"
		ls="In maintenance"
		ls_SQL+=ls_f+"('&',srv_flags,8)>0"
	case "INSVC"
		ls="In service"
		ls_SQL+=ls_f+"('&',srv_flags,16)>0"
	case "OOS"
		ls="Out of service"
		ls_SQL+=ls_f+"('&',flags,3)>0"
	case "SEC"
		ls="Security alert"
		ls_SQL+=ls_f+"('&',srv_flags,32)>0"
	case "DIS"
		ls="Disconnected"
		ls_SQL="SELECT loc_n, eq_n, eq_label FROM gfi_eq eq "+&
				 "WHERE loc_n IN (SELECT code FROM gfi_lst WHERE type='EQ LOC' AND class="+String(f_getaut())+") AND eq_type=1 "+&
					"AND NOT EXISTS(SELECT 0 FROM gfi_eq_status WHERE loc_n=eq.loc_n AND eq_type=1 AND eq_n=eq.eq_n AND st_id=0 "+&
					"AND "+ls_f+"('&',flags,134217728)=0)"
	case else
		MessageBox(gnv_syscon.of_gettitle(),"Internal error: no valid condition type was received by w_gfiscc_view window. "+gnv_syscon.is_suffix,StopSign!)
		Close(This)
		RETURN FALSE
end choose

dw_eq.SetTransObject(SQLCA)
if dw_eq.SetSQLSelect(ls_SQL)<0 then
	MessageBox(gnv_syscon.of_gettitle(),"Database error: dynamically replacing an SQL statement in w_gfiscc_view window failed. "+gnv_syscon.is_suffix,StopSign!)
	Close(This)
	RETURN FALSE
end if

ids_eq=CREATE datastore
if ids_eq.Create(SQLCA.SyntaxFromSQL(ls_SQL,"",ls_f))<0 then
	MessageBox(gnv_syscon.of_gettitle(),"Runtime error: creating datastore from the following SQL statement failed. "+gnv_syscon.is_suffix+"~r~n~r~n"+ls_SQL,StopSign!)
	Close(This)
	RETURN FALSE
end if
ids_eq.SetSort("#2 A")

ids_dtl=CREATE datastore
ids_dtl.DataObject="d_gfiscc_eq_dtl_cache"

ids_ev=CREATE datastore
ids_ev.DataObject="d_gfiscc_ev"

dw_eq.GetChild("loc_n",ldwc)
ldwc.SetTransObject(SQLCA)
ldwc.Retrieve(f_getaut())

Title="Genfare TVM Condition Viewer - "+ls
gb_eq.Text="TVM Condition: "+ls

is_key=gnv_syscon.of_getregkey2()+"\Custom\"+GetApplication().DisplayName+"\"+gnv_syscon.is_parm[1]

if gnv_syscon.of_isallowed("Show Revenue") then ii_rev=1

Resize(2930,1708)

f_win_restore(is_key,This)

if RegistryGet(is_key,"Bar",Regulong!,lul)>0 then of_barmove(lul)

if RegistryGet(gnv_syscon.of_getregkey2()+"\Custom\"+GetApplication().DisplayName,"EV Limit",Regulong!,lul)>0 and lul>0 and lul<=32767 then ii_limit=lul

im_pop=CREATE m_gfiscc_popup

f_seteqdiskparm(dw_dtl)

Timer(3)

Event Timer()

RETURN TRUE

end function

public subroutine of_refresh_eq ();long		ll
int		li, li_eq
string	ls

li=dw_eq.GetRow()
if li>0 then
	li_eq=dw_eq.GetItemNumber(li,"eq_n")
	ll=Long(dw_eq.Describe("datawindow.VerticalScrollPosition"))
end if

dw_eq.SetRedraw(FALSE)

ids_eq.SetTransObject(SQLCA)
if ids_eq.Retrieve()>0 then
	ls=ids_eq.Describe("datawindow.data")
	if ls=dw_eq.Describe("datawindow.data") then
	else
		dw_eq.Reset()
		dw_eq.ImportString(ls)

		if li_eq>0 then
			li=dw_eq.Find("eq_n="+String(li_eq),1,dw_eq.RowCount()+1)
			if li>0 then
				dw_eq.ScrollToRow(li)
				if ll>0 then dw_eq.Modify("datawindow.VerticalScrollPosition="+String(ll))
			else
				dw_eq.ScrollToRow(1)
			end if
		else
			dw_eq.ScrollToRow(1)
		end if
		dw_eq.SelectRow(0,FALSE)
		dw_eq.SelectRow(dw_eq.GetRow(),TRUE)
	end if
	ids_eq.Reset()

elseif dw_eq.RowCount()>0 then
	dw_eq.Reset()
end if

dw_eq.SetRedraw(TRUE)

if ib_closing then RETURN

of_refresh_dtl()
of_refresh_ev()

end subroutine

public subroutine of_refresh_dtl ();//////////////////////////////////////////////////////////////
//
// Subroutine of_refresh_dtl()
//
// Description: refresh TVM detail section
//
//////////////////////////////////////////////////////////////
int		li, li_loc, li_eq
long		ll[2]
string	ls

if ib_closing then RETURN

li=dw_eq.GetRow()
if li>0 then
	li_loc=dw_eq.GetItemNumber(li,"loc_n")
	li_eq=dw_eq.GetItemNumber(li,"eq_n")
else
	dw_dtl.Reset()
	RETURN
end if

if dw_dtl.RowCount()>0 then
	ll={Long(dw_dtl.Describe("datawindow.HorizontalScrollPosition")),&
		 Long(dw_dtl.Describe("datawindow.VerticalScrollPosition"))}
end if

dw_dtl.SetRedraw(FALSE)

ids_dtl.SetTransObject(SQLCA)
if ids_dtl.Retrieve(f_getaut(),li_loc,1,li_eq)>0 then
	if ib_closing then GOTO DONE

	ls=ids_dtl.Describe("datawindow.data")
	if ls=dw_dtl.Describe("datawindow.data") then
	else
		dw_dtl.Reset()
		dw_dtl.ImportString(ls)

		if ll[1]>0 then dw_dtl.Modify("datawindow.HorizontalScrollPosition="+String(ll[1]))
		if ll[2]>0 then dw_dtl.Modify("datawindow.VerticalScrollPosition="+String(ll[2]))
	end if
	ids_dtl.Reset()

elseif dw_dtl.RowCount()>0 then
	dw_dtl.Reset()
end if
DONE:
dw_dtl.SetRedraw(TRUE)

end subroutine

public subroutine of_refresh_ev ();//////////////////////////////////////////////////////////////
//
// Subroutine of_refresh_ev()
//
// Description: refresh event list section
//
//////////////////////////////////////////////////////////////
int	li, li_loc, li_eq
long	ll

if ib_closing then RETURN

li=dw_eq.GetRow()
if li>0 then
	li_loc=dw_eq.GetItemNumber(li,"loc_n")
	li_eq=dw_eq.GetItemNumber(li,"eq_n")
else
	ii_loc=0
	ii_eq=0
	dw_ev.Reset()
	RETURN
end if

dw_ev.SetRedraw(FALSE)

if li_loc=ii_loc and li_eq=ii_eq then
else
	ii_loc=li_loc
	ii_eq=li_eq
	il_id=0

	SELECT MAX(ts) INTO :idt_ev FROM vnd_ev WHERE loc_n=:li_loc AND eq_type=1 AND eq_n=:li_eq;
	if SQLCA.SQLCode=0 and idt_ev>DateTime(1900-01-01) then
		idt_ev=DateTime(Date(idt_ev))
	else
		idt_ev=DateTime(Today())
	end if
	dw_ev.Reset()

	if ib_closing then GOTO DONE
end if

ids_ev.SetTransObject(SQLCA)
ll=ids_ev.Retrieve(li_loc,li_eq,idt_ev,il_id,ii_rev)
if ll>0 then
	if ib_closing then GOTO DONE

	idt_ev=ids_ev.GetItemDateTime(ll,"ts")
	il_id=ids_ev.GetItemNumber(ll,"ev_id")

	dw_ev.ImportString(ids_ev.Describe("datawindow.data"))
	ll=dw_ev.RowCount()
	if ll>ii_limit then dw_ev.RowsDiscard(1,ll - ii_limit,Primary!)
	dw_ev.ScrollToRow(dw_ev.RowCount())
	ids_ev.Reset()
end if
DONE:
dw_ev.SetRedraw(TRUE)

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
		ls="gficmd.exe -teqlog -lGFISCC -s"+ls

		as_eq=Trim(gnv_syscon.of_getappparm("TVM UID"))
		if as_eq<>"" and as_eq<>"gfi" then ls+=" -u"+as_eq

		as_eq=Trim(gnv_syscon.of_getappparm("TVM PWD"))
		if as_eq<>"" and as_eq<>"gfi" then ls+=" -p"+as_eq

		Run(ls)
	end if
end if

end subroutine

on w_gfiscc_view.create
this.st_bar=create st_bar
this.dw_ev=create dw_ev
this.dw_dtl=create dw_dtl
this.dw_eq=create dw_eq
this.gb_eq=create gb_eq
this.Control[]={this.st_bar,&
this.dw_ev,&
this.dw_dtl,&
this.dw_eq,&
this.gb_eq}
end on

on w_gfiscc_view.destroy
destroy(this.st_bar)
destroy(this.dw_ev)
destroy(this.dw_dtl)
destroy(this.dw_eq)
destroy(this.gb_eq)
end on

event resize;long	ll

if sizetype=1 then RETURN				// minimized

ib_resizing=TRUE

newwidth=Max(newwidth,2*dw_eq.X+dw_eq.Width)
newheight=Max(newheight,0)

SetRedraw(FALSE)

gb_eq.Resize(newwidth - 2*gb_eq.X,Max(newheight - gb_eq.Y - 36,0))
dw_eq.Height=Max(gb_eq.Height - 116,0)

dw_dtl.Width=Max(newwidth - dw_dtl.X - dw_eq.X,1065)
dw_ev.Resize(dw_dtl.Width,dw_eq.Y+dw_eq.Height - dw_ev.Y)
st_bar.Width=dw_dtl.Width

ll=dw_ev.Width - Long(dw_ev.Describe("typ.x")) - 105
dw_ev.Modify("typ.width="+String(ll)+" typ_t.width="+String(ll))

SetRedraw(TRUE)

ib_resizing=FALSE

end event

event close;if IsValid(im_pop) then DESTROY im_pop
if IsValid(ids_eq) then DESTROY ids_eq
if IsValid(ids_dtl) then DESTROY ids_dtl
if IsValid(ids_ev) then DESTROY ids_ev

f_win_save(is_key,This)
RegistrySet(is_key,"Bar",Regulong!,st_bar.Y)

end event

event closequery;ib_closing=TRUE

Timer(0)										// stop timer event

Yield(); Yield(); Yield()

RETURN 0

end event

event open;if Not of_init() then RETURN

Post Show()

end event

event timer;if ib_working or ib_resizing then RETURN

ib_working=TRUE

of_refresh_eq()

ib_working=FALSE

end event

type st_bar from statictext within w_gfiscc_view
event mousemove pbm_mousemove
event lbuttondown pbm_lbuttondown
event lbuttonup pbm_lbuttonup
integer x = 1184
integer y = 708
integer width = 1641
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

event mousemove;if KeyDown(KeyLeftButton!) then
	ib_resizing=TRUE
	dw_ev.SetRedraw(FALSE)
	of_barmove(Parent.PointerY())
	dw_ev.ScrollToRow(dw_ev.RowCount())
	dw_ev.SetRedraw(TRUE)
	ib_resizing=FALSE
end if

end event

event constructor;BackColor=Parent.BackColor
end event

type dw_ev from datawindow within w_gfiscc_view
integer x = 1184
integer y = 736
integer width = 1641
integer height = 880
integer taborder = 30
string dataobject = "d_gfiscc_ev"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;int		li, li2
string	ls, ls2, ls_parm[]

DECLARE cur_evt CURSOR FOR SELECT code, name FROM gfi_lst WHERE type='EV TYPE';

OPEN cur_evt;
if SQLCA.SQLCode=0 then
	li2=1
	do
		FETCH cur_evt INTO :li, :ls;
		if SQLCA.SQLCode=0 then
			SetValue("type",li2,Trim(ls)+"~t"+String(li))
			li2++
		end if
	loop while SQLCA.SQLCode=0
		
	CLOSE cur_evt;
end if

DECLARE cur_mod CURSOR FOR SELECT code, name FROM gfi_lst WHERE type='MOD TYPE' AND class=0;

OPEN cur_mod;
if SQLCA.SQLCode=0 then
	li2=1
	do
		FETCH cur_mod INTO :li, :ls;
		if SQLCA.SQLCode=0 then
			SetValue("mod_type",li2,Trim(ls)+"~t"+String(li))
			li2++
		end if
	loop while SQLCA.SQLCode=0
		
	CLOSE cur_mod;
end if

li=gnv_syscon.of_reconnect_security()
if li>=0 then
	li2=1
	DECLARE cur_uid CURSOR FOR SELECT user_name, description FROM security_usr USING gnv_syscon;
	OPEN cur_uid;
	if gnv_syscon.SQLCode=0 then
		do
			FETCH cur_uid INTO :ls, :ls2;
			if gnv_syscon.SQLCode=0 then
				if Match(ls,"^[0-9]+$") then
					ls2=Trim(ls2)
					if ls2<>"" then
						SetValue("userid",li2,ls2+"~t"+ls)
						li2++
					end if
				end if
			end if
		loop while gnv_syscon.SQLCode=0

		CLOSE cur_uid;
	end if

	if li>0 then gnv_syscon.of_disconnect("DBA")
end if

f_geteqdiskparm(ls_parm)
if ls_parm[1]<>"90%" or ls_parm[2]<>"95%" then
	ls=Describe("typ.expression")
	ls=f_replace(f_replace(ls,"90%",ls_parm[1]),"95%",ls_parm[2])
	object.typ.expression=ls
end if

end event

type dw_dtl from datawindow within w_gfiscc_view
integer x = 1184
integer y = 92
integer width = 1641
integer height = 608
integer taborder = 20
string dataobject = "d_gfiscc_eq_dtl"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;string	ls

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

type dw_eq from datawindow within w_gfiscc_view
integer x = 87
integer y = 92
integer width = 1056
integer height = 1524
integer taborder = 10
string dataobject = "d_gfiscc_tvm"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;if currentrow>0 then
	ib_working=TRUE
	SelectRow(0,FALSE)
	SelectRow(currentrow,TRUE)
	Yield()

	SetPointer(HourGlass!)
	of_refresh_dtl()
	of_refresh_ev()
	ib_working=FALSE
end if

end event

event rbuttondown;string	ls
boolean	lb[3]
int		li_loc, li_eq, li

if row>0 and dwo.name="eq" then
	lb={gnv_syscon.of_isallowed("Poll"),gnv_syscon.of_isallowed("Control"),gnv_syscon.of_isallowed("Download")}
	if lb[1] or lb[2] or lb[3] then
		li_loc=GetItemNumber(row,"loc_n")
		li_eq=GetItemNumber(row,"eq_n")
	else
		RETURN
	end if

	ls="TVM"+String(li_eq)+"_"+String(li_loc)

	im_pop.m_eq.m_reset.Text="Reset TVM "+String(li_eq)+" Status"
	im_pop.m_eq.m_reset.Tag="EQRESET"+ls
	im_pop.m_eq.m_reset.MicroHelp="Reset TVM "+String(li_eq)+" status to normal"

	if lb[2] then
		SELECT 0 INTO :SQLCA.SQLNRows FROM gfi_eq_status
		WHERE loc_n=:li_loc AND eq_type=1 AND eq_n=:li_eq AND st_id=0;
		im_pop.m_eq.m_reset.Enabled=(SQLCA.SQLCode=0)
	else
		im_pop.m_eq.m_reset.Enabled=FALSE
	end if

	im_pop.m_eq.m_poll.Text="Poll TVM "+String(li_eq)
	im_pop.m_eq.m_poll.Tag="EQPOLL"+ls
	im_pop.m_eq.m_poll.MicroHelp="Poll TVM "+String(li_eq)+" for status and transactions"
	im_pop.m_eq.m_poll.Enabled=lb[1] 

	im_pop.m_eq.m_sendcmd.Text="Send Command to TVM "+String(li_eq)
	im_pop.m_eq.m_sendcmd.m_insvc.Tag="EQCMDINSVC"+ls
	im_pop.m_eq.m_sendcmd.m_insvc.MicroHelp="Set TVM "+String(li_eq)+" into service mode"
	im_pop.m_eq.m_sendcmd.m_oos.Tag="EQCMDOOS"+ls
	im_pop.m_eq.m_sendcmd.m_oos.MicroHelp="Take TVM "+String(li_eq)+" out of service"
	im_pop.m_eq.m_sendcmd.m_reboot.Tag="EQCMDREBOOT"+ls
	im_pop.m_eq.m_sendcmd.m_reboot.MicroHelp="Reboot TVM "+String(li_eq)
	im_pop.m_eq.m_sendcmd.m_shutdown.Tag="EQCMDSHUTDOWN"+ls
	im_pop.m_eq.m_sendcmd.m_shutdown.MicroHelp="Shutdown TVM "+String(li_eq)
	im_pop.m_eq.m_sendcmd.Enabled=lb[2]

	im_pop.m_eq.m_sendfile.Text="Send File to TVM "+String(li_eq)
	im_pop.m_eq.m_sendfile.m_cfg.Tag="EQFILECFG"+ls
	im_pop.m_eq.m_sendfile.m_cfg.MicroHelp="Send configuration settings to TVM "+String(li_eq)
	im_pop.m_eq.m_sendfile.m_snd.Tag="EQFILESND"+ls
	im_pop.m_eq.m_sendfile.m_snd.MicroHelp="Send sound file(s) to TVM "+String(li_eq)
	im_pop.m_eq.m_sendfile.m_all.Tag="EQFILEALL"+ls
	im_pop.m_eq.m_sendfile.m_all.MicroHelp="Send all configuration files to TVM "+String(li_eq)
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

	im_pop.m_eq.m_download.Text="Download log files from TVM "+String(li_eq)
	im_pop.m_eq.m_download.Tag="EQDOWNLOAD"+ls
	im_pop.m_eq.m_download.MicroHelp="Download log files from TVM "+String(li_eq)
	im_pop.m_eq.m_download.Enabled=lb[3] 

	im_pop.m_eq.PopMenu(This.PointerX(),This.PointerY())
end if

end event

type gb_eq from groupbox within w_gfiscc_view
integer x = 41
integer y = 24
integer width = 2843
integer height = 1644
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "TVM"
end type

