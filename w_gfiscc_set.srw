$PBExportHeader$w_gfiscc_set.srw
$PBExportComments$[GFISCC] Set equipment position window
forward
global type w_gfiscc_set from window
end type
type dw_map from datawindow within w_gfiscc_set
end type
end forward

global type w_gfiscc_set from window
integer width = 2798
integer height = 1752
boolean titlebar = true
string title = "Map TVM"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = popup!
string icon = "AppIcon!"
event ue_menuevent ( string as_action )
dw_map dw_map
end type
global w_gfiscc_set w_gfiscc_set

type variables
PROTECTED:
string				is_key, is_drag, is_dragicon, is_clicked, is_action[]={"",""}, is_selected
long					il_w, il_h, il_pos[2]	// in pixels
long					il_clicked_pos[2], il_keycnt, il_logo_h
datastore			ids_eq
m_gfiscc_popup		im_pop
boolean				ib_changed, ib_movemap, ib_nomap

n_tooltip			inv_tooltip					// tooltip object
int					ii_tID[1]					// tooltip ID
string				is_tdwo						// tooltip datawindow object (prevent repeated tooltip calls)

end variables

forward prototypes
public subroutine of_scroll (integer ai_step)
public function boolean of_buildmapdw ()
public subroutine of_scroll (long al_x, long al_y)
public subroutine of_setpopmenu ()
public subroutine of_find (string as_eq)
public subroutine of_saveaction (string as_action)
public subroutine of_showtip (string as_eq)
public subroutine of_select (string as_eq)
public subroutine of_showselected (boolean ab_show)
public subroutine of_dragdrop (boolean ab_accept)
public subroutine of_place (string as_eq, ref long al_x, ref long al_y)
protected function integer of_overlap (string as_eq, long al_x, long al_y)
public subroutine of_move (long al_x, long al_y)
public subroutine of_scroll ()
public subroutine of_undoall ()
public subroutine of_undo (integer ai_action)
public subroutine of_undo ()
public subroutine of_unmap ()
public function string of_getobj ()
end prototypes

event ue_menuevent(string as_action);as_action=Upper(Trim(as_action))
choose case as_action
	case "MAPHIDE"								// hide the map for 10 seconds
		dw_map.Modify("p_map.visible=0")
		Timer(10)

	case "MAPINFO"								// show map file information
		MessageBox(gnv_syscon.of_gettitle(),"Map file: "+gnv_syscon.is_parm[1]+&
					 "~r~n~r~nMap size: "+dw_map.Describe("p_map.width")+"x"+dw_map.Describe("p_map.height")+" (pixels)")

	case "MAPUNDO"								// undo last action; infinite depth
		dw_map.SetRedraw(FALSE)
		of_undo()
		dw_map.SetRedraw(TRUE)

	case "MAPUNDOALL"							// undo all action
		dw_map.SetRedraw(FALSE)
		of_undoall()
		dw_map.SetRedraw(TRUE)

	case "MAPPRINT"
		f_printdw(dw_map)

	case else
		if Match(as_action,"^MAPFIND") then
			of_find(Lower(Mid(as_action,8)))	// find a equipment by displaying its tooltip; if it's out of sight, scroll to it
		end if

end choose

end event

public subroutine of_scroll (integer ai_step);//////////////////////////////////////////////////////////////
//
// Subroutine of_scroll(int ai_step)
//
// Description: this function is exclusively called by the
//					 navigation control; it detects which arrow is
//					 clicked and scroll accordingly
//
//					 arrow sensive areas are relative to the navigation control
//
// Argument: ai_step - number of pixels to scroll
//
//////////////////////////////////////////////////////////////
long	ll[2]

ai_step=Abs(ai_step)
if ai_step>0 then
else
	RETURN
end if

ll={This.PointerX() - PixelsToUnits(5,XPixelsToUnits!),&
	 This.PointerY() - PixelsToUnits(5,YPixelsToUnits!)}
if ll[1]>=4 and ll[1]<4+78 then
	if ll[2]>=60 and ll[2]<60+56 then		// left arrow sensive area: 4,60 78x56
		of_scroll(ai_step,0)
	end if
elseif ll[1]>=119 and ll[1]<119+78 then
	if ll[2]>=60 and ll[2]<60+56 then		// right arrow sensive area: 119,60 78x56
		of_scroll( -ai_step,0)
	end if
elseif ll[1]>=68 and ll[1]<68+64 then
	if ll[2]>=4 and ll[2]<4+64 then			// up arrow sensive area: 68,4 64x64
		of_scroll(0,ai_step)
	elseif ll[2]>=108 and ll[2]<108+64 then// down arrow sensive area: 68,108 64x64
		of_scroll(0, -ai_step)
	end if
end if

end subroutine

public function boolean of_buildmapdw ();//////////////////////////////////////////////////////////////
//
// Function boolean of_buildmapdw()
//
// Description: construct map datawindow
//
// Return: TRUE - successful; FALSE - error
//
//////////////////////////////////////////////////////////////
string	ls, ls_eq
long		ll_size[], ll, ll_pos[]
int		li_row=1, li_col=1

ls=f_replace(f_getdwhdr(),"<TITLE>","Equipment Map")

ls+="table(column=(type=long updatewhereclause=no name=a dbname='a'))~r~ndata(null)~r~n"
															// set map
if gnv_syscon.is_parm[1]<>"" and f_getimageinfo(gnv_syscon.is_parm[1],ll_size) then
	ls+="bitmap(band=detail filename='"+gnv_syscon.is_parm[1]+"' x='0' y='0' width='"+String(ll_size[1])+"' height='"+String(ll_size[2])+"' border='0' name=p_map visible='1')~r~n"
else
	ib_nomap=TRUE
	ll_size={0,0}
end if
															// set logo
if gnv_syscon.is_parm[2]<>"" and f_getimageinfo(gnv_syscon.is_parm[2],ll_pos) then
	ls+="bitmap(band=detail filename='"+gnv_syscon.is_parm[2]+"' x='6' y='6' width='"+String(ll_pos[1])+"' height='"+String(ll_pos[2])+"' border='0' name=p_logo visible='1')~r~n"
	il_logo_h=ll_pos[2]
end if
															// set transit authority name
ls+="text(band=detail alignment='1' text=~""+f_replace(f_getauthority(),"~"","'")+"~" border='0' color='8388608' x='0' y='0' height='28' width='"+String(UnitsToPixels(dw_map.Width,XUnitsToPixels!) - 5)+"' html.valueishtml='0' name=t_aut visible='1' font.face='Arial Rounded MT Bold' font.height='-18' font.weight='700' font.family='2' font.pitch='2' font.charset='0' background.mode='1' background.color='553648127')~r~n"

for ll=ids_eq.RowCount() to 1 step -1
	ls_eq="tvm"+String(ids_eq.GetItemNumber(ll,"eq_n"))+"_"+String(ids_eq.GetItemNumber(ll,"loc_n"))

	ll_pos={ids_eq.GetItemNumber(ll,"map_x"),ids_eq.GetItemNumber(ll,"map_y")}
	if ll_pos[1]>0 or ll_pos[2]>0 then			// if already positioned, record the max size for setting up scrollable size later
		ls+="bitmap(band=detail filename='led_g.gif'"
	else													// list not yet positioned equipments
		ll_pos={54+(li_col - 1)*16,28+(li_row - 1)*16}
		if ll_pos[1]>ll_size[1] - 16 then
			ll_pos={54,28+li_row*16}
			li_row++
			li_col=2
		else
			li_col++
		end if
		ls+="bitmap(band=detail filename='led_w.gif'"
	end if
	ls+=" x='"+String(ll_pos[1])+"' y='"+String(ll_pos[2])+"' width='16' height='16' border='0'"+&
		 " name="+ls_eq+" tag='"+ids_eq.GetItemString(ll,"label")+"<rn><rn>(to place or reposition, drag to a new location on the map)' pointer='Arrow!' visible='1')~r~n"+&
		 " text(band=detail alignment='2' text='a' border='0' color='255' x='5' y='5' height='12' width='7' html.valueishtml='0' name=t_"+ls_eq+" pointer='Arrow!' visible='0' font.face='Webdings' font.height='-10' font.weight='400' font.family='1' font.pitch='2' font.charset='2' background.mode='1' background.color='553648127')~r~n"
															// if no map, give 50 pixels space
	if ib_nomap then ll_pos={ll_pos[1]+50,ll_pos[2]+50}
	ll_size={Max(ll_size[1],ll_pos[1]+16),Max(ll_size[2],ll_pos[2]+16)}
next

ls=f_replace(ls,"<DBH>",String(ll_size[2]))	// set detail band height, one row for this dw
if ll_size[1]<ll_size[2] then						// width < height: change to portrait
	ls=f_replace(ls,"print.orientation=1","print.orientation=2")
end if
ls+="bitmap(band=detail filename='navigate.gif' x='5' y='5' height='44' width='44' border='0' name=p_nav pointer='Arrow!' visible='1')~r~n"+&
	 "bitmap(band=detail filename='bin_empty.gif' tag='To remove a TVM from its mapped position, drop it here' x='5' y='5' height='46' width='38' border='0' name=p_bin pointer='Arrow!' visible='1')~r~n"

if dw_map.Create(ls+f_getdwftr(),ls)>0 then
	il_w=ll_size[1]
	il_h=ll_size[2]
	RETURN TRUE
else
	MessageBox(gnv_syscon.of_gettitle(),"Internal error: creating map datawindow failed. "+gnv_syscon.is_suffix+"~r~n~r~n"+ls,StopSign!)
end if

RETURN FALSE

end function

public subroutine of_scroll (long al_x, long al_y);//////////////////////////////////////////////////////////////
//
// Subroutine of_scroll(long al_x, long al_y)
//
// Description: scroll the map based on the input dispositions
//
// Arguments: al_x - number of pixels to scroll left (>0) or right (<0)
//				  al_y - number of pixels to scroll up (>0) or down (<0)
//
//////////////////////////////////////////////////////////////
long		ll[2]
string	ls[2]={"datawindow.horizontalscrollposition","datawindow.verticalscrollposition"}

dw_map.SetRedraw(FALSE)

if al_x>0 then							// scroll left (move the map right)
	ll[1]=Long(dw_map.Describe(ls[1]))
	if ll[1]>0 then dw_map.Modify(ls[1]+"="+String(Max(ll[1] - al_x,0)))

elseif al_x<0 then					// scroll right (move the map left)
	ll={Long(dw_map.Describe(ls[1])),Long(dw_map.Describe("datawindow.horizontalscrollmaximum"))}
	if ll[1]<ll[2] then dw_map.Modify(ls[1]+"="+String(Min(ll[1] - al_x,ll[2])))

end if

if al_y>0 then							// scroll up (move the map down)
	ll[1]=Long(dw_map.Describe(ls[2]))
	if ll[1]>0 then dw_map.Modify(ls[2]+"="+String(Max(ll[1] - al_y,0)))

elseif al_y<0 then					// scroll down (move the map up)
	ll={Long(dw_map.Describe(ls[2])),Long(dw_map.Describe("datawindow.verticalscrollmaximum"))}
	if ll[1]<ll[2] then dw_map.Modify(ls[2]+"="+String(Min(ll[1] - al_y,ll[2])))

end if

Event resize(0,WorkSpaceWidth(),WorkSpaceHeight())

dw_map.SetRedraw(TRUE)

end subroutine

public subroutine of_setpopmenu ();//////////////////////////////////////////////////////////////
//
// Subroutine of_setpopmenu()
//
// Description: dynamically create equipment menu items from
//					 m_gfiscc_popup_dynamic which has a script to
//					 trigger an action in the parent window
//
//////////////////////////////////////////////////////////////
menu	lm_find
long	ll, ll2
int	li, li_grp

im_pop=CREATE m_gfiscc_popup				// instantiate the popup menu
im_pop.m_map.m_selectall1.Visible=FALSE
im_pop.m_map.m_mapbar0.Visible=FALSE

// dynamically create the find equipment menu items (assume all equipments in the datastore are TVMs)
if ids_eq.RowCount()<=25 then				// 25 or less, do not divide into groups
	lm_find=im_pop.m_map.m_mapfind
	for ll=ids_eq.RowCount() to 1 step -1
		li++
		lm_find.Item[li]=CREATE m_gfiscc_popup_dynamic
		lm_find.Item[li].Text=ids_eq.GetItemString(ll,"label")
		lm_find.Item[li].Tag="MAPFINDTVM"+String(ids_eq.GetItemNumber(ll,"eq_n"))+"_"+String(ids_eq.GetItemNumber(ll,"loc_n"))
	next
else												// more than 25, group them so that each menu list is too long
	ll=ids_eq.RowCount()
	do
		ll2=Max(ll - 24,1)
		lm_find=im_pop.m_map.m_mapfind

		li_grp++
		lm_find.Item[li_grp]=CREATE m_gfiscc_popup_dynamic
		if ll=1 then							// 1 item remaining, do not group it
			lm_find.Item[li_grp].Text=ids_eq.GetItemString(ll,"label")
			lm_find.Item[li_grp].Tag="MAPFINDTVM"+String(ids_eq.GetItemNumber(ll,"eq_n"))+"_"+String(ids_eq.GetItemNumber(ll,"loc_n"))
		else
			lm_find.Item[li_grp].Text="TVM "+String(ids_eq.GetItemNumber(ll,"eq_n"))+" - "+String(ids_eq.GetItemNumber(ll2,"eq_n"))
			lm_find=lm_find.Item[li_grp]
			li=1
			for ll=ll to ll2 step -1
				lm_find.Item[li]=CREATE m_gfiscc_popup_dynamic
				lm_find.Item[li].Text=ids_eq.GetItemString(ll,"label")
				lm_find.Item[li].Tag="MAPFINDTVM"+String(ids_eq.GetItemNumber(ll,"eq_n"))+"_"+String(ids_eq.GetItemNumber(ll,"loc_n"))
				li++
			next
		end if
		ll=ll2 - 1
	loop while ll>0
end if

if ib_nomap then
	im_pop.m_map.m_maphide.Visible=FALSE
	im_pop.m_map.m_mapinfo.Visible=FALSE
end if

im_pop.m_map.m_mapbar2.Visible=FALSE
im_pop.m_map.m_filter.Visible=FALSE

end subroutine

public subroutine of_find (string as_eq);//////////////////////////////////////////////////////////////
//
// Subroutine of_find(string as_eq)
//
// Description: find an equipment by displaying its tooltip;
//					 scroll if the equipment icon is out of sight
//
// Argument: as_eq - equipment object name
//
//////////////////////////////////////////////////////////////
long		ll[2], ll_pos[2], ll_scroll[2], ll_dw[2]
blob{8}	lblb

ll={Long(dw_map.Describe(as_eq+".x")),Long(dw_map.Describe(as_eq+".y"))}
if dw_map.Describe(as_eq+".visible")="1" and (ll[1]>0 or ll[2]>0) then
	ll_pos={Long(dw_map.Describe("datawindow.horizontalscrollposition")),&
			  Long(dw_map.Describe("datawindow.verticalscrollposition"))}
	ll_dw={UnitsToPixels(dw_map.Width,XUnitsToPixels!),&
			 UnitsToPixels(dw_map.Height,YUnitsToPixels!)}

	if ll[1]<=ll_pos[1] then		// if the equipment is out of sight, scroll the map to expose it
		ll_scroll[1]=ll_pos[1] - ll[1]+1
	elseif ll[1]+16>ll_pos[1]+ll_dw[1] then
		ll_scroll[1]=ll_pos[1]+ll_dw[1] - ll[1] - 16
	end if

	if ll[2]<=ll_pos[2] then
		ll_scroll[2]=ll_pos[2] - ll[2]+1
	elseif ll[2]+16>ll_pos[2]+ll_dw[2] then
		ll_scroll[2]=ll_pos[2]+ll_dw[2] - ll[2] - 16
	end if

	if ll_scroll[1]=0 and ll_scroll[2]=0 then
	else
		of_scroll(ll_scroll[1],ll_scroll[2])
	end if
	of_showtip(as_eq)					// display its tooltip as a way for users to identify it
else										// if the equipment is not visible (removed), display a message
	MessageBox(gnv_syscon.of_gettitle(),Upper(Left(as_eq,3))+" "+Mid(as_eq,4)+" was not found on the map.")
end if

end subroutine

public subroutine of_saveaction (string as_action);//////////////////////////////////////////////////////////////
//
// Subroutine of_saveaction(string as_action)
//
// Description: save an action for undo with infinite depth
//
// Argument: as_action - action string
//					Remove: R<equipment object name>; e.g. Rtvm123
//					Move: M<equipment object name>,<original X>,<original y>,<original icon file name without extension>; e.g. Mtvm123,150,200,led_w
//
//////////////////////////////////////////////////////////////
int	li

as_action=Trim(as_action)
if as_action<>"" then
	for li=1 to UpperBound(is_action)	// find the next available slot
		if is_action[li]<>"" then CONTINUE
		EXIT
	next
	is_action[li]=as_action
	ib_changed=TRUE
end if

end subroutine

public subroutine of_showtip (string as_eq);//////////////////////////////////////////////////////////////
//
// Subroutine of_showtip(string as_eq)
//
// Description: display tooltip for the specified equipment
//
// Argument: as_eq - equipment object name such as tvm123
//
//////////////////////////////////////////////////////////////
long		ll[2]
blob{8}	lblb
											// get equipment's absolute map position
ll={Long(dw_map.Describe(as_eq+".x")),Long(dw_map.Describe(as_eq+".y"))}
											// relative to dw_map by subtracting scrolling factor; adjust by 8 is to point to the center of the equipment icon (16x16)
ll={ll[1]+8 - Long(dw_map.Describe("datawindow.horizontalscrollposition")),&
	 ll[2]+8 - Long(dw_map.Describe("datawindow.verticalscrollposition"))}

BlobEdit(lblb,1,ll[1])
BlobEdit(lblb,5,ll[2])
ClientToScreen(Handle(dw_map),lblb)// convert to screen coordinates
											// set cursor based on the screen coordinates
SetCursorPos(Long(BlobMid(lblb,1,4)),Long(BlobMid(lblb,5)))
											// show tooltip
inv_tooltip.of_setdw(dw_map,ii_tID[1],as_eq)

end subroutine

public subroutine of_select (string as_eq);//////////////////////////////////////////////////////////////
//
// Subroutine of_select(string as_eq)
//
// Description: toggle selection of the specified equipment
//
// Argument: as_eq - equipment object name such as tvm123
//
//////////////////////////////////////////////////////////////
as_eq=Trim(as_eq)
if as_eq<>"" then
	if Pos(is_selected,as_eq+",")>0 then	// selected, deselect it
		is_selected=f_replace(is_selected,as_eq+",","")
		dw_map.Modify("t_"+as_eq+".visible=0")
	elseif Left(dw_map.Describe(as_eq+".filename"),5)="led_w" then
	else												// not selected, select it
		is_selected+=as_eq+","
		dw_map.Modify("t_"+as_eq+".x="+String(Long(dw_map.Describe(as_eq+".x"))+5)+&
						 " t_"+as_eq+".y="+String(Long(dw_map.Describe(as_eq+".y")) - 1)+&
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

if is_selected<>"" then		// is_selected contains comma delimited equipments end with a comma such as tvm123,tvm124,tvm125,
	if ab_show then
		for li=f_ss2a(is_selected,ls_eq) to 1 step -1
			if ls_eq[li]<>"" then
				ls+=" t_"+ls_eq[li]+".x="+String(Long(dw_map.Describe(ls_eq[li]+".x"))+5)+&
					 " t_"+ls_eq[li]+".y="+String(Long(dw_map.Describe(ls_eq[li]+".y")) - 1)
				if Left(dw_map.Describe(ls_eq[li]+".filename"),5)="led_g" then
					ls+=" t_"+ls_eq[li]+".visible=1"
				else
					ls+=" t_"+ls_eq[li]+".visible=0"
				end if
			end if
		next
		dw_map.Modify(Mid(ls,2))
	else
		ls="t_"+f_replace(is_selected,",",".visible=0 t_")
		dw_map.Modify(Left(ls,Len(ls) - 3))
	end if
end if

end subroutine

public subroutine of_dragdrop (boolean ab_accept);//////////////////////////////////////////////////////////////
//
// Subroutine of_dragdrop(boolean ab_accept)
//
// Description: when a dragged equipment reaches its target
//					 position, this function handles the rest
//
// Argument: ab_accept - TRUE: accept drop; FALSE: cancel drop
//
//////////////////////////////////////////////////////////////
long		ll[2], ll_dis[2], ll_old[2], ll_row
string	ls_eq, ls_desc
int		li, li_loc, li_eq
												// current position (pointer is at the center of the 16x16 icon)
ll={UnitsToPixels(dw_map.PointerX(),XUnitsToPixels!)+&
	 Long(dw_map.Describe("datawindow.horizontalscrollposition")) - 8,&
	 UnitsToPixels(dw_map.PointerY(),YUnitsToPixels!)+&
	 Long(dw_map.Describe("datawindow.verticalscrollposition")) - 8}
												// cursor positions must be obtained before the messagebox
if ab_accept then
	ls_eq=f_replace(f_replace(f_replace(f_replace(is_selected,"tvm",""),",",", "),"_","-"),"-0,",",")
	ls_eq=Left(ls_eq,Len(ls_eq) - 2)+" (<TVM ID>-<Station ID>)"	// remove trailing comma

	if f_istrue(gnv_syscon.of_getappparm("Confirm")) then
		if MessageBox(gnv_syscon.of_gettitle(),"Do you want to move the following TVM(s) to the new location on the transit map?~r~n~r~n"+ls_eq,Question!,OKCancel!,1)=2 then GOTO CANCEL
	end if
else
CANCEL:
	dw_map.SetRedraw(FALSE)
	GOTO DONE
end if
	 											// get x,y disposition
ll_dis={ll[1] - Long(dw_map.Describe(is_drag+".x")),&
		  ll[2] - Long(dw_map.Describe(is_drag+".y"))}

dw_map.SetRedraw(FALSE)
do												// drop all selected equipments
	li=Pos(is_selected,",",li+1)
	if li=0 then EXIT

	ls_eq=Left(is_selected,li - 1)	// get next equipment object name
	if Pos(ls_eq,",")>0 then ls_eq=Mid(ls_eq,LastPos(ls_eq,",")+1)
												// get original position
	ll_old={Long(dw_map.Describe(ls_eq+".x")),Long(dw_map.Describe(ls_eq+".y"))}
												// get new position by applying the disposition to its starting position
	ll={ll_dis[1]+ll_old[1],ll_dis[2]+ll_old[2]}
												// move it to be within the boundary
	ll={Min(Max(ll[1],0),il_w - 16),Min(Max(ll[2],0),il_h - 16)}
	if ll[1]=0 and ll[2]=0 then ll={1,1}

	of_place(ls_eq,ll[1],ll[2])
												// equipment object name naming convention: tvm<TVM ID>_<Station ID>, e.g. tvm123_1
	li_loc=Integer(Mid(ls_eq,LastPos(ls_eq,"_")+1))
	li_eq=Integer(Mid(Left(ls_eq,Pos(ls_eq,"_") - 1),4))

	ll_row=ids_eq.Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,ids_eq.RowCount()+1)
	if ll_row>0 then						// update the datastore
		ls_desc=ids_eq.GetItemString(ll_row,"label")

		UPDATE gfi_eq SET map_x=:ll[1], map_y=:ll[2]
		WHERE loc_n=:li_loc AND eq_type=1 AND eq_n=:li_eq;
		if SQLCA.SQLCode<0 then
			if Left(is_dragicon,5)="led_w" then
				if gnv_syscon.ib_log_db then f_log("Placing "+ls_desc+" on the transit map failed~r~n"+SQLCA.SQLErrText)
			else
				if gnv_syscon.ib_log_db then f_log("Moving "+ls_desc+" from ("+String(ll_old[1])+","+String(ll_old[2])+") to ("+String(ll[1])+","+String(ll[2])+") on the transit map failed~r~n"+SQLCA.SQLErrText)
			end if
			ROLLBACK;
			CONTINUE							// if database error, skip this equipment
		else
			COMMIT;
			if Left(is_dragicon,5)="led_w" then
				if gnv_syscon.ib_log_info then f_log(ls_desc+" has been placed at ("+String(ll[1])+","+String(ll[2])+") on the transit map")
			else
				if gnv_syscon.ib_log_info then f_log(ls_desc+" has been moved from ("+String(ll_old[1])+","+String(ll_old[2])+") to ("+String(ll[1])+","+String(ll[2])+") on the transit map")
			end if
		end if
		ids_eq.SetItem(ll_row,"map_x",ll[1])
		ids_eq.SetItem(ll_row,"map_y",ll[2])
		ids_eq.SetItem(ll_row,"visible",1)
	end if
												// make it visible so that it is included in the overlap avoidance logic
	dw_map.Modify(ls_eq+".x="+String(ll[1])+" "+ls_eq+".y="+String(ll[2])+" "+ls_eq+".filename='led_g.gif' "+ls_eq+".visible=1")

	of_saveaction("M"+ls_eq+","+String(ll_old[1])+","+String(ll_old[2])+","+Left(is_dragicon,5))

loop while li>0

DONE:
ls_eq=f_replace(is_selected,",",".visible=1 ")
dw_map.Modify(RightTrim(ls_eq))
of_showselected(TRUE)
is_drag=""
dw_map.SetRedraw(TRUE)

end subroutine

public subroutine of_place (string as_eq, ref long al_x, ref long al_y);//////////////////////////////////////////////////////////////
//
// Subroutine of_place(string as_eq, long al_x, long al_y)
//
// Description: position an equipment without overlapping
//
// Arguments: as_eq - equipment object name to place
//				  al_x, al_y - position
//
//////////////////////////////////////////////////////////////
long	ll[2], ll_new[2]
int	li, li_dir[4,2], li2

li=of_overlap(as_eq,al_x,al_y)
if li<=0 then RETURN

// search sequence (4 quadrants, clockwise)
li_dir[1,1]=Sign(al_x - Long(dw_map.Describe(as_eq+".x")))
li_dir[1,2]=Sign(al_y - Long(dw_map.Describe(as_eq+".y")))
if li_dir[1,1]=0 then
	if li_dir[1,2]>0 then
		li_dir[2,2]= -1
		li_dir[3,2]= -1
		li_dir[4,2]=1
	elseif li_dir[1,2]<0 then
		li_dir[2,2]= -1
		li_dir[3,2]=1
		li_dir[4,2]=1
	end if
elseif li_dir[1,1]>0 then
	li_dir[2,1]=1
	li_dir[3,1]= -1
	li_dir[4,1]= -1
	if li_dir[1,2]>0 then
		li_dir[2,2]= -1
		li_dir[3,2]= -1
		li_dir[4,2]=1
	elseif li_dir[1,2]<0 then
		li_dir[2,2]= -1
		li_dir[3,2]=1
		li_dir[4,2]=1
	end if
else
	li_dir[2,1]= -1
	li_dir[3,1]=1
	li_dir[4,1]=1
	if li_dir[1,2]>=0 then
		li_dir[2,2]=1
		li_dir[3,2]= -1
		li_dir[4,2]= -1
	elseif li_dir[1,2]<0 then
		li_dir[2,2]=1
		li_dir[3,2]=1
		li_dir[4,2]= -1
	end if
end if

ll={ids_eq.GetItemNumber(li,"map_x"),ids_eq.GetItemNumber(li,"map_y")}
for li=1 to 10								// try 10 rings	
	for li2=1 to 4
												// try before
		ll_new={ll[1] - li_dir[li2,1]*15*li,ll[2] - li_dir[li2,2]*15*li}
		if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE

		if Abs(ll[1] - al_x)>=Abs(ll[2] - al_y) then	// horizontal first
			if li_dir[li2,1]<>0 then	// try horizontal before
				ll_new={ll[1] - li_dir[li2,1]*15*li,ll[2]}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
			if li_dir[li2,2]<>0 then	// try vertical before
				ll_new={ll[1],ll[2] - li_dir[li2,2]*15*li}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
		else									// vertical first
			if li_dir[li2,2]<>0 then	// try vertical before
				ll_new={ll[1],ll[2] - li_dir[li2,2]*15*li}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
			if li_dir[li2,1]<>0 then	// try horizontal before
				ll_new={ll[1] - li_dir[li2,1]*15*li,ll[2]}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
		end if
												// try after
		ll_new={ll[1]+li_dir[li2,1]*15*li,ll[2]+li_dir[li2,2]*15*li}
		if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE

		if Abs(ll[1] - al_x)>=Abs(ll[2] - al_y) then	// horizontal first
			if li_dir[li2,1]<>0 then	// try horizontal after
				ll_new={ll[1]+li_dir[li2,1]*15*li,ll[2]}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
			if li_dir[li2,2]<>0 then	// try vertical after
				ll_new={ll[1],ll[2]+li_dir[li2,2]*15*li}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
		else									// vertical first
			if li_dir[li2,2]<>0 then	// try vertical after
				ll_new={ll[1],ll[2]+li_dir[li2,2]*15*li}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
			if li_dir[li2,1]<>0 then	// try horizontal after
				ll_new={ll[1]+li_dir[li2,1]*15*li,ll[2]}
				if of_overlap(as_eq,ll_new[1],ll_new[2])=0 then GOTO DONE
			end if
		end if
	next
next

if gnv_syscon.ib_log_warn then f_log("Internal issue: after 10 rings of probing, no open slot; a unusual high concentration of TVMs must be encountered")

DONE:
al_x=ll_new[1]
al_y=ll_new[2]

end subroutine

protected function integer of_overlap (string as_eq, long al_x, long al_y);//////////////////////////////////////////////////////////////
//
// Function int of_overlap(string as_eq, long al_x, long al_y)
//
// Description: check if there's overlap
//
// Arguments: as_eq - equipment object name, must be in the format of tvmNNN...
//				  al_x, al_y - position
//
// Return: row number if overlap or 0 if no overlap
//
//////////////////////////////////////////////////////////////
string	ls

if al_x=0 and al_y=0 then RETURN -1

if al_x<0 or al_y<0 then RETURN -1

if al_x+16>il_w or al_y+16>il_h then RETURN -1

ls="(map_x>0 or map_y>0) and string(eq_n)+'_'+string(loc_n)<>'"+Mid(as_eq,4)+"' and "+&
	"(visible=1 or (visible=0 and pos('"+is_selected+"','tvm'+string(eq_n)+'_'+string(loc_n)+',')=0)) and "+&
	"abs("+String(al_x)+" - map_x)<15 and "+&
	"abs("+String(al_y)+" - map_y)<15"

RETURN ids_eq.Find(ls,1,ids_eq.RowCount()+1)

end function

public subroutine of_move (long al_x, long al_y);//////////////////////////////////////////////////////////////
//
// Subroutine of_move(long al_x, long al_y)
//
// Description: move selected equipments by the specified disposition
//
// Arguments: al_x, al_y - disposition in pixels (+: right/down; -: left/up)
//
//////////////////////////////////////////////////////////////
int		li
string	ls_eq[], ls, ls_find
long		ll[2], ll_x[2]={999999999,0}, ll_y[2]={999999999,0}

if (al_x=0 and al_y=0) or is_selected="" then RETURN

for li=f_ss2a(is_selected,ls_eq) to 1 step -1
	if ls_eq[li]<>"" then
		ll={Long(dw_map.Describe(ls_eq[li]+".x"))+al_x,&
			 Long(dw_map.Describe(ls_eq[li]+".y"))+al_y}
		if ll[1]=0 and ll[2]=0 then
		elseif ll[1]<0 or ll[2]<0 then
		elseif ll[1]+16>il_w or ll[2]+16>il_h then
		else
			ls_find="(map_x>0 or map_y>0) and string(eq_n)+'_'+string(loc_n)<>'"+Mid(ls_eq[li],4)+"' and "+&
					  "pos('"+is_selected+"','tvm'+string(eq_n)+'_'+string(loc_n)+',')=0 and "+&
					  "abs("+String(ll[1])+" - map_x)<15 and "+&
					  "abs("+String(ll[2])+" - map_y)<15"
			if ids_eq.Find(ls_find,1,ids_eq.RowCount()+1)=0 then
				ls+=" "+ls_eq[li]+".x="+String(ll[1])+" t_"+ls_eq[li]+".x="+String(ll[1]+5)+&
					 " "+ls_eq[li]+".y="+String(ll[2])+" t_"+ls_eq[li]+".y="+String(ll[2] - 1)
				ll_x={Min(ll_x[1],ll[1]),Max(ll_x[2],ll[1])}
				ll_y={Min(ll_y[1],ll[2]),Max(ll_y[2],ll[2])}
				CONTINUE
			end if
		end if
		ls=""
		EXIT
	end if
next

if ls<>"" then
	dw_map.SetRedraw(FALSE)

	dw_map.Modify(Mid(ls,2))
												// when moving close to borders, scroll
	ll={Long(dw_map.Describe("datawindow.horizontalscrollposition")),&
		 Long(dw_map.Describe("datawindow.verticalscrollposition"))}
	ll_x={ll_x[1] - ll[1],ll_x[2] - ll[1]}
	ll_y={ll_y[1] - ll[2],ll_y[2] - ll[2]}
	if ll_x[1]<4 and ll[1]>0 then		// scroll left
		ll[1]=25
	elseif ll_x[2]>=UnitsToPixels(dw_map.Width,XUnitsToPixels!) - 4 then		// scroll right
		ll[1]= -25
	else
		ll[1]=0
	end if

	if ll_y[1]<4 then						// scroll up
		ll[2]=25
	elseif ll_y[2]>=UnitsToPixels(dw_map.Height,YUnitsToPixels!) - 4 then	// scroll down
		ll[2]= -25
	else
		ll[2]=0
	end if

	if ll[1]=0 and ll[2]=0 then
	else
		of_scroll(ll[1],ll[2])
	end if

	dw_map.SetRedraw(TRUE)
end if

end subroutine

public subroutine of_scroll ();//////////////////////////////////////////////////////////////
//
// Subroutine of_scroll()
//
// Description: scroll 25 pixels when close to datawindow border
//
//////////////////////////////////////////////////////////////
long	ll[2]
										// when moving close to borders, scroll
ll={UnitsToPixels(dw_map.PointerX(),XUnitsToPixels!),UnitsToPixels(dw_map.PointerY(),YUnitsToPixels!)}
if ll[1]<4 then					// scroll left
	ll[1]=25
elseif ll[1]>=UnitsToPixels(dw_map.Width,XUnitsToPixels!) - 4 then		// scroll right
	ll[1]= -25
else
	ll[1]=0
end if

if ll[2]<4 then					// scroll up
	ll[2]=25
elseif ll[2]>=UnitsToPixels(dw_map.Height,YUnitsToPixels!) - 4 then	// scroll down
	ll[2]= -25
else
	ll[2]=0
end if

if ll[1]=0 and ll[2]=0 then
else
	of_scroll(ll[1],ll[2])
end if

end subroutine

public subroutine of_undoall ();//////////////////////////////////////////////////////////////
//
// Subroutine of_undoall()
//
// Description: undo all actions
//
//////////////////////////////////////////////////////////////
int	li

for li=UpperBound(is_action) to 1 step -1
	if is_action[li]<>"" then
		of_undo(li)
		if is_action[li]<>"" then EXIT
	end if
next

end subroutine

public subroutine of_undo (integer ai_action);//////////////////////////////////////////////////////////////
//
// Subroutine of_undo(int ai_action)
//
// Description: undo last action with unlimited depth
//
// Argument: ai_action - action index
//
//////////////////////////////////////////////////////////////
string	ls_pos[], ls_desc
long		ll, ll_pos[2]
int		li_loc, li_eq

if ai_action>0 then
else
	RETURN
end if

choose case Upper(Left(is_action[ai_action],1))
	case "R"								// remove: R<equipment object name>; e.g. Rtvm123_1
		is_action[ai_action]=Mid(is_action[ai_action],2)	// object name; e.g. tvm123_1
											// equipment object name naming convention: tvm<TVM ID>_<Station ID>, e.g. tvm123_1
		li_loc=Integer(Mid(is_action[ai_action],LastPos(is_action[ai_action],"_")+1))
		li_eq=Integer(Mid(Left(is_action[ai_action],Pos(is_action[ai_action],"_") - 1),4))

		ll=ids_eq.Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,ids_eq.RowCount()+1)
		if ll>0 then
			ls_desc=ids_eq.GetItemString(ll,"label")
		else
			GOTO ERR
		end if

		ll_pos={Long(dw_map.Describe(is_action[ai_action]+".x")),&
				  Long(dw_map.Describe(is_action[ai_action]+".y"))}
		of_place(is_action[ai_action],ll_pos[1],ll_pos[2])

		UPDATE gfi_eq SET map_x=:ll_pos[1], map_y=:ll_pos[2]
		WHERE loc_n=:li_loc AND eq_type=1 AND eq_n=:li_eq;
		if SQLCA.SQLCode<0 then
			Post MessageBox(gnv_syscon.of_gettitle(),"Restoring "+ls_desc+" failed.~r~n~r~n"+SQLCA.SQLErrText,StopSign!)
			ROLLBACK;
			RETURN
		else
			COMMIT;
			if gnv_syscon.ib_log_info then f_log(ls_desc+" has been restored")
		end if

		ids_eq.SetItem(ll,"map_x",ll_pos[1])
		ids_eq.SetItem(ll,"map_y",ll_pos[2])
		ids_eq.SetItem(ll,"visible",1)
		dw_map.Modify(is_action[ai_action]+".visible=1")

		is_action[ai_action]=""

		for ai_action=ai_action - 1 to 1 step -1
			if Upper(Left(is_action[ai_action],1))="R" then EXIT
		next
		if ai_action=0 then dw_map.Modify("p_bin.filename='bin_empty.gif'")

	case "M"								// move: M<equipment object name>,<original X>,<original y>,<original icon file name without extension>; e.g. Mtvm123,150,200,led_w
		if f_ss2a(Mid(is_action[ai_action],2),ls_pos)=4 then
											// equipment object name naming convention: tvm<TVM ID>_<Station ID>, e.g. tvm123_1
			li_loc=Integer(Mid(ls_pos[1],LastPos(ls_pos[1],"_")+1))
			li_eq=Integer(Mid(Left(ls_pos[1],Pos(ls_pos[1],"_") - 1),4))

			ll=ids_eq.Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,ids_eq.RowCount()+1)
			if ll>0 then
				ls_desc=ids_eq.GetItemString(ll,"label")
			else
				GOTO ERR
			end if

			ll_pos={Long(ls_pos[2]),Long(ls_pos[3])}
			of_place(ls_pos[1],ll_pos[1],ll_pos[2])
			ls_pos[2]=String(ll_pos[1])
			ls_pos[3]=String(ll_pos[2])
		else
			GOTO ERR
		end if

		UPDATE gfi_eq SET map_x=:ll_pos[1], map_y=:ll_pos[2]
		WHERE loc_n=:li_loc AND eq_type=1 AND eq_n=:li_eq;
		if SQLCA.SQLCode<0 then
			Post MessageBox(gnv_syscon.of_gettitle(),"Moving "+ls_desc+" back to its prior position at ("+ls_pos[2]+","+ls_pos[3]+") failed.~r~n~r~n"+SQLCA.SQLErrText,StopSign!)
			ROLLBACK;
			RETURN
		else
			COMMIT;
			if gnv_syscon.ib_log_info then f_log(ls_desc+" has been moved back to its prior position at ("+ls_pos[2]+","+ls_pos[3]+")")
		end if

		ids_eq.SetItem(ll,"map_x",ll_pos[1])
		ids_eq.SetItem(ll,"map_y",ll_pos[2])
		ids_eq.SetItem(ll,"visible",1)

		dw_map.Modify(ls_pos[1]+".x="+ls_pos[2]+" "+ls_pos[1]+".y="+ls_pos[3]+" "+ls_pos[1]+".filename='"+ls_pos[4]+".gif' "+ls_pos[1]+".visible=1")
		of_showselected(TRUE)

		is_action[ai_action]=""

	case else
ERR:	Post MessageBox(gnv_syscon.of_gettitle(),"Internal error: the action syntax, "+is_action[ai_action]+", is invalid. "+gnv_syscon.is_suffix,StopSign!)
		is_action[ai_action]=""

end choose

end subroutine

public subroutine of_undo ();//////////////////////////////////////////////////////////////
//
// Subroutine of_undo()
//
// Description: undo last action
//
//////////////////////////////////////////////////////////////
int	li

for li=UpperBound(is_action) to 1 step -1
	if is_action[li]<>"" then
		of_undo(li)
		EXIT
	end if
next

end subroutine

public subroutine of_unmap ();//////////////////////////////////////////////////////////////
//
// Subroutine of_unmap()
//
// Description: remove a mapped object
//
//////////////////////////////////////////////////////////////
string	ls_desc, ls_pos
long		ll
int		li_loc, li_eq
														// equipment object naming convention is <type><ID>_<Station>; <type> can be tvm, pem, etc.; <ID> is a numeric number
li_loc=Integer(Mid(is_drag,LastPos(is_drag,"_")+1))
li_eq=Integer(Mid(Left(is_drag,Pos(is_drag,"_") - 1),4))

ll=ids_eq.Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,ids_eq.RowCount()+1)
if ll>0 then
	ls_desc=ids_eq.GetItemString(ll,"label")
else
	Post MessageBox(gnv_syscon.of_gettitle(),"Internal error: the equipment to remove, "+is_drag+", does not exist. "+gnv_syscon.is_suffix,StopSign!)
	dw_map.Modify(is_drag+".visible=1")
	GOTO DONE
end if

if Left(is_dragicon,5)="led_g" then			// confirm for already placed equipments
	if f_istrue(gnv_syscon.of_getappparm("Confirm")) then
		if MessageBox(gnv_syscon.of_gettitle(),"Do you want to remove "+ls_desc+" from its mapped position?",Question!,YesNo!,2)=2 then
			dw_map.Modify(is_drag+".visible=1")
			GOTO DONE								// cancel removal
		end if
	end if
else													// not placed equipment cannot be removed
	dw_map.Modify(is_drag+".visible=1")
	GOTO DONE
end if

ls_pos="("+dw_map.Describe(is_drag+".x")+","+dw_map.Describe(is_drag+".y")+")"

UPDATE gfi_eq SET map_x=0, map_y=0
WHERE loc_n=:li_loc AND eq_type=1 AND eq_n=:li_eq;
if SQLCA.SQLCode<0 then
	Post MessageBox(gnv_syscon.of_gettitle(),"Removing "+ls_desc+" from its mapped position at "+ls_pos+" failed.~r~n~r~n"+SQLCA.SQLErrText,StopSign!)
	ROLLBACK;
	dw_map.Modify(is_drag+".visible=1")
	GOTO DONE										// cancel removal when a database error occurs
else
	COMMIT;
	if gnv_syscon.ib_log_info then f_log(ls_desc+" has been removed from its mapped position at "+ls_pos)
end if

ids_eq.SetItem(ll,"map_x",0)
ids_eq.SetItem(ll,"map_y",0)
ids_eq.SetItem(ll,"visible",0)
														// get the removed one off the selected list
is_selected=f_replace(is_selected,is_drag+",","")
dw_map.Modify("p_bin.filename='bin_full.gif")

of_saveaction("R"+is_drag)						// record the action for undo

DONE:
of_dragdrop(FALSE)

end subroutine

public function string of_getobj ();//////////////////////////////////////////////////////////////
//
// Function string of_getobj()
//
// Description: get object name under mouse pointer
//
// Return: object name under mouse pointer or empty if not of the interested objects
//
//////////////////////////////////////////////////////////////
long	ll[2], ll_obj[4]

ll={UnitsToPixels(dw_map.PointerX(),XUnitsToPixels!)+&
	 Long(dw_map.Describe("datawindow.horizontalscrollposition")),&
	 UnitsToPixels(dw_map.PointerY(),YUnitsToPixels!)+&
	 Long(dw_map.Describe("datawindow.verticalscrollposition"))}

ll_obj={Long(dw_map.Describe("p_nav.x")),Long(dw_map.Describe("p_nav.y")),&
		  Long(dw_map.Describe("p_nav.width")),Long(dw_map.Describe("p_nav.height"))}
if ll[1] - ll_obj[1]>0 and ll[1] - ll_obj[1]<ll_obj[3] and &
	ll[2] - ll_obj[2]>0 and ll[2] - ll_obj[2]<ll_obj[4] then RETURN "p_nav"

ll_obj={Long(dw_map.Describe("p_bin.x")),Long(dw_map.Describe("p_bin.y")),&
		  Long(dw_map.Describe("p_bin.width")),Long(dw_map.Describe("p_bin.height"))}
if ll[1] - ll_obj[1]>0 and ll[1] - ll_obj[1]<ll_obj[3] and &
	ll[2] - ll_obj[2]>0 and ll[2] - ll_obj[2]<ll_obj[4] then RETURN "p_bin"

RETURN ""

end function

on w_gfiscc_set.create
this.dw_map=create dw_map
this.Control[]={this.dw_map}
end on

on w_gfiscc_set.destroy
destroy(this.dw_map)
end on

event resize;string	ls
long		ll[2], ll_dw[2]

if sizetype=1 then RETURN				// minimized

dw_map.Resize(Max(newwidth,2002),Max(newheight,1500))

ll={Max(Long(dw_map.Describe("datawindow.horizontalscrollposition")),0),&
	 Max(Long(dw_map.Describe("datawindow.verticalscrollposition")),0)}

ll_dw={UnitsToPixels(dw_map.Width,XUnitsToPixels!),&
		 UnitsToPixels(dw_map.Height,YUnitsToPixels!)}

ls="p_nav.x="+String(ll[1]+5)+" p_nav.y="+String(ll[2]+5)+&
	" datawindow.detail.height="+String(ll[2]+ll_dw[2])+&
	" p_bin.x="+String(ll[1]+ll_dw[1] - 5 - 38)+" p_bin.y="+String(ll[2]+ll_dw[2] - 5 - 46)+&
	" t_aut.width="+String(ll[1]+ll_dw[1] - 5)+" t_aut.y="+String(ll[2])

if il_logo_h>0 then
	ls+=" p_logo.x="+String(ll[1]+6)+" p_logo.y="+String(ll[2]+ll_dw[2] - 6 - il_logo_h)
end if

dw_map.Modify(ls)

// p_bin: 38x46 in pixels

end event

event open;//////////////////////////////////////////////////////////////
//
// Description: this window allows users to place equipments
//					 to their positions on a transit map using 
//					 drag and drop; if the map is changed, most
//					 likely, the equipments have to be remapped.
//
//////////////////////////////////////////////////////////////
Title=gnv_syscon.of_gettitle()+" [Version "+gnv_syscon.of_getversion()+"] - Map TVM"

is_key=gnv_syscon.of_getregkey2()+"\Custom\"+GetApplication().DisplayName+"\MapEQ"

Resize(3502,2500)						// default size

f_win_restore(is_key,This)			// restore window size and position

ids_eq=gnv_syscon.ids_parm[1]

if Not of_buildmapdw() then
	Close(This)
	RETURN
end if

of_setpopmenu()						// dynamically create menu items for equipments
											// configure tooltip
ii_tID[1]=inv_tooltip.of_addtool(dw_map,"")
inv_tooltip.il_height_adjustment=Long(dw_map.Describe("datawindow.header.height"))
inv_tooltip.of_setdelay(3,250)	// set initial delay to 250 ms
inv_tooltip.of_setdelay(2,10000)	// set duration to 10 seconds

end event

event close;if IsValid(im_pop) then DESTROY im_pop

f_win_save(is_key,This)

if IsValid(w_gfiscc) then
	if w_gfiscc.ib_closing then
	elseif ib_changed then
		f_slog("UPDATE","Equipment map position(s) have been modified")
		w_gfiscc.Post of_remap()
	else
		w_gfiscc.Post Show()
	end if
end if

end event

event timer;Timer(0)
dw_map.Modify("p_map.visible=1")

end event

event key;if is_selected<>"" then
	choose case key
		case KeyLeftArrow!, KeyUpArrow!, KeyRightArrow!, KeyDownArrow!
			RETURN dw_map.Event ue_key(key,keyflags)
	end choose
end if

RETURN 0

end event

type dw_map from datawindow within w_gfiscc_set
event ue_mousemove pbm_dwnmousemove
event ue_lbuttonup pbm_dwnlbuttonup
event ue_key pbm_dwnkey
event ue_keyup pbm_keyup
integer width = 2002
integer height = 1500
integer taborder = 10
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event ue_mousemove;string	ls
long		ll[2]

if is_drag<>"" then RETURN				// while in dragging mode, do not process this script

ls=dwo.name
choose case ls
	case "p_nav"
	case "p_bin"
		if ls<>is_tdwo and ii_tID[1]>0 then
			inv_tooltip.of_setdw(This,ii_tID[1],ls)
		end if

	case "p_map", "datawindow", "p_logo", "t_aut"
		if KeyDown(KeyLeftButton!) then
			choose case is_clicked
				case "p_map", "datawindow", "p_logo", "t_aut"
					if Abs(xpos - il_clicked_pos[1])>2 or Abs(ypos - il_clicked_pos[2])>2 then
						ib_movemap=TRUE
						ll={UnitsToPixels(This.PointerX(),XUnitsToPixels!),&
							 UnitsToPixels(This.PointerY(),YUnitsToPixels!)}
						if il_pos[1]=0 and il_pos[2]=0 then
							il_pos=ll		// record current cursor position in pixels
						elseif Abs(ll[1] - il_pos[1])>=8 or Abs(ll[2] - il_pos[2])>=8 then
							// scroll the map if disposition is 8 pixels or more (mickey)
							of_scroll(ll[1] - il_pos[1],ll[2] - il_pos[2])
							il_pos=ll		// record current cursor position in pixels
						end if
					end if
			end choose
		end if

	case else
		if Match(ls,"^t_tvm") then ls=Mid(ls,3)
		if Match(ls,"^tvm") then
			if KeyDown(KeyLeftButton!) then	// enter drag mode if disposition is over 2 pixels, drag icon is the image name (led_w.gif/led_g.gif) with .ico extension
				if ls=is_clicked then
					if Abs(xpos - il_clicked_pos[1])>2 or Abs(ypos - il_clicked_pos[2])>2 then
						is_dragicon=Left(Describe(ls+".filename"),6)+"ico"
						DragIcon=is_dragicon
						of_showselected(FALSE)
						is_drag=ls
						if Pos(is_selected,ls+",")=0 then is_selected+=ls+","

						// hide equipment(s) while dragging (drag icon is showing)
						ls=f_replace(is_selected,",",".visible=0 ")
						Modify(RightTrim(ls))

						Drag(Begin!)
					end if
				end if
				RETURN

			elseif ls<>is_tdwo and ii_tID[1]>0 then
				inv_tooltip.of_setdw(This,ii_tID[1],ls)
			end if
		end if

end choose

is_tdwo=ls

end event

event ue_lbuttonup;choose case String(dwo.name)
	case "p_map", "datawindow", "p_logo", "t_aut"
		if ib_movemap then
			ib_movemap=FALSE
		else
			of_showselected(FALSE)
			is_selected=""
		end if
end choose

end event

event ue_key;il_keycnt++

choose case key					// move selected equipments, accelerated
	case KeyLeftArrow!
		of_move( -Max(Min(Int((il_keycnt+4)/5),16),1),0)
	case KeyRightArrow!
		of_move(Max(Min(Int((il_keycnt+4)/5),16),1),0)
	case KeyUpArrow!
		of_move(0, -Max(Min(Int((il_keycnt+4)/5),16),1))
	case KeyDownArrow!
		of_move(0,Max(Min((Int(il_keycnt+4)/5),16),1))
	case else
		il_keycnt --
		RETURN 0
end choose

RETURN 1

end event

event ue_keyup;string	ls_eq[], ls, ls_desc
int		li, li_loc, li_eq
long		ll[2], ll_row

il_keycnt=0

choose case key
	case KeyLeftArrow!, KeyUpArrow!, KeyRightArrow!, KeyDownArrow!
		if is_selected="" then RETURN 1
	case else
		RETURN 0
end choose

dw_map.SetRedraw(FALSE)
for li=f_ss2a(is_selected,ls_eq) to 1 step -1
	if ls_eq[li]<>"" then
		ll={Long(dw_map.Describe(ls_eq[li]+".x")),Long(dw_map.Describe(ls_eq[li]+".y"))}

		li_loc=Integer(Mid(ls_eq[li],LastPos(ls_eq[li],"_")+1))
		li_eq=Integer(Mid(Left(ls_eq[li],Pos(ls_eq[li],"_") - 1),4))

		ll_row=ids_eq.Find("loc_n="+String(li_loc)+" and eq_n="+String(li_eq),1,ids_eq.RowCount()+1)
		if ll_row>0 then
			ls_desc=ids_eq.GetItemString(ll_row,"label")
			ls=" from ("+String(ids_eq.GetItemNumber(ll_row,"map_x"))+","+String(ids_eq.GetItemNumber(ll_row,"map_y"))+") to ("+&
				String(ll[1])+","+String(ll[2])+") on the transit map"

			UPDATE gfi_eq SET map_x=:ll[1], map_y=:ll[2]
			WHERE loc_n=:li_loc AND eq_type=1 AND eq_n=:li_eq;
			if SQLCA.SQLCode<0 then
				if gnv_syscon.ib_log_db then f_log("Moving "+ls_desc+ls+" failed~r~n"+SQLCA.SQLErrText)
				ROLLBACK;
				CONTINUE
			else
				COMMIT;
				if gnv_syscon.ib_log_info then f_log(ls_desc+" has been moved"+ls)
			end if
			of_saveaction("M"+ls_eq[li]+","+String(ids_eq.GetItemNumber(ll_row,"map_x"))+","+String(ids_eq.GetItemNumber(ll_row,"map_y"))+",led_g")

			ids_eq.SetItem(ll_row,"map_x",ll[1])
			ids_eq.SetItem(ll_row,"map_y",ll[2])
		end if
	end if
next
dw_map.SetRedraw(TRUE)

RETURN 1

end event

event dragdrop;choose case of_getobj()
	case "p_nav"
		of_dragdrop(FALSE)
	case "p_bin"
		of_unmap()
	case else
		of_dragdrop(TRUE)
		il_pos={0,0}
end choose

end event

event dragwithin;string	ls

choose case of_getobj()
	case "p_nav"
		ls="led_r.ico"				// set drag icon to red when on top of navigation control
	case "p_bin"
		if Left(is_dragicon,5)="led_w" then
			ls="led_r.ico"			// set drag icon to red when dragging on top of recycle bin
		else
			ls="led_y.ico"			// set drag icon to yellow when dragging on top of recycle bin
		end if
	case else
		ls=is_dragicon				// restore original drag icon if necessary
		of_scroll()					// when dragged close to borders, scroll
end choose

if DragIcon<>ls then DragIcon=ls

end event

event dragenter;DragIcon=is_dragicon				// restore the drag icon

end event

event rbuttondown;choose case dwo.name
	case "p_map", "t_aut", "datawindow", "p_logo"	// when right click on the map, show the popup menu
		if Integer(Describe("p_map.visible"))=1 then
			im_pop.m_map.m_maphide.Enabled=TRUE
			if gnv_syscon.is_parm[1]<>"" then
				im_pop.m_map.m_mapinfo.Enabled=TRUE
			else
				im_pop.m_map.m_mapinfo.Enabled=FALSE
			end if
		else
			im_pop.m_map.m_maphide.Enabled=FALSE
			im_pop.m_map.m_mapinfo.Enabled=FALSE
		end if
		im_pop.m_map.m_mapundo.Enabled=(is_action[1]<>"")
		im_pop.m_map.m_mapundoall.Enabled=(is_action[2]<>"")
		im_pop.m_map.PopMenu(Parent.PointerX(),Parent.PointerY())

end choose

end event

event clicked;string	ls

ls=String(dwo.name)
if ls="p_nav" then					// navigate
	of_scroll(25)						// move 25 pixels if arrow areas are clicked
	RETURN
elseif ls="p_bin" then				// recycle bin - do nothing
	RETURN
end if

SetRedraw(FALSE)
											// if clicked on "selected check", remove prefix
if Match(ls,"^t_tvm") then ls=Mid(ls,3)

if Match(ls,"^tvm") then			// click an equipment
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
else
	ib_movemap=FALSE
end if

is_clicked=ls							// remember the clicked object
il_clicked_pos={xpos,ypos}			// remember the clicked position

SetRedraw(TRUE)

il_keycnt=0
il_pos={0,0}							// initialize position variable that is used to move the map by dragging it

end event

event doubleclicked;if dwo.name="p_nav" then of_scroll(50)			// move 50 pixels if arrow areas are double clicked

end event

