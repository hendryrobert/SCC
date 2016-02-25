$PBExportHeader$w_gfiscc_file.srw
$PBExportComments$Configuration file type window
forward
global type w_gfiscc_file from window
end type
type ddlb_pkg from dropdownlistbox within w_gfiscc_file
end type
type cb_cancel from commandbutton within w_gfiscc_file
end type
type cb_ok from commandbutton within w_gfiscc_file
end type
type gb_type from groupbox within w_gfiscc_file
end type
end forward

global type w_gfiscc_file from window
boolean visible = false
integer width = 1710
integer height = 528
boolean titlebar = true
string title = "Choose configuration package"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
ddlb_pkg ddlb_pkg
cb_cancel cb_cancel
cb_ok cb_ok
gb_type gb_type
end type
global w_gfiscc_file w_gfiscc_file

type variables

end variables

on w_gfiscc_file.create
this.ddlb_pkg=create ddlb_pkg
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.gb_type=create gb_type
this.Control[]={this.ddlb_pkg,&
this.cb_cancel,&
this.cb_ok,&
this.gb_type}
end on

on w_gfiscc_file.destroy
destroy(this.ddlb_pkg)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.gb_type)
end on

event open;string	ls[2]={"TVM PARM SEQ","PEM PARM SEQ"}
int		li

li=Message.DoubleParm
if li=2 then
else
	li=1
end if

Message.DoubleParm= -1

DECLARE cur CURSOR FOR SELECT code, name, value FROM gfi_lst WHERE type=:ls[li] AND class=0 AND code BETWEEN 1 AND 10 ORDER BY value, code;
OPEN cur;
if SQLCA.SQLCode=0 then
	do
		FETCH cur INTO :li, :ls[1], :ls[2];
		if SQLCA.SQLCode=0 then
			ddlb_pkg.AddItem(String(li,"00")+" - ["+ls[2]+"] "+Trim(ls[1]))
		end if
	loop while SQLCA.SQLCode=0
	CLOSE cur;

	if ddlb_pkg.TotalItems()>0 then
		ddlb_pkg.SelectItem(1)
		Show()
	else
		MessageBox(gnv_syscon.of_gettitle(),"No parameter file setting was found. The default one will be sent.")
		Post CloseWithReturn(This,1)
		RETURN
	end if
else
	MessageBox(gnv_syscon.of_gettitle(),"Checking parameter file settings in "+ls[li]+" list failed. The default one will be sent.~r~n~r~n"+SQLCA.SQLErrText,Exclamation!)
	Post CloseWithReturn(This,1)
	RETURN
end if

end event

type ddlb_pkg from dropdownlistbox within w_gfiscc_file
integer x = 105
integer y = 100
integer width = 1481
integer height = 732
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 33554432
boolean sorted = false
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_gfiscc_file
integer x = 905
integer y = 296
integer width = 343
integer height = 92
integer taborder = 30
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
string text = "Cancel"
boolean cancel = true
end type

event clicked;CloseWithReturn(Parent, -1)
end event

type cb_ok from commandbutton within w_gfiscc_file
integer x = 448
integer y = 296
integer width = 343
integer height = 92
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
string text = "OK"
boolean default = true
end type

event clicked;CloseWithReturn(Parent,Integer(Left(ddlb_pkg.Text,2)))

end event

type gb_type from groupbox within w_gfiscc_file
integer x = 46
integer y = 20
integer width = 1600
integer height = 220
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Microsoft Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Configuration Package"
end type

