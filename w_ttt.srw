$PBExportHeader$w_ttt.srw
forward
global type w_ttt from window
end type
end forward

global type w_ttt from window
integer width = 4754
integer height = 1980
boolean titlebar = true
string title = "Untitled"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
end type
global w_ttt w_ttt

event open;long	ll=10203

messagebox("",string(ll/10000,"#0")+"."+string(mod(ll/100,100),"00")+"."+string(mod(ll,100),"00"))
end event

on w_ttt.create
end on

on w_ttt.destroy
end on

