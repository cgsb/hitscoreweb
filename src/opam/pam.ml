(* Copyright (c) 2008 Sharvil Nanavati *)

type pam_error = 
	| Pam_Abort                  (*  0 *)
	| Pam_Session_Err
	| Pam_Authtok_Err
	| Pam_Authtok_Recover_Err
	| Pam_Authtok_Lock_Busy
	| Pam_Authtok_Disable_Aging  (*  5 *)
	| Pam_Try_Again
	| Pam_Acct_Expired
	| Pam_New_Authtok_Reqd
	| Pam_Perm_Denied
	| Pam_Cred_Err               (* 10 *)
	| Pam_Cred_Expired
	| Pam_Cred_Unavail
	| Pam_Auth_Err
	| Pam_Cred_Insufficient
	| Pam_Authinfo_Unavail       (* 15 *)
	| Pam_Maxtries
	| Pam_User_Unknown
	| Pam_Buf_Err
	| Pam_System_Err
	| Pam_Bad_Item;;             (* 20 *)

exception Pam_Error of pam_error;;

type pam_handle;;
type pam_conv_type =
	| Pam_Prompt_Echo_Off
	| Pam_Prompt_Echo_On
	| Pam_Error_Msg
	| Pam_Text_Info;;

type pam_conv = pam_conv_type -> string -> string;;
type pam_auth_flags = Pam_Disallow_Null_Authtok;;

type pam_fail_delay = int -> int -> unit;;

type pam_credentials =
	| Pam_Establish_Cred
	| Pam_Delete_Cred
	| Pam_Reinitialize_Cred
	| Pam_Refresh_Cred;;

type pam_token_flags = Pam_Change_Expired_Authtok;;
type pam_item =
	| Pam_Service of string
	| Pam_User of string
	| Pam_User_Prompt of string
	| Pam_Tty of string
	| Pam_RUser of string
	| Pam_RHost of string
	| Pam_AuthTok of string
	| Pam_OldAuthTok of string
	| Pam_Conv of pam_conv
	| Pam_Fail_Delay of pam_fail_delay;;

let pam_item_service = Pam_Service "";;
let pam_item_user = Pam_User "";;
let pam_item_user_prompt = Pam_User_Prompt "";;
let pam_item_tty = Pam_Tty "";;
let pam_item_ruser = Pam_RUser "";;
let pam_item_rhost = Pam_RHost "";;
let pam_item_authtok = Pam_AuthTok "";;
let pam_item_oldauthtok = Pam_OldAuthTok "";;
let pam_item_conv = Pam_Conv (fun x y -> "");;
let pam_item_fail_delay = Pam_Fail_Delay (fun x y -> ());;

let _ = Callback.register_exception "hsopam.net.nanavati.sharvil.pam.error" (Pam_Error Pam_Abort);;

external pam_start : string -> ?user:string -> pam_conv -> pam_handle = "pam_start_stub";;
external pam_end : pam_handle -> bool = "pam_end_stub";;
external i_pam_remove_fail_delay : pam_handle -> unit = "pam_remove_fail_delay";;
external i_pam_set_item : pam_handle -> pam_item -> unit = "pam_set_item_stub";;
external pam_get_item : pam_handle -> pam_item -> pam_item = "pam_get_item_stub";;
external pam_fail_delay : pam_handle -> int -> unit = "pam_fail_delay_stub";;
external pam_authenticate : 
  pam_handle -> pam_auth_flags list -> ?silent:bool -> bool = "pam_authenticate_stub";;
external pam_setcred : pam_handle -> pam_credentials -> ?silent:bool -> unit = "pam_setcred_stub";;
external pam_acct_mgmt : pam_handle -> pam_auth_flags list -> ?silent:bool -> unit = "pam_acct_mgmt_stub";;
external pam_chauthtok : pam_handle -> pam_token_flags list -> ?silent:bool -> unit = "pam_chauthtok_stub";;
external pam_open_session : pam_handle -> ?silent:bool -> unit = "pam_open_session_stub";;
external pam_close_session : pam_handle -> ?silent:bool -> unit = "pam_close_session_stub";;
external pam_putenv : pam_handle -> string -> unit = "pam_putenv_stub";;
external pam_getenv : pam_handle -> string -> string option = "pam_getenv_stub";;
external pam_getenvlist : pam_handle -> string list = "pam_getenvlist_stub";;

let pam_set_item handle item =
	match item with
		| Pam_Fail_Delay _ -> if item == pam_item_fail_delay then i_pam_remove_fail_delay handle else i_pam_set_item handle item
		| _ -> i_pam_set_item handle item
;;

type pam_functions =
{
	pam_end : unit -> bool;
	pam_set_item : pam_item -> unit;
	pam_get_item : pam_item -> pam_item;
	pam_fail_delay : int -> unit;
	pam_authenticate : pam_auth_flags list -> ?silent:bool -> bool;
	pam_setcred : pam_credentials -> ?silent:bool -> unit;
	pam_acct_mgmt : pam_auth_flags list -> ?silent:bool -> unit;
	pam_chauthtok : pam_token_flags list -> ?silent:bool -> unit;
	pam_open_session : ?silent:bool -> unit -> unit;
	pam_close_session : ?silent:bool -> unit -> unit;
	pam_putenv : string -> unit;
	pam_getenv : string -> string option;
	pam_getenvlist : unit -> string list
};;

let pam_start_ex serviceName ?user conv=
	let h = pam_start serviceName ?user conv in
	{
		pam_end = (function () -> pam_end h);
		pam_set_item = pam_set_item h;
		pam_get_item = pam_get_item h;
		pam_fail_delay = pam_fail_delay h;
		pam_authenticate = pam_authenticate h;
		pam_setcred = pam_setcred h;
		pam_acct_mgmt = pam_acct_mgmt h;
		pam_chauthtok = pam_chauthtok h;
		pam_open_session = (fun ?silent () -> pam_open_session h ?silent);
		pam_close_session = (fun ?silent () -> pam_close_session h ?silent);
		pam_putenv = pam_putenv h;
		pam_getenv = pam_getenv h;
		pam_getenvlist = (function () -> pam_getenvlist h);
	}
;;
