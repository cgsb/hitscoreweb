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

val pam_item_service : pam_item;;
val pam_item_user : pam_item;;
val pam_item_user_prompt : pam_item;;
val pam_item_tty : pam_item;;
val pam_item_ruser : pam_item;;
val pam_item_rhost : pam_item;;
val pam_item_authtok : pam_item;;
val pam_item_oldauthtok : pam_item;;
val pam_item_conv : pam_item;;
val pam_item_fail_delay : pam_item;;

type pam_functions =
{
	pam_end : unit -> bool;
	pam_set_item : pam_item -> unit;
	pam_get_item : pam_item -> pam_item;
	pam_fail_delay : int -> unit;
	pam_authenticate : pam_auth_flags list -> ?silent:bool -> unit;
	pam_setcred : pam_credentials -> ?silent:bool -> unit;
	pam_acct_mgmt : pam_auth_flags list -> ?silent:bool -> unit;
	pam_chauthtok : pam_token_flags list -> ?silent:bool -> unit;
	pam_open_session : ?silent:bool -> unit -> unit;
	pam_close_session : ?silent:bool -> unit -> unit;
	pam_putenv : string -> unit;
	pam_getenv : string -> string option;
	pam_getenvlist : unit -> string list
};;

val pam_start_ex : string -> ?user:string -> pam_conv -> pam_functions;;
val pam_start : string -> ?user:string -> pam_conv -> pam_handle;;
val pam_end : pam_handle -> bool;;
val pam_set_item : pam_handle -> pam_item -> unit;;
val pam_get_item : pam_handle -> pam_item -> pam_item;;
val pam_fail_delay : pam_handle -> int -> unit;;
val pam_authenticate : pam_handle -> pam_auth_flags list -> ?silent:bool -> unit;;
val pam_setcred : pam_handle -> pam_credentials -> ?silent:bool -> unit;;
val pam_acct_mgmt : pam_handle -> pam_auth_flags list -> ?silent:bool -> unit;;
val pam_chauthtok : pam_handle -> pam_token_flags list -> ?silent:bool -> unit;;
val pam_open_session : pam_handle -> ?silent:bool -> unit;;
val pam_close_session : pam_handle -> ?silent:bool -> unit;;
val pam_putenv : pam_handle -> string -> unit;;
val pam_getenv : pam_handle -> string -> string option;;
val pam_getenvlist : pam_handle -> string list;;
