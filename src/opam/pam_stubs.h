/* Copyright (c) 2008 Sharvil Nanavati */

#ifndef PAM_STUBS_H
#define PAM_STUBS_H

typedef enum pam_error_tag
{
	Pam_Abort,
	Pam_Session_Err,
	Pam_Authtok_Err,
	Pam_Authtok_Recover_Err,
	Pam_Authtok_Lock_Busy,
	Pam_Authtok_Disable_Aging,
	Pam_Try_Again,
	Pam_Acct_Expired,
	Pam_New_Authtok_Reqd,
	Pam_Perm_Denied,
	Pam_Cred_Err,
	Pam_Cred_Expired,
	Pam_Cred_Unavail,
	Pam_Auth_Err,
	Pam_Cred_Insufficient,
	Pam_Authinfo_Unavail,
	Pam_Maxtries,
	Pam_User_Unknown,
	Pam_Buf_Err,
	Pam_System_Err,
	Pam_Bad_Item
} pam_error;

enum
{
	Pam_Prompt_Echo_Off,
	Pam_Prompt_Echo_On,
	Pam_Error_Msg,
	Pam_Text_Info
};

enum
{
	Pam_Disallow_Null_Authtok
};

enum
{
	Pam_Establish_Cred,
	Pam_Delete_Cred,
	Pam_Reinitialize_Cred,
	Pam_Refresh_Cred
};

enum
{
	Pam_Change_Expired_Authtok
};

enum
{
	Pam_Service,
	Pam_User,
	Pam_User_Prompt,
	Pam_Tty,
	Pam_RUser,
	Pam_RHost,
	Pam_AuthTok,
	Pam_OldAuthTok,
	Pam_Conv,
	Pam_Fail_Delay
};

typedef struct caml_pam_handle_tag
{
	pam_handle_t * handle;
	value callback;
	value fail_delay;
	int error_code;
} caml_pam_handle;

#define Handle_val(v) (caml_pam_handle *)Data_custom_val(v)

#endif
