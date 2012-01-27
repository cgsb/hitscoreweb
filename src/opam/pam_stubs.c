/* Copyright (c) 2008 Sharvil Nanavati */

#include <stdlib.h>
#include <string.h>

#include <security/pam_appl.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/custom.h>

#include "pam_stubs.h"

static void raise(pam_error exception) Noreturn;
static void finalizer(value v);

static struct custom_operations caml_pam_operations = 
{
	"net.nanavati.sharvil.pam.operations",
	finalizer,
	custom_compare_default,
	custom_hash_default,
	custom_serialize_default,
	custom_deserialize_default	
};

static void raise(pam_error exception)
{
	static value * e = 0;

	if(e == 0)
		e = caml_named_value("net.nanavati.sharvil.pam.error");

	caml_raise_with_arg(*e, Val_int(exception));
}

static void finalizer(value v)
{
	caml_pam_handle * h = Handle_val(v);

	if(h->handle != 0)
		pam_end(h->handle, h->error_code);

	if(h->callback != Val_int(0))
		caml_remove_global_root(&h->callback);

	if(h->fail_delay != Val_int(0))
		caml_remove_global_root(&h->fail_delay);
}

static int converse(int nMsg, const struct pam_message ** messages, struct pam_response ** responses, void * data)
{
	int i;
	struct pam_response * local_responses;
	caml_pam_handle * h;

	CAMLlocal1(ret);

	h = (caml_pam_handle *)data;
	local_responses = (struct pam_response *)calloc(nMsg, sizeof(struct pam_response));

	if(local_responses == 0)
	{
		free(local_responses);
		return PAM_BUF_ERR;
	}
	for(i = 0; i < nMsg; ++i)
		switch(messages[i]->msg_style)
		{
			case PAM_PROMPT_ECHO_OFF:
			{
				ret = caml_callback2(h->callback, Val_int(Pam_Prompt_Echo_Off), caml_copy_string(messages[i]->msg));
				local_responses[i].resp = strdup(String_val(ret));
				break;
			}
			case PAM_PROMPT_ECHO_ON:
			{
				ret = caml_callback2(h->callback, Val_int(Pam_Prompt_Echo_On), caml_copy_string(messages[i]->msg));
				local_responses[i].resp = strdup(String_val(ret));
				break;
			}
			case PAM_ERROR_MSG:
			{
				caml_callback2(h->callback, Val_int(Pam_Error_Msg), caml_copy_string(messages[i]->msg));
				break;
			}
			case PAM_TEXT_INFO:
			{
				caml_callback2(h->callback, Val_int(Pam_Text_Info), caml_copy_string(messages[i]->msg));
				break;
			}
			default:
			{
				for(i = 0; i < nMsg; ++i)
					if(local_responses[i].resp != 0)
						free(local_responses[i].resp);
				free(local_responses);
				return PAM_CONV_ERR;
			}
		}

	*responses = local_responses;
	return PAM_SUCCESS;
}

static void fail_delay(int retval, unsigned usec_delay, void * data)
{
	caml_pam_handle * h = (caml_pam_handle *)data;

	if(h->fail_delay == Val_int(0))
		return;

	caml_callback2(h->fail_delay, Val_int(retval), Val_int(usec_delay));
}

CAMLprim value pam_start_stub(value serviceName, value user, value conversation)
{
	CAMLparam3 (serviceName, user, conversation);
	CAMLlocal1(ret);
	struct pam_conv conv;
	const char * user_str = 0;
	caml_pam_handle * h;

	ret = caml_alloc_custom(&caml_pam_operations, sizeof(caml_pam_handle), 1, 100);
	h = Handle_val(ret);

	caml_register_global_root(&h->callback);
	h->callback = conversation;

	caml_register_global_root(&h->fail_delay);
	h->fail_delay = Val_int(0);

	conv.conv = converse;
	conv.appdata_ptr = h;

	if(Is_block(user))
		user_str = String_val(Field(user, 0));

	h->error_code = pam_start(String_val(serviceName), user_str, &conv, &h->handle);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_ABORT:      raise(Pam_Abort);
		case PAM_BUF_ERR:    raise(Pam_Buf_Err);
		case PAM_SYSTEM_ERR: raise(Pam_System_Err);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(ret);
}

CAMLprim value pam_end_stub(value handle)
{
	CAMLparam1(handle);
	CAMLlocal1(ret);
	caml_pam_handle * h = Handle_val(handle);

	ret = Val_true;

	if(h->handle != 0)
	{
		h->error_code = pam_end(h->handle, h->error_code);
		if(h->error_code != PAM_SUCCESS)
			ret = Val_false;
	}

	if(h->callback != Val_int(0))
		caml_remove_global_root(&h->callback);

	if(h->fail_delay != Val_int(0))
		caml_remove_global_root(&h->fail_delay);

	h->handle = 0;
	h->callback = Val_int(0);
	h->fail_delay = Val_int(0);

	CAMLreturn(ret);
}

CAMLprim value pam_remove_fail_delay(value handle)
{
	CAMLparam1(handle);
	caml_pam_handle * h = Handle_val(handle);

	h->error_code = pam_set_item(h->handle, PAM_FAIL_DELAY, 0);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_BAD_ITEM:   raise(Pam_Bad_Item);
		case PAM_BUF_ERR:    raise(Pam_Buf_Err);
		case PAM_SYSTEM_ERR: raise(Pam_System_Err);
		default: caml_failwith("Unknown PAM error");
	}

	h->fail_delay = Val_int(0);

	CAMLreturn(Val_unit);
}

CAMLprim value pam_set_item_stub(value handle, value item)
{
	int type;
	CAMLparam2(handle, item);
	caml_pam_handle * h = Handle_val(handle);

	switch(Tag_val(item))
	{
		case Pam_Service:     type = PAM_SERVICE;     break;
		case Pam_User:        type = PAM_USER;        break;
		case Pam_User_Prompt: type = PAM_USER_PROMPT; break;
		case Pam_Tty:         type = PAM_TTY;         break;
		case Pam_RUser:       type = PAM_RUSER;       break;
		case Pam_RHost:       type = PAM_RHOST;       break;
		case Pam_AuthTok:     type = PAM_AUTHTOK;     break;
		case Pam_OldAuthTok:  type = PAM_OLDAUTHTOK;  break;
		case Pam_Conv:        type = PAM_CONV;        break;
		case Pam_Fail_Delay:  type = PAM_FAIL_DELAY;  break;
		default: 
			raise(Pam_Bad_Item);
	}

	if(type == PAM_CONV)
	{
		h->callback = Field(item, 0);
		CAMLreturn(Val_unit);
	}

	if(type == PAM_FAIL_DELAY)
	{
		h->fail_delay = Field(item, 0);
		h->error_code = pam_set_item(h->handle, type, fail_delay);
	}
	else
		h->error_code = pam_set_item(h->handle, type, String_val(Field(item, 0)));

	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_BAD_ITEM:   raise(Pam_Bad_Item);
		case PAM_BUF_ERR:    raise(Pam_Buf_Err);
		case PAM_SYSTEM_ERR: raise(Pam_System_Err);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value pam_get_item_stub(value handle, value item)
{
	int type;
	const char * val;
	CAMLparam2(handle, item);
	CAMLlocal1(ret);
	caml_pam_handle * h = Handle_val(handle);

	switch(Tag_val(item))
	{
		case Pam_Service:     type = PAM_SERVICE;     break;
		case Pam_User:        type = PAM_USER;        break;
		case Pam_User_Prompt: type = PAM_USER_PROMPT; break;
		case Pam_Tty:         type = PAM_TTY;         break;
		case Pam_RUser:       type = PAM_RUSER;       break;
		case Pam_RHost:       type = PAM_RHOST;       break;
		case Pam_AuthTok:     type = PAM_AUTHTOK;     break;
		case Pam_OldAuthTok:  type = PAM_OLDAUTHTOK;  break;
		case Pam_Conv:        type = PAM_CONV;        break;
		case Pam_Fail_Delay:  type = PAM_FAIL_DELAY;  break;
		default: 
			raise(Pam_Bad_Item);
	}

	/* Special-case PAM_CONV since we're storing the closure internally. */
	if(type == PAM_CONV)
	{
		ret = caml_alloc(1, Tag_val(item));
		Store_field(ret, 0, h->callback);
		CAMLreturn(ret);
	}

	if(type == PAM_FAIL_DELAY)
	{
		ret = caml_alloc(1, Tag_val(item));
		Store_field(ret, 0, (h->fail_delay == Val_int(0)) ? Field(item, 1) : h->fail_delay);
		CAMLreturn(ret);
	}

	h->error_code = pam_get_item(h->handle, type, (const void **)&val); 
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_BAD_ITEM:    raise(Pam_Bad_Item);
		case PAM_BUF_ERR:     raise(Pam_Buf_Err);
		case PAM_PERM_DENIED: raise(Pam_Perm_Denied);
		case PAM_SYSTEM_ERR:  raise(Pam_System_Err);
		default: caml_failwith("Unknown PAM error");
	}

	ret = caml_alloc(1, Tag_val(item));
	Store_field(ret, 0, caml_copy_string((val == 0) ? "" : val));

	CAMLreturn(ret);
}

CAMLprim value pam_fail_delay_stub(value handle, value usec)
{
	CAMLparam2(handle, usec);
	caml_pam_handle * h = Handle_val(handle);

	h->error_code = pam_fail_delay(h->handle, Int_val(usec));
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_SYSTEM_ERR: raise(Pam_System_Err);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value pam_authenticate_stub(value handle, value flags, value silent)
{
	int flagValue = 0;
	CAMLparam3(handle, flags, silent);
	caml_pam_handle * h = Handle_val(handle);

	while(flags != Val_int(0))
	{
		switch(Int_val(Field(flags, 0)))
		{
			case Pam_Disallow_Null_Authtok:
				flagValue |= PAM_DISALLOW_NULL_AUTHTOK;
				break;

			default: raise(Pam_Bad_Item);
		}
		flags = Field(flags, 1);
	}

	if(Is_block(silent) && Field(silent, 0) == Val_true)
		flagValue |= PAM_SILENT;

	h->error_code = pam_authenticate(h->handle, flagValue);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_ABORT:             raise(Pam_Abort);
		case PAM_AUTH_ERR:          raise(Pam_Auth_Err);
		case PAM_CRED_INSUFFICIENT: raise(Pam_Cred_Insufficient);
		case PAM_AUTHINFO_UNAVAIL:  raise(Pam_Authinfo_Unavail);
		case PAM_MAXTRIES:          raise(Pam_Maxtries);
		case PAM_USER_UNKNOWN:      raise(Pam_User_Unknown);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value pam_setcred_stub(value handle, value credList, value silent)
{
	CAMLparam3(handle, credList, silent);
	CAMLlocal1(cred);
	int flags = 0;
	caml_pam_handle * h = Handle_val(handle);

	switch(Int_val(credList))
	{
		case Pam_Establish_Cred:    flags |= PAM_ESTABLISH_CRED;    break;
		case Pam_Delete_Cred:       flags |= PAM_DELETE_CRED;       break;
		case Pam_Reinitialize_Cred: flags |= PAM_REINITIALIZE_CRED; break;
		case Pam_Refresh_Cred:      flags |= PAM_REFRESH_CRED;      break;
		default: raise(Pam_System_Err);
	}

	if(Is_block(silent) && Field(silent, 0) == Val_true)
		flags |= PAM_SILENT;

	h->error_code = pam_setcred(h->handle, flags);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_BUF_ERR: raise(Pam_Buf_Err);
		case PAM_CRED_ERR: raise(Pam_Cred_Err);
		case PAM_CRED_EXPIRED: raise(Pam_Cred_Expired);
		case PAM_CRED_UNAVAIL: raise(Pam_Cred_Unavail);
		case PAM_SYSTEM_ERR: raise(Pam_System_Err);
		case PAM_USER_UNKNOWN: raise(Pam_User_Unknown);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value pam_acct_mgmt_stub(value handle, value authFlags, value silent)
{
	CAMLparam3(handle, authFlags, silent);
	int flags = 0;
	caml_pam_handle * h = Handle_val(handle);

	while(authFlags != Val_int(0))
	{
		switch(Field(authFlags, 0))
		{
			case Pam_Disallow_Null_Authtok: flags |= PAM_DISALLOW_NULL_AUTHTOK; break;
			default: raise(Pam_System_Err);
		}
		authFlags = Field(authFlags, 1);
	}

	if(Is_block(silent) && Field(silent, 0) == Val_true)
		flags |= PAM_SILENT;

	h->error_code = pam_acct_mgmt(h->handle, flags);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_ACCT_EXPIRED: raise(Pam_Acct_Expired);
		case PAM_AUTH_ERR: raise(Pam_Auth_Err);
		case PAM_NEW_AUTHTOK_REQD: raise(Pam_New_Authtok_Reqd);
		case PAM_PERM_DENIED: raise(Pam_Perm_Denied);
		case PAM_USER_UNKNOWN: raise(Pam_User_Unknown);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value pam_chauthtok_stub(value handle, value tokenFlags, value silent)
{
	CAMLparam3(handle, tokenFlags, silent);
	int flags = 0;
	caml_pam_handle * h = Handle_val(handle);

	while(tokenFlags != Int_val(0))
	{
		switch(Field(tokenFlags, 0))
		{
			case Pam_Change_Expired_Authtok: flags |= PAM_CHANGE_EXPIRED_AUTHTOK; break;
			default: raise(Pam_System_Err);
		}
		tokenFlags = Field(tokenFlags, 1);
	}

	if(Is_block(silent) && Field(silent, 0) == Val_true)
		flags |= PAM_SILENT;

	h->error_code = pam_chauthtok(h->handle, flags);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_AUTHTOK_ERR: raise(Pam_Authtok_Err);
		case PAM_AUTHTOK_RECOVER_ERR: raise(Pam_Authtok_Recover_Err);
		case PAM_AUTHTOK_LOCK_BUSY: raise(Pam_Authtok_Lock_Busy);
		case PAM_AUTHTOK_DISABLE_AGING: raise(Pam_Authtok_Disable_Aging);
		case PAM_PERM_DENIED: raise(Pam_Perm_Denied);
		case PAM_TRY_AGAIN: raise(Pam_Try_Again);
		case PAM_USER_UNKNOWN: raise(Pam_User_Unknown);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value pam_open_session_stub(value handle, value silent)
{
	CAMLparam2(handle, silent);
	caml_pam_handle * h = Handle_val(handle);
	int flags = 0;

	if(Is_block(silent) && Field(silent, 0) == Val_true)
		flags |= PAM_SILENT;

	h->error_code = pam_open_session(h->handle, flags);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_ABORT: raise(Pam_Abort);
		case PAM_BUF_ERR: raise(Pam_Buf_Err);
		case PAM_SESSION_ERR: raise(Pam_Session_Err);
		default: caml_failwith("Unknown PAM error");
	}

	CAMLreturn(Val_unit);
}

CAMLprim value pam_close_session_stub(value handle, value silent)
{
	CAMLparam2(handle, silent);
	caml_pam_handle * h = Handle_val(handle);
	int flags = 0;

	if(Is_block(silent) && Field(silent, 0) == Val_true)
		flags |= PAM_SILENT;

	h->error_code = pam_close_session(h->handle, flags);
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_ABORT: raise(Pam_Abort);
		case PAM_BUF_ERR: raise(Pam_Buf_Err);
		case PAM_SESSION_ERR: raise(Pam_Session_Err);
		default: caml_failwith("Unknown PAM error");
	}
	CAMLreturn(Val_unit);
}

CAMLprim value pam_putenv_stub(value handle, value nameValue)
{
	CAMLparam2(handle, nameValue);
	caml_pam_handle * h = Handle_val(handle);

	h->error_code = pam_putenv(h->handle, String_val(nameValue));
	switch(h->error_code)
	{
		case PAM_SUCCESS: break;
		case PAM_PERM_DENIED: raise(Pam_Perm_Denied);
		case PAM_BAD_ITEM: raise(Pam_Bad_Item);
		case PAM_ABORT: raise(Pam_Abort);
		case PAM_BUF_ERR: raise(Pam_Buf_Err);
		default: caml_failwith("Unknown PAM error");
	}
	CAMLreturn(Val_unit);
}

CAMLprim value pam_getenv_stub(value handle, value nameValue)
{
	CAMLparam2(handle, nameValue);
	CAMLlocal1(ret);
	const char * val;
	caml_pam_handle * h = Handle_val(handle);

	ret = Val_int(0);
	val = pam_getenv(h->handle, String_val(nameValue));
	if(val != 0)
	{
		ret = caml_alloc(1, 0);
		Store_field(ret, 0, caml_copy_string(val));
	}

	CAMLreturn(ret);
}

CAMLprim value pam_getenvlist_stub(value handle)
{
	CAMLparam1(handle);
	CAMLlocal2(ret, tmp);
	char ** envlist;
	caml_pam_handle * h = Handle_val(handle);

	envlist = pam_getenvlist(h->handle);

	ret = Val_int(0);
	for(; *envlist != 0; ++envlist)
	{
		tmp = caml_alloc(2, 0);
		Store_field(tmp, 1, ret);
		Store_field(tmp, 0, caml_copy_string(*envlist));
		ret = tmp;

		free(*envlist);
	}

	CAMLreturn(ret);
}
