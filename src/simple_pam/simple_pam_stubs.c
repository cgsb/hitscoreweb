
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/threads.h>
#include <stdio.h>


#include <security/pam_appl.h>


/*
int pam_start(const char *service_name, 
const char *user,
 const struct pam_conv *pam_conversation,
 pam_handle_t **pamh);
*/


/* Adapted from xenagentd.hg:src/xa_auth.c */
struct xa_auth_info {
    const char *username;
    const char *password;
};

static int xa_auth_conv(int num_msg, const struct pam_message **msg,
                        struct pam_response **resp, void *app_data)
{
    struct xa_auth_info *auth_info = app_data;
    struct pam_response *response;
    int i, j;

    if (msg == NULL || resp == NULL || app_data == NULL) 
        return PAM_CONV_ERR;
    
    response = calloc (num_msg, sizeof (struct pam_response));
    if (response == NULL)
        return PAM_CONV_ERR;
    
    for (i = 0; i < num_msg; i++) {
        switch(msg[i]->msg_style) {
        case PAM_PROMPT_ECHO_ON:
            response[i].resp = strdup(auth_info->username);
            if (response[i].resp == NULL)
              goto resperr;
            break;
        case PAM_PROMPT_ECHO_OFF:
            response[i].resp = strdup(auth_info->password);
            if (response[i].resp == NULL)
              goto resperr;
            break;
        default:
            goto resperr;
        }
    }
   
    *resp = response;
    return PAM_SUCCESS;

resperr:
    for (j = 0; j < i; j++)
        free(response[j].resp);
    free(response);
    return PAM_CONV_ERR;
}


#define AUTH_SUCCESS 0
#define AUTH_FAILURE 1

int
do_pam_authorize(const char *service,
                 const char *username,
                 const char *password,
                 const char **error) {
  struct xa_auth_info auth_info = {username, password};
  struct pam_conv xa_conv = {xa_auth_conv, &auth_info};
  pam_handle_t *pamh;
  int rc = AUTH_SUCCESS;

    if ((rc = pam_start(service, username, &xa_conv, &pamh))
        != PAM_SUCCESS) {
      goto exit;
    }
    if ((rc = pam_authenticate(pamh, PAM_DISALLOW_NULL_AUTHTOK))
        != PAM_SUCCESS) {
      goto exit;
    }
    
    rc = pam_acct_mgmt(pamh, PAM_DISALLOW_NULL_AUTHTOK);

 exit:
    pam_end(pamh, rc);
    if (rc != PAM_SUCCESS) {
        if (error) *error = pam_strerror(pamh, rc);
        rc = AUTH_FAILURE;
    }
    else {
        rc = AUTH_SUCCESS;
    }
    return rc;
}


CAMLprim value 
simple_pam_authorize_stub(value service, value username, value password){
  CAMLparam3(service, username, password);
  CAMLlocal1(ret);
  ret = Val_unit;
  
  char *c_service  = strdup(String_val(service));
  char *c_username = strdup(String_val(username));
  char *c_password = strdup(String_val(password));
  const char *error = NULL;
  int rc;
  
  caml_enter_blocking_section();
  rc = do_pam_authorize(c_service, c_username, c_password, &error);
  caml_leave_blocking_section();
  
  free(c_service);
  free(c_username);
  free(c_password);
  
  if (rc != AUTH_SUCCESS)
    caml_failwith(error ? error : "Unknown error");
  CAMLreturn(ret);
}
