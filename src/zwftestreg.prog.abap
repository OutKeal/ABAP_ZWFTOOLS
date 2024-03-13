*&---------------------------------------------------------------------*
*& Report ZWFTESTREG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwftestreg.

DATA key TYPE string.
DATA dword_value TYPE i.
DATA rc TYPE i.
dword_value = 0.

key = 'Software\SAP\SAPGUI Front\SAP Frontend Server\Security'.
PERFORM frm_set_reg_dword USING key 'DefaultAction' 0.
PERFORM frm_set_reg_dword USING key 'SecurityLevel' 0.

key = 'Software\SAP\SAPGUI Front\SAP Frontend Server\Customize'.
PERFORM frm_set_reg_dword USING key 'Dropdown.ShowKey' 1.
PERFORM frm_set_reg_dword USING key 'Dropdown.SortKey' 1.
PERFORM frm_set_reg_dword USING key 'PasswordPlaceholderChars' 0.
PERFORM frm_set_reg_word USING key 'ActivePageSessionNaviTreeID' 'N02L115'.


FORM frm_set_reg_dword USING key value dword_value.
  cl_gui_frontend_services=>registry_set_dword_value( EXPORTING root = cl_gui_frontend_services=>hkey_current_user
    key = key
    value = value
    dword_value = dword_value
  IMPORTING rc = rc
  EXCEPTIONS cntl_error = 1
    error_no_gui = 2
    not_supported_by_gui = 3
    ).
  cl_gui_cfw=>flush( EXCEPTIONS OTHERS = 0 ) .
ENDFORM.

FORM frm_set_reg_word USING key  value_name value.
  cl_gui_frontend_services=>registry_set_value( EXPORTING root = cl_gui_frontend_services=>hkey_current_user
  key = key
  value_name = value_name
  value = value
IMPORTING rc = rc
EXCEPTIONS cntl_error = 1
  error_no_gui = 2
  not_supported_by_gui = 3
  ).
  cl_gui_cfw=>flush( EXCEPTIONS OTHERS = 0 ) .


ENDFORM.
