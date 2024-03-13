*&---------------------------------------------------------------------*
*& Report zwft_table_upload_mass
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_table_upload_mass.

TABLES dd03l.
FIELD-SYMBOLS: <dyn_table> TYPE table,
               <dyn_wa>    TYPE any,
               <txt_field> TYPE any,
               <val_field> TYPE any.
DATA: dyn_table TYPE REF TO data.
DATA: dyn_wa TYPE REF TO data.
*声明变量
DATA:  lv_path TYPE string.

*获取表名和路径
SELECT-OPTIONS: s_tab FOR dd03l-tabname .
PARAMETERS:p_path TYPE string.

*将输入的表名存入内表
SELECT tabname
INTO TABLE @DATA(lt_tabname)
      FROM dd02l
      WHERE tabname IN @s_tab.

CHECK sy-subrc EQ 0.
*循环处理每个表
LOOP AT lt_tabname INTO DATA(lv_tabname).

*创建动态表结构
  CREATE DATA dyn_table TYPE TABLE OF (lv_tabname-tabname).
*创建动态内表
  ASSIGN dyn_table->* TO <dyn_table>.
*创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
*创建动态工作区
  ASSIGN dyn_wa->* TO <dyn_wa>.

  lv_path = p_path && '/' && lv_tabname-tabname && '.csv'.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_path
      filetype                = 'ASC'
      has_field_separator     = '#'
    CHANGING
      data_tab                = <dyn_table>
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  MODIFY (lv_tabname-tabname) FROM TABLE <dyn_table>.
  COMMIT WORK.
ENDLOOP.
