*&---------------------------------------------------------------------*
*& Report ZWFT_MENU
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_menu.

DATA io_html TYPE REF TO zwft_html.

START-OF-SELECTION.

  CALL SCREEN 100.

MODULE status_0100 OUTPUT.
  CHECK io_html IS INITIAL.

  SET TITLEBAR '100'.
  io_html = NEW zwft_html( html_temp = 'ZWFT_MENU_HTML'
  css = 'ZWFT_BUTTON_CSS'
  js = 'ZWFT_MENU_JS'
  ).
  PERFORM frm_get_tcode_info .
  io_html->build_html( ).
  io_html->html_viewer_show( i_parent = NEW cl_gui_custom_container( container_name = 'VIEW') ).
ENDMODULE.


MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

FORM frm_get_tcode_info .

  DATA lt_html TYPE cl_abap_browser=>html_table.
  IF sy-tcode = 'SE38' OR sy-tcode = 'ZMENU'.
    SELECT * FROM zwft_menu
    INTO TABLE @DATA(lt_menu).
  ELSE.
    SELECT * FROM zwft_menu WHERE tcode_group = @sy-tcode
    INTO TABLE @lt_menu.

  ENDIF.

  CHECK sy-subrc EQ 0.
  SORT lt_menu BY tcode_group num.

  LOOP AT lt_menu INTO DATA(g_menu)
        GROUP BY ( group_name = g_menu-group_name )
  INTO DATA(group_menu).
    APPEND |<div class="frame">| TO lt_html.
    APPEND |<h2 class="group">{ group_menu-group_name }</h2>| TO lt_html.
    LOOP AT GROUP group_menu INTO DATA(ls_menu).
      APPEND INITIAL LINE TO lt_html ASSIGNING FIELD-SYMBOL(<line>).
      <line> = zwft_html=>set_button(
      class = |custom-btn btn-{ COND #( WHEN ls_menu-button_css IS INITIAL THEN '12' ELSE ls_menu-button_css ) }|
      onclick = |form_submit('{ ls_menu-tcode }')|
      name1 = ls_menu-name
      name2 = COND #( WHEN ls_menu-button_css = 12 THEN ls_menu-tcode ELSE '' )
      ).
    ENDLOOP.
    APPEND |</div>| TO lt_html.
  ENDLOOP.
  io_html->add_html( flag = '    <!-- body -->' html = lt_html ).
ENDFORM.


FORM frm_html_callback USING callback TYPE zwft_html=>callback.
  CHECK callback IS NOT INITIAL.
  CHECK callback-query_table IS NOT INITIAL.
  READ TABLE callback-query_table INTO DATA(l_query) INDEX 1.
  CALL TRANSACTION l_query-value.

ENDFORM.
