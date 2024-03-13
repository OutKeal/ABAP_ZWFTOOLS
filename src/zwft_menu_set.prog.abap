*&---------------------------------------------------------------------*
*& Report ZWFT_MENU_SET
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_menu_set.
TABLES:bmenuname.
DATA: l_list_of_refs LIKE hier_ref OCCURS 0 .
DATA: l_list_of_texts LIKE hier_texts OCCURS 0 .
DATA: l_list_of_nodes LIKE hier_iface OCCURS 0 .
DATA:lt_menu TYPE TABLE OF zwft_menu.
DATA: l_msg LIKE hier_mess.

PARAMETERS: p_id LIKE  ttree-id.

START-OF-SELECTION.

  CALL FUNCTION 'STREE_HIERARCHY_READ'
    EXPORTING
      structure_id       = p_id
      filter_id          = ' '
      read_also_texts    = 'X'
      language           = sy-langu
    IMPORTING
      message            = l_msg
    TABLES
      list_of_nodes      = l_list_of_nodes[]
      list_of_references = l_list_of_refs[]
      list_of_texts      = l_list_of_texts[].
*     USER_PARAMETERS    =

  DATA(tcode_group) = |ZMENU_{ p_id+1 }|.
  DATA group_name LIKE zwft_menu-group_name.
  DATA num LIKE zwft_menu-num.
  DATA button_css LIKE zwft_menu-button_css.
  READ TABLE l_list_of_nodes INTO DATA(basis) WITH KEY node_type = '01'.

  num = 0.
  button_css = 0.
  PERFORM frm_froech_node USING basis-node_id ''.
  MODIFY zwft_menu FROM TABLE  lt_menu.
  COMMIT WORK AND WAIT.

  zwft_falv=>create( CHANGING ct_table = lt_menu )->display( ).

FORM frm_froech_node USING parent_id brother_id."遍历节点
  READ TABLE l_list_of_nodes INTO DATA(l_node) WITH KEY parent_id = parent_id brother_id = brother_id.
  CHECK sy-subrc EQ 0.
  IF l_node-node_type = 'NULL'.
    PERFORM frm_froech_node USING l_node-node_id ''."遍历子节点
  ENDIF.

  IF l_node-node_type = 'TCOD'.
    CLEAR group_name.
    num += 1.
    button_css += 1.
    IF button_css > 16.
      button_css = 1.
    ENDIF.
    PERFORM frm_get_parent_name USING l_node-parent_id CHANGING group_name."获取上级的组名称
    READ TABLE l_list_of_refs INTO DATA(l_ref) WITH KEY node_id = l_node-node_id ref_type = 'TCOD'.
    CHECK sy-subrc EQ 0.
    READ TABLE l_list_of_texts INTO DATA(l_text) WITH KEY node_id = l_node-node_id.
    CHECK sy-subrc EQ 0.
    APPEND VALUE #( tcode_group = tcode_group
                                    num = num
                                    group_name = group_name
                                    tcode = l_ref-ref_object
                                    name = l_text-text
                                    button_css = button_css
                                  ) TO lt_menu.
  ENDIF.
  PERFORM frm_froech_node USING l_node-parent_id l_node-node_id."遍历兄弟节点
ENDFORM.



*LOOP AT l_list_of_nodes WHERE node_type = 'TCOD' AND brother_id IS INITIAL.
*  CLEAR group_name.
*  PERFORM frm_get_parent_name USING l_list_of_nodes-parent_id CHANGING group_name.
*ENDLOOP.

FORM frm_get_parent_name USING parent_id CHANGING group_name.
  READ TABLE l_list_of_texts INTO DATA(l_text) WITH KEY node_id = parent_id.
  IF sy-subrc EQ 0.
    IF group_name IS INITIAL.
      group_name = l_text-text.
    ELSE.
      group_name = l_text-text && '-' && group_name.
    ENDIF.
  ENDIF.

  READ TABLE l_list_of_nodes INTO DATA(l_node) WITH KEY node_id = parent_id.
  IF sy-subrc EQ 0.
    IF l_node-parent_id IS NOT INITIAL.
      PERFORM frm_get_parent_name USING l_node-parent_id CHANGING group_name.
    ENDIF.
  ENDIF.
ENDFORM.
