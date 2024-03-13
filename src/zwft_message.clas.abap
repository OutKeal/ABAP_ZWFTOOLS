class ZWFT_MESSAGE definition
  public
  final
  create public .

public section.

  data T_RETURN type BAPIRET2_T .

  methods CONSTRUCTOR .
  methods ADD_SINGLE
    importing
      value(MSGTY) type MSGTY
      value(MSGID) type MSGID
      value(MSGNO) type MSGNO
      value(MSGV1) type CLIKE optional
      value(MSGV2) type CLIKE optional
      value(MSGV3) type CLIKE optional
      value(MSGV4) type CLIKE optional .
  methods ADD_LINE
    importing
      value(IS_RETURN) type BAPIRET2 .
  methods ADD_TABLE
    importing
      value(IT_RETURN) type BAPIRET2_T .
  methods ADD_SINGLE_SY .
  methods GET_RETURN
    returning
      value(ET_RETURN) type BAPIRET2_T .
  methods GET_ERROR
    returning
      value(ERROR) type ABAP_BOOL .
  methods POP_ALL_MSG
    importing
      value(REFRESH) type ABAP_BOOL optional .
  methods POP_MSG
    importing
      value(REFRESH) type ABAP_BOOL optional .
  methods FLUSH .
protected section.
private section.
ENDCLASS.



CLASS ZWFT_MESSAGE IMPLEMENTATION.


  METHOD ADD_LINE.
    APPEND is_return TO t_return.
  ENDMETHOD.


  METHOD ADD_SINGLE.
    APPEND VALUE #( type = msgty
                                  id = msgid
                                  number = msgno
                                  message_v1 = msgv1
                                  message_v2 = msgv2
                                  message_v3 = msgv3
                                  message_v4 = msgv4
                                ) TO t_return.
  ENDMETHOD.


  METHOD ADD_TABLE.
    LOOP AT it_return INTO DATA(is_return).
      APPEND is_return TO me->t_return.
    ENDLOOP.
  ENDMETHOD.


  METHOD CONSTRUCTOR.
    CLEAR t_return.
  ENDMETHOD.


  METHOD FLUSH.
    CLEAR t_return.
  ENDMETHOD.


  METHOD GET_ERROR.
    error = abap_false.
    LOOP AT t_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
      error = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_RETURN.
    et_return = t_return.
  ENDMETHOD.


  METHOD pop_msg.
    CHECK t_return IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM t_return.
    IF lines( t_return ) = 1.
      READ TABLE t_return INTO DATA(l_return) INDEX 1 .

      MESSAGE ID l_return-id TYPE 'S'
      NUMBER l_return-number
      WITH l_return-message_v1
       l_return-message_v2
       l_return-message_v3
       l_return-message_v4 DISPLAY LIKE l_return-type.
      IF refresh EQ abap_true.
        flush( ).
      ENDIF.
      RETURN.
    ENDIF.
    DATA(log) = NEW cl_isu_error_log( ).
    LOOP AT t_return INTO l_return WHERE type CA 'EAX'.
      log->add_message( x_msgid = l_return-id
                                         x_msgty = l_return-type
                                         x_msgno = l_return-number
                                         x_msgv1 = l_return-message_v1
                                         x_msgv2 = l_return-message_v2
                                         x_msgv3 = l_return-message_v3
                                         x_msgv4 = l_return-message_v4 ).
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT t_return INTO l_return .
        log->add_message( x_msgid = l_return-id
        x_msgty = l_return-type
        x_msgno = l_return-number
        x_msgv1 = l_return-message_v1
        x_msgv2 = l_return-message_v2
        x_msgv3 = l_return-message_v3
        x_msgv4 = l_return-message_v4 ).
      ENDLOOP.
    ENDIF.
    log->display_messages( ).

    IF refresh EQ abap_true.
      flush( ).
    ENDIF.

  ENDMETHOD.


  METHOD add_single_sy.
    APPEND VALUE #( type = sy-msgty
                                  id = sy-msgid
                                  number = sy-msgno
                                  message_v1 = sy-msgv1
                                  message_v2 = sy-msgv2
                                  message_v3 = sy-msgv3
                                  message_v4 = sy-msgv4
                                  ) TO t_return.
  ENDMETHOD.


  METHOD pop_all_msg.

    DATA:lt_message TYPE TABLE OF esp1_message_wa_type .
    DATA:it_message TYPE TABLE OF esp1_message_wa_type .

    CHECK t_return IS NOT INITIAL.

    LOOP AT t_return INTO DATA(is_return).
      APPEND VALUE #( msgty = is_return-type
      msgid = is_return-id
      msgno = is_return-number
      msgv1 = is_return-message_v1
      msgv2 = is_return-message_v2
      msgv3 = is_return-message_v3
      msgv4 = is_return-message_v4   ) TO lt_message.
    ENDLOOP.

    LOOP AT lt_message INTO DATA(is_message) .
      APPEND is_message TO it_message.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = it_message[].
    ELSE.
      IF lines( lt_message ) = 1.
        DATA(message_line) = lt_message[ 1 ].
        MESSAGE ID message_line-msgid TYPE message_line-msgty
          NUMBER message_line-msgno
          WITH message_line-msgv1 message_line-msgv2 message_line-msgv3 message_line-msgv4.
      ELSE.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_message[].
      ENDIF.
    ENDIF.

    IF refresh EQ abap_true.
      flush( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
