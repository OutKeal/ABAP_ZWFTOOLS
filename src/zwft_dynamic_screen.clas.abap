class ZWFT_DYNAMIC_SCREEN definition
  public
  final
  create public .

public section.

  types:
    tt_d021s TYPE TABLE OF d021s .
  types:
    BEGIN OF ty_default_field_list,
        type  TYPE char5,
        d021s TYPE d021s,
      END OF ty_default_field_list .
  types:
    tt_d022s TYPE TABLE OF d022s .
  types:
    tt_d023s TYPE TABLE OF d023s .

  data DYNPRO_HEADER type D020S .
  data FIELD_LIST type TT_D021S .
  data FLOW_LOGIC type TT_D022S .
  data MATCHCODE type TT_D023S .
  data MESSAGE type TEXT240 .
  data LINES type I .
  data WORD type CHAR72 .
  data:
    BEGIN OF dynp_id,
        prog TYPE d020s-prog,
        dnum TYPE d020s-dnum,
      END OF dynp_id .
  constants FIELD_TYPE_NAME type CHAR5 value 'NAME' ##NO_TEXT.
  constants FIELD_TYPE_CHAR type CHAR5 value 'CHAR' ##NO_TEXT.
  constants FIELD_TYPE_QUAN type CHAR5 value 'QUAN' ##NO_TEXT.
  constants FIELD_TYPE_CURR type CHAR5 value 'CURR' ##NO_TEXT.
  constants FIELD_TYPE_DATS type CHAR5 value 'DATS' ##NO_TEXT.
  constants FIELD_TYPE_TIMS type CHAR5 value 'TIMS' ##NO_TEXT.
  constants FIELD_TYPE_FRAME type CHAR5 value 'FRAME' ##NO_TEXT.
  data DEFAULT_TEXT_SHIFT type I value 10 ##NO_TEXT.
  data MAX_COL type I value 3 ##NO_TEXT.
  data CURRENT_COLN type I value 1 ##NO_TEXT.
  data MAX_WIDTH type I .
  data MAX_COL_WIDTH type I value 35 ##NO_TEXT.
  data CURRENT_COL_WIDTH type I .
  data TEXT_LENG type I value 6 ##NO_TEXT.
  data MAX_INPUT_LENG type I value 25 ##NO_TEXT.
  data FRAME_INDEX type I .
  data CURRENT_LINE type LINE_____2 value '01' ##NO_TEXT.
  data CURRENT_COL type COLN_____4 value '03' ##NO_TEXT.
  data:
    BEGIN OF col_pos ,
            col1 TYPE coln_____4,
            col2 TYPE coln_____4,
            col3 TYPE coln_____4,
            col4 TYPE coln_____4,
            col5 TYPE coln_____4,
            col6 TYPE coln_____4,
            col7 TYPE coln_____4,
            col8 TYPE coln_____4,
            col9 TYPE coln_____4,
          END OF col_pos .

  methods CONSTRUCTOR
    importing
      !PROG type PROG
      !DYNNR type DYNNR
      !TYPE type SYCHAR01 default ' '
      !LINE type SCRHUSEL optional
      !COL type SCRHUSEC optional .
  methods ADD_FIELD
    importing
      !NAME type FIELDNAME
      !TEXT type STXT_____1 optional
      !TEXT_NAME type FIELDNAME optional
      !NEWLINE type ABAP_BOOL default ABAP_FALSE
      !CHAIN type ABAP_BOOL default ABAP_FALSE
      !F4 type ABAP_BOOL default ABAP_FALSE
      !LENG type I optional .
  methods ADD_INPUT_FIELD
    importing
      !NAME type FIELDNAME
      !NEWLINE type ABAP_BOOL default ABAP_FALSE
      !CHAIN type ABAP_BOOL default ABAP_FALSE
      !F4 type ABAP_BOOL default ABAP_FALSE
      !LENG type I optional
      !INPUT type ABAP_BOOL default ABAP_TRUE .
  methods ADD_FRAME
    importing
      !TEXT type STXT_____1
      !HEIGHT type I optional .
  methods SET_FLOW_LOGIC
    importing
      !PBO type TT_D022S optional
      !PAI type TT_D022S optional .
  methods SET_MATCHCODE .
  methods GENERATE
    importing
      !SAVE type ABAP_BOOL optional .
  class-methods CREATE
    importing
      !PROG type PROG
      !DYNNR type DYNNR
      !LINE type SCRHUSEL optional
      !TYPE type SYCHAR01 default ' '
      !COL type SCRHUSEC optional
    returning
      value(SCREEN) type ref to ZWFT_DYNAMIC_SCREEN .
  methods ADD_FLOW_LOGIC_CHAIN
    importing
      !NAME type FIELDNAME
      !CODE type DYNTXLINE optional .
  methods ADD_FLOW_LOGIC_F4
    importing
      !NAME type FIELDNAME
      !CODE type DYNTXLINE optional .
  methods SET_MAX_COL .
  methods SET_COL_POS .
  methods MOVE_RIGHT
    importing
      value(OFFSET) type I optional .
  methods MOVE_DOWN
    importing
      value(OFFSET) type I optional .
protected section.
private section.

  data FIELD_TYPE_CHAR_OUTPUT type CHAR10 value 'OUTPUT' ##NO_TEXT.
  data:
    default_field_list TYPE TABLE OF ty_default_field_list .
  data FCOUNT type I .
  data NEXT_END_LINE type LINE_____2 .
  data LAST_FRAME type FNAM_____4 .
  constants CHAIN_FLAG type CHAR50 value '*CHAIN_FLAG' ##NO_TEXT.
  constants F4_FLAG type CHAR50 value '*F4_FLAG' ##NO_TEXT.

  methods GET_DEFAULT_FIELD_LIST
    importing
      !FIELD_TYPE type CLIKE
    returning
      value(FIELD_LIST) type D021S .
  methods SET_DEFAULT_FIELD_LIST .
  methods SET_FIELD_LENG
    importing
      !I_DDIC type DFIES
      !I_LENG type I
    returning
      value(R_LENG) type I .
  methods GET_NEXT_COL
    returning
      value(COL) type COLN_____4 .
ENDCLASS.



CLASS ZWFT_DYNAMIC_SCREEN IMPLEMENTATION.


  METHOD add_field.
    DATA fieldname TYPE char100.
    DATA elemdescr TYPE REF TO cl_abap_elemdescr.
    current_col_width = max_col_width.
    IF newline = abap_true.
      move_down( 1 ).
    ENDIF.
    fieldname = |({ dynp_id-prog }){ name }|.
    ASSIGN (fieldname) TO FIELD-SYMBOL(<fieldname>).
    CHECK sy-subrc EQ 0.
    current_col = get_next_col( ).
    elemdescr ?= cl_abap_typedescr=>describe_by_data( <fieldname> ).
    DATA(ddic) = elemdescr->get_ddic_field( ).
    APPEND VALUE #( BASE get_default_field_list( field_type_name )
                                  fnam = name
                                  line = current_line
                                  coln = current_col
                                  leng = text_leng
                                  stxt = COND #( WHEN text IS NOT INITIAL THEN text ELSE ddic-fieldtext )
                                  ) TO field_list.
    move_right( default_text_shift ) .
    current_col_width -= default_text_shift.

    DATA(l_leng) = set_field_leng( EXPORTING i_ddic = ddic i_leng = leng ).
    APPEND VALUE #( BASE get_default_field_list( ddic-datatype )
                                    fnam = name
                                    fmb2 = COND #( WHEN ddic-f4availabl = 'X' THEN '18' ELSE '' )
                                    line = current_line
                                    coln = current_col
                                    leng = l_leng
                                    ucnv = ddic-convexit
                                    aglt = ddic-outputlen
                                    adez = ddic-decimals
                                    stxt = ''
                                    ) TO field_list.

    IF text_name IS NOT INITIAL.
      DATA(text_col) = current_col + l_leng + 1.
      fieldname = |({ dynp_id-prog }){ text_name }|.
      ASSIGN (fieldname) TO <fieldname>.
      elemdescr ?= cl_abap_typedescr=>describe_by_data( <fieldname> ).
      ddic = elemdescr->get_ddic_field( ).
      DATA(text_leng) = set_field_leng( EXPORTING i_ddic = ddic i_leng = leng ) .
      APPEND VALUE #( BASE get_default_field_list( ddic-datatype )
                                      fnam = text_name
                                      fmb1 = '30'
                                      line = current_line
                                      coln = text_col
                                      leng = text_leng
                                      ucnv = ddic-convexit
                                      aglt = ddic-outputlen
                                      adez = ddic-decimals
                                      stxt = ''
                                      ) TO field_list.
      l_leng += text_leng + 1.
    ENDIF.
    IF l_leng > current_col_width.
      current_coln += 1.
      fcount += 1.
      move_right( max_col_width + current_col_width ) .
      IF ( current_col + 0 ) > ( max_col_width * ( max_col - 1 ) ).
        move_down( 1 ).
      ELSE.
      ENDIF.
    ELSE.
      current_col_width -= l_leng.
      ADD 1 TO fcount.
      move_right( current_col_width ) .
      IF fcount MOD max_col = 0.
        move_down( 1 ).
      ENDIF.
    ENDIF.


    IF chain = abap_true.
      add_flow_logic_chain( name ).
    ENDIF.
    IF f4 = abap_true.
      add_flow_logic_f4( name ).
    ENDIF.


  ENDMETHOD.


  METHOD add_flow_logic_chain.
    READ TABLE flow_logic TRANSPORTING NO FIELDS WITH KEY line = chain_flag.
    CHECK sy-subrc EQ 0.
    DATA(tabix) = sy-tabix .
    IF code IS INITIAL.
      INSERT |FIELD { name } MODULE { name } ON CHAIN-REQUEST.| INTO flow_logic INDEX tabix.
    ELSE.
      INSERT code INTO flow_logic INDEX tabix.
    ENDIF.
  ENDMETHOD.


  METHOD ADD_FLOW_LOGIC_F4.
    READ TABLE flow_logic TRANSPORTING NO FIELDS WITH KEY line = f4_flag.
    CHECK sy-subrc EQ 0.
    DATA(tabix) = sy-tabix - 1.
    IF code IS INITIAL.
      INSERT |FIELD { name } MODULE { name }_F4.| INTO flow_logic INDEX tabix.
    ELSE.
      INSERT CODE INTO flow_logic INDEX tabix.
    ENDIF.
  ENDMETHOD.


  METHOD add_frame.
    IF current_line <> '01'.
      move_down( 1 ).
    ENDIF.
    ADD 1 TO frame_index.
    last_frame = 'FRAME' && frame_index.
    APPEND VALUE #( BASE get_default_field_list( field_type_frame )
                                    didx = height - 1
                                    fnam = last_frame
                                    line = current_line
                                    coln = '02'
                                    leng = max_width
                                    stxt = text
                                  ) TO field_list.
    IF NOT height IS INITIAL.
      next_end_line = current_line + height + 1.
    ELSE.
      CLEAR next_end_line.
    ENDIF.

    move_down( 1 ).

  ENDMETHOD.


  METHOD add_input_field.
    DATA fieldname TYPE char100.
    DATA elemdescr TYPE REF TO cl_abap_elemdescr.
    DATA l_leng TYPE i.
    IF newline = abap_true.
      move_down( 1 ).
    ENDIF.
    fieldname = |({ dynp_id-prog }){ name }|.
    ASSIGN (fieldname) TO FIELD-SYMBOL(<fieldname>).
    CHECK sy-subrc EQ 0.
    elemdescr ?= cl_abap_typedescr=>describe_by_data( <fieldname> ).
    DATA(ddic) = elemdescr->get_ddic_field( ).
    l_leng = COND #(  WHEN leng > 2 * max_input_leng THEN 2 * max_input_leng
                                  WHEN leng IS NOT INITIAL THEN leng
                                  WHEN ddic-leng > max_input_leng THEN max_input_leng
                                  ELSE ddic-leng ).
    APPEND VALUE #( BASE get_default_field_list( ddic-datatype )
                                      fnam = name
                                      fmb1 = COND #( WHEN input = abap_true THEN '30' ELSE '31' )
                                      fmb2 = COND #( WHEN f4 = abap_true THEN '18'
                                                                  WHEN ddic-f4availabl = 'X' THEN '18' ELSE '' )
                                      line = current_line
                                      coln = current_col
                                      leng = l_leng
                                      ucnv = ddic-convexit
                                      adez = ddic-decimals
                                      stxt = '__________'
                                      ) TO field_list.
    move_right( l_leng + 1 ) .

*    IF fcount MOD max_col = 0.
*      move_down( 1 ).
*    ENDIF.

    IF chain = abap_true.
      add_flow_logic_chain( name ).
    ENDIF.
*    IF f4 = abap_true.
*      add_flow_logic_f4( name ).
*    ENDIF.


  ENDMETHOD.


  METHOD constructor.
    dynp_id-prog = prog.
    dynp_id-dnum = dynnr.

    dynpro_header-prog = prog.
    dynpro_header-dnum = dynnr.
    dynpro_header-fnum = dynnr.
    dynpro_header-type = type.
    dynpro_header-mili = 192.
    dynpro_header-mico = 37.
    dynpro_header-noli = COND #( WHEN line IS INITIAL THEN 200 ELSE line ).
    dynpro_header-noco = COND #( WHEN col IS INITIAL THEN 200 ELSE col )..
    dynpro_header-cuan = 'G'.
    dynpro_header-spra = sy-langu.
    dynpro_header-dgen = sy-datum.
    dynpro_header-tgen = sy-uzeit.

    set_default_field_list( ).
    set_col_pos( ).
    zwft_common=>get_screen_xy_point( IMPORTING charx = max_width ).
    set_max_col( ).
  ENDMETHOD.


  METHOD create.
    screen = NEW zwft_dynamic_screen( prog = prog
                                                               dynnr = dynnr
                                                               line = line
                                                               col = col
                                                               type = type ).
  ENDMETHOD.


  METHOD generate.
    DELETE DYNPRO dynp_id.

    LOOP AT field_list ASSIGNING FIELD-SYMBOL(<field_list>) WHERE fill = 'R'.
    ENDLOOP.
    IF sy-subrc EQ 0.
      ADD 1 TO <field_list>-didx .
    ENDIF.

    IF save = 'X'.
      CALL FUNCTION 'RPY_DYNPRO_INSERT_NATIVE'
        EXPORTING
          header             = dynpro_header
          dynprotext         = 'Generated dynpro'
        TABLES
          fieldlist          = field_list
          flowlogic          = flow_logic
          params             = matchcode
        EXCEPTIONS
          cancelled          = 1
          already_exists     = 2
          program_not_exists = 3
          not_executed       = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.

      ENDIF.
    ENDIF.
    GENERATE DYNPRO dynpro_header field_list flow_logic matchcode ID dynp_id
    MESSAGE message
    LINE lines WORD word.
    IF message IS NOT INITIAL.
      MESSAGE e000(zafo) WITH message.
    ENDIF.

    IF save = 'X'.
      CALL FUNCTION 'RS_WORKING_AREA_ACTIVE_CHECK'
        EXCEPTIONS
          nok    = 1
          OTHERS = 2.
      IF sy-subrc = 0.
        DATA lv_obj_name TYPE e071-obj_name.
        lv_obj_name = dynp_id.
        CALL FUNCTION 'RS_WORKING_AREA_INIT'.
        CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
          EXPORTING
            object   = 'DYNP'
            obj_name = lv_obj_name
          EXCEPTIONS
            OTHERS   = 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD GET_DEFAULT_FIELD_LIST.
    READ TABLE default_field_list WITH KEY type = field_type INTO DATA(ls_list).
    IF sy-subrc EQ 0.
      field_list = ls_list-d021s.
      RETURN.
    ELSE.
      field_list = default_field_list[ type = field_type_char ]-d021s.
    ENDIF.
  ENDMETHOD.


  METHOD get_next_col.
    DATA(fname) = |COL_POS-COL{ current_coln }|.
    ASSIGN (fname) TO FIELD-SYMBOL(<col>).
    CHECK sy-subrc EQ 0.
    if col + 0 > max_width.
      move_down( 1 ).
    endif.
    col = <col>.
    current_coln += 1.
  ENDMETHOD.


  METHOD move_down.
    FIELD-SYMBOLS  : <x>  TYPE  x .
    ASSIGN current_line TO <x> CASTING.
    ADD offset TO <x> .
    current_line = <x>.
    current_col = '03'.
    current_coln = 1.

    IF current_line = next_end_line.
      CLEAR next_end_line.
      move_down( 1 ).
    ELSEIF next_end_line = 0 .
      READ TABLE field_list WITH KEY fnam = last_frame ASSIGNING FIELD-SYMBOL(<flist>).
      IF sy-subrc EQ 0.
        <flist>-didx = current_line - <flist>-line + 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD MOVE_RIGHT.
    FIELD-SYMBOLS  : <x>  TYPE  x .
    ASSIGN current_col TO <x> CASTING.
    ADD offset TO <x> .
    current_col = <x>.
  ENDMETHOD.


  METHOD set_col_pos.
    FIELD-SYMBOLS  : <fvalue>  TYPE  x .
    FIELD-SYMBOLS  : <tvalue>  TYPE  x .
    col_pos-col1 = '03'.
    DO 9 TIMES.
      CHECK sy-index > 1.
      DATA(fname) = |COL_POS-COL{ sy-index - 1 }|.
      ASSIGN (fname) TO <fvalue> CASTING .
      DATA(tname) = |COL_POS-COL{ sy-index }|.
      ASSIGN (tname) TO <tvalue> CASTING.
      <tvalue> = <fvalue> + max_col_width.
    ENDDO.

  ENDMETHOD.


  METHOD SET_DEFAULT_FIELD_LIST.
    APPEND VALUE #( type = field_type_name"文本 TEXT
    d021s = VALUE #( fmb1 = '30'
    leng = text_leng
    type = 'CHAR'
    ityp = 'C'
    )
    ) TO default_field_list.

    APPEND VALUE #( type = field_type_char"文本 TEXT
    d021s = VALUE #( flg1 = '80'
    flg3 = '80'
    type = 'CHAR'
    ityp = 'C'
*                                  stxt = '__'
    )
    ) TO default_field_list.

    APPEND VALUE #( type = field_type_frame"文本 TEXT
    d021s = VALUE #( fill = 'R'
    fmb1 = '30'
    type = 'CHAR'
    auth = '101'
    )
    ) TO default_field_list.

    APPEND VALUE #( type = field_type_quan
    d021s = VALUE #( flg1 = '80'
    flg3 = '80'
    type = 'DEC'
    ityp = 'P'
    )
    ) TO default_field_list.

    APPEND VALUE #( type = field_type_curr
    d021s = VALUE #( flg1 = '80'
    flg3 = '80'
    type = 'DEC'
    ityp = 'P'
    )
    ) TO default_field_list.
    APPEND VALUE #( type = field_type_dats
    d021s = VALUE #( flg1 = '80'
    flg3 = '80'
    leng = '0A'
    type = 'DATS'
    ityp = 'D'
    )
    ) TO default_field_list.

    APPEND VALUE #( type = field_type_tims
    d021s = VALUE #( flg1 = '80'
    flg3 = '80'
    leng = '08'
    type = 'TIMS'
    ityp = 'T'
    )
    ) TO default_field_list.
  ENDMETHOD.


  METHOD SET_FIELD_LENG.
    IF i_leng IS NOT INITIAL.
      IF i_leng > 2 * max_input_leng .
        r_leng = 2 * max_input_leng.
      ELSE.
        r_leng = i_leng.
      ENDIF.
      RETURN.
    ENDIF.

    READ TABLE default_field_list WITH KEY type = i_ddic-datatype INTO DATA(ls_list).
    IF sy-subrc EQ 0 AND ls_list-d021s-leng IS NOT INITIAL.
      r_leng = ls_list-d021s-leng.
      RETURN.
    ENDIF.

    IF i_ddic-leng > max_input_leng.
      r_leng = max_input_leng.
    ELSE.
      r_leng = i_ddic-leng.
    ENDIF.

  ENDMETHOD.


  METHOD SET_FLOW_LOGIC.
    CLEAR flow_logic.
    APPEND `PROCESS BEFORE OUTPUT.`  TO flow_logic.
    IF pbo IS INITIAL.
      APPEND |  MODULE STATUS_{ dynp_id-dnum }.| TO flow_logic.
    ELSE.
      APPEND LINES OF pbo TO flow_logic.
    ENDIF.

    APPEND 'PROCESS AFTER INPUT.' TO  flow_logic.
    APPEND '  CHAIN.' TO  flow_logic.
    APPEND chain_flag TO  flow_logic.
    APPEND '  ENDCHAIN.' TO  flow_logic.

    IF pai IS INITIAL.
      APPEND |  MODULE exit_command AT EXIT-COMMAND.| TO flow_logic.
      APPEND |  MODULE USER_COMMAND_{ dynp_id-dnum }.| TO flow_logic.
    ELSE.
      APPEND LINES OF pai TO flow_logic.
    ENDIF.

    APPEND 'PROCESS ON VALUE-REQUEST.' TO flow_logic.
    APPEND f4_flag TO flow_logic.
  ENDMETHOD.


  METHOD SET_MATCHCODE.
    matchcode = VALUE #( ( type = '' content = '' ) ).
  ENDMETHOD.


  METHOD set_max_col.
    max_col = max_width DIV ( max_col_width ).
  ENDMETHOD.
ENDCLASS.
