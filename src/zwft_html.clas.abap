class ZWFT_HTML definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_html_type,
        html_type TYPE char20,
        html      TYPE w3html_tab,
      END OF ty_html_type .
  types:
    BEGIN OF callback,
             action      TYPE string,
             frame       TYPE string,
             getdata     TYPE string,
             postdata    TYPE cnht_post_data_tab,
             query_table TYPE  rrxw3tquery,
           END OF callback .

  data HTML_TAB type CL_ABAP_BROWSER=>HTML_TABLE .

  events SAPEVENT
    exporting
      value(ACTION) type C
      value(FRAME) type C
      value(GETDATA) type C
      value(POSTDATA) type CNHT_POST_DATA_TAB
      value(QUERY_TABLE) type CNHT_QUERY_TABLE .

  methods CONSTRUCTOR
    importing
      !HTML_TEMP type W3OBJID default 'ZWFT_HTML_TEMP'
      !CSS type W3OBJID optional
      !IMAGE type W3OBJID optional
      !JS type W3OBJID optional .
  class-methods DISPLAY_FALV
    importing
      !FALV type ref to ZWFT_FALV
    returning
      value(R_HTML) type ref to ZWFT_HTML .
  methods SET_TITLE
    importing
      !TITLE type CLIKE .
  methods ADD_IMAGE
    importing
      !PIC type W3OBJID optional .
  methods ADD_JS
    importing
      !HTML_JS type W3OBJID .
  methods ADD_STYLE
    importing
      !HTML_CSS type W3OBJID .
  methods ADD_HEADER_LABEL
    importing
      !LABEL type LABEL default 'span'
      !NAME type CLIKE optional
      !VALUE type ANY .
  methods ADD_BOTTOM_LABEL
    importing
      !LABEL type LABEL default 'span'
      !NAME type CLIKE optional
      !VALUE type ANY .
  methods ADD_TABLE
    importing
      !I_TITLE type CLIKE optional
      !IT_FCAT type LVC_T_FCAT
      !IT_TABLE type STANDARD TABLE .
  methods ADD_HTML
    importing
      !FLAG type ANY
      !HTML type CL_ABAP_BROWSER=>HTML_TABLE .
  class-methods SET_BUTTON
    importing
      !CLASS type ANY optional
      !ONCLICK type ANY optional
      !NAME1 type ANY
      !NAME2 type ANY optional
    returning
      value(LINE) type W3_HTML .
  methods BUILD_HTML .
  methods HTML_SHOW .
  methods HTML_VIEWER_SHOW
    importing
      !I_PARENT type ref to CL_GUI_CONTAINER optional
      !I_REPID type SY-REPID optional .
  methods ON_SAPEVENT
    for event SAPEVENT of CL_GUI_HTML_VIEWER
    importing
      !ACTION
      !FRAME
      !GETDATA
      !POSTDATA
      !QUERY_TABLE .
  class-methods GET_HTML_TEMP
    importing
      !I_OBJID type ANY
    returning
      value(R_HTML) type W3HTML_TAB .
  class-methods GET_HTML_MIME
    importing
      !I_OBJID type ANY
    returning
      value(R_MIME) type LVC_T_MIME .
  class-methods CONV_ICON_TO_EMOJI
    importing
      !I_VALUE type ANY
    returning
      value(R_VALUE) type STRING .
  class-methods CONV_EXIT
    importing
      !I_FCAT type LVC_S_FCAT optional
      !I_VALUE type ANY
    returning
      value(R_VALUE) type TEXT50 .
  class-methods SHOW_PICTURE
    importing
      !IMAGE_XSTRING type XSTRING .
  class-methods GET_WWWDATA_URL
    importing
      !I_RELID type CHAR2 default 'MI'         "ANY" "MI"
      !I_OBJECT type ANY
      !I_URL_TYPE type CHAR10 default 'TEXT'
      !I_URL_SUBTYPE type CHAR10 default 'HTML'
    returning
      value(R_URL) type CHAR255 .
  class-methods GET_LOCOL_DATA
    importing
      !FILE type STRING
    returning
      value(DATA) type W3HTML_TAB .
  methods GET_LOCOL_URL
    importing
      !I_FILE type STRING
      !I_TYPE type CHAR10
      !I_SUBTYPE type CHAR10
    returning
      value(R_URL) type CHAR255 .
protected section.
private section.

  data:
    html_type_tab TYPE TABLE OF ty_html_type .
  data HTML_TEMPLATE type SYCHAR40 .
  data HTML_STR type STRING .
  data REPID type SY-REPID .

  methods SET_TEMP
    importing
      !HTML_TEMP type W3OBJID .
  methods GET_INSERT_INDEX
    importing
      !FLAG type CLIKE
    returning
      value(TABIX) type SY-TABIX .
  methods INSERT_HTML_TAB
    importing
      !I_FLAG type CLIKE
      !IT_HTML type W3HTML_TAB .
ENDCLASS.



CLASS ZWFT_HTML IMPLEMENTATION.


  METHOD add_bottom_label.
    DATA bottom_html TYPE w3html_tab.
    IF name IS NOT INITIAL.
      DATA(l_name) = |{ name }:  |.
    ENDIF.
    APPEND |<{ label } class="bottom_text">{ l_name }{ conv_exit( value ) }</{ label }>| TO bottom_html.
    APPEND VALUE #( html_type = 'bottom' html = bottom_html ) TO html_type_tab.
  ENDMETHOD.


  METHOD add_header_label.
    DATA header_html TYPE w3html_tab.
    IF name IS NOT INITIAL.
      DATA(l_name) = |{ name }:  |.
    ENDIF.
    APPEND |<{ label } class="header_text">{ l_name }{ conv_exit( value ) }</{ label }>| TO header_html.
    APPEND VALUE #( html_type = 'header' html = header_html ) TO html_type_tab.
  ENDMETHOD.


  METHOD add_table.
    DATA table_html TYPE w3html_tab.
    DATA(lt_fcat) = it_fcat.
    SORT lt_fcat BY col_pos.
    DELETE lt_fcat WHERE tech = 'X'.
    CHECK lt_fcat IS NOT INITIAL.
    CHECK it_table IS NOT INITIAL.

    APPEND |<hr /> | TO table_html.
    IF i_title IS NOT INITIAL.
      APPEND |<h3 class='h3'>{ i_title }</h3>| TO table_html.
    ENDIF.
    APPEND |<table class="table"> | TO table_html.

    APPEND |<tr>| TO table_html.
    LOOP AT lt_fcat INTO DATA(ls_fcat) WHERE tech = ''.
      APPEND |<th>{ COND #( WHEN ls_fcat-coltext IS INITIAL THEN ls_fcat-reptext
                                                  ELSE ls_fcat-coltext ) }</th>| TO table_html.
    ENDLOOP.
    APPEND |<tr>| TO table_html.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<line>).
      APPEND |<tr>| TO table_html.
      LOOP AT lt_fcat INTO ls_fcat.
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
        CHECK sy-subrc EQ 0.
        DATA(r_value) = conv_exit( i_fcat = ls_fcat i_value = <value> ).
        APPEND |<td>{ r_value }</td>| TO table_html.
      ENDLOOP.
      APPEND |</tr>| TO table_html.
    ENDLOOP.

    APPEND |</table>| TO table_html.
    APPEND |<br />| TO table_html.
    APPEND VALUE #( html_type = 'center' html = table_html ) TO html_type_tab.

  ENDMETHOD.


  METHOD build_html.
    LOOP AT html_type_tab INTO DATA(l_html).
      DATA(index) = get_insert_index(  l_html-html_type ) .
      IF index IS INITIAL.
        CASE l_html-html_type.
          WHEN 'css'.
            APPEND |<style type="text/css">| TO html_tab.
            APPEND LINES OF l_html-html TO html_tab.
            APPEND |</style>| TO html_tab.
          WHEN 'js'.
            APPEND |<script>| TO html_tab.
            APPEND LINES OF l_html-html TO html_tab.
            APPEND |</script>| TO html_tab.
        ENDCASE.
      ELSE.
        INSERT LINES OF l_html-html INTO html_tab INDEX index.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    IF html_temp IS NOT INITIAL.
      set_temp( html_temp ).
    ENDIF.
    IF css IS NOT INITIAL.
      add_style( css ).
    ENDIF.
    IF image IS NOT INITIAL.
      add_image( image ).
    ENDIF.
    IF js IS NOT INITIAL.
      add_js( js ).
    ENDIF.


  ENDMETHOD.


  METHOD conv_exit.
    IF i_fcat IS INITIAL.
      DATA: elemdescr TYPE REF TO cl_abap_elemdescr.
      elemdescr ?= cl_abap_elemdescr=>describe_by_data( i_value ).
      IF  elemdescr->edit_mask IS NOT INITIAL.
        WRITE i_value USING EDIT MASK elemdescr->edit_mask TO r_value.
      ELSEIF elemdescr->type_kind = 'D'."日期
        r_value = |{ CONV datum( i_value ) DATE = ENVIRONMENT }|.
      ELSEIF elemdescr->type_kind = 'T'."时间
        r_value = |{ CONV erzet( i_value ) TIME = ENVIRONMENT }|.
      ELSEIF elemdescr->type_kind = 'N'."数字
        r_value = |{ i_value ALPHA = OUT }|.
      ENDIF.
    ELSE.
      IF i_fcat-fieldname = 'ICON'.
        r_value = conv_icon_to_Emoji( i_value ).
      ELSEIF  i_fcat-edit_mask IS NOT INITIAL.
        WRITE i_value USING EDIT MASK i_fcat-edit_mask TO r_value.
      ELSEIF i_fcat-inttype = 'D'."日期
        r_value = |{ CONV datum( i_value ) DATE = ENVIRONMENT }|.
      ELSEIF i_fcat-inttype = 'T'."日期
        r_value = |{ CONV erzet( i_value ) TIME = ENVIRONMENT }|.
      ELSEIF i_fcat-inttype = 'N'."数字
        r_value = |{ i_value ALPHA = OUT }|.
      ENDIF.
    ENDIF.

    IF r_value IS  INITIAL.
      r_value = i_value.
    ENDIF.

  ENDMETHOD.


  METHOD conv_icon_to_emoji.
    CASE i_value.
      WHEN icon_refresh.
        r_value = '&#128259'.
      WHEN icon_checked.
        r_value = '&#9989'.
      WHEN icon_incomplete.
        r_value = '&#10062'.
      WHEN icon_led_red OR icon_red_light.
        r_value = '&#128308'.
      WHEN icon_led_yellow OR icon_yellow_light.
        r_value = '&#128310'.
      WHEN icon_led_green OR icon_green_light.
        r_value = '&#9989'.
      WHEN OTHERS.
        r_value = '&#9726'.

    ENDCASE.
  ENDMETHOD.


  METHOD display_falv.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN falv->outtab->* TO <table>.
    CHECK <table> IS NOT INITIAL.
    r_html = NEW zwft_html( ).
    DATA(title) = COND #(  WHEN falv->title_v1 IS NOT INITIAL THEN falv->title_v1
                                             ELSE sy-title ).
    r_html->add_table( i_title = title
    it_fcat = falv->fcat
    it_table = <table>
    ).
    r_html->html_show( ).
  ENDMETHOD.


  METHOD GET_HTML_MIME.
    DATA ls_key     TYPE wwwdatatab.

    ls_key-relid = 'MI'.
    ls_key-objid = i_objid.
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        MIME              = R_MIME
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 99.
  ENDMETHOD.


  METHOD get_html_temp.
    DATA ls_key     TYPE wwwdatatab.

    ls_key-relid = 'HT'.
    ls_key-objid = i_objid.
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        html              = r_html
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 99.
  ENDMETHOD.


  METHOD get_insert_index.
    tabix = 0.
    DATA(l_flag) = |<!--{ flag }-->|.
    READ TABLE html_tab TRANSPORTING NO FIELDS WITH KEY table_line = l_flag.
    CHECK sy-subrc EQ 0.
    tabix = sy-tabix .
  ENDMETHOD.


  METHOD get_locol_data.
    cl_gui_frontend_services=>gui_upload( EXPORTING filename = file
                                                                   CHANGING data_tab = data ).

  ENDMETHOD.


  METHOD get_locol_url.
    DATA(data) = get_locol_data( i_file ).
    CHECK data IS NOT INITIAL.
    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type                 = i_type
        subtype              = i_subtype
      TABLES
        data                 = data
      CHANGING
        url                  = r_url
      EXCEPTIONS
        dp_invalid_parameter = 1
        dp_error_put_table   = 2
        dp_error_general     = 3
        OTHERS               = 4.
  ENDMETHOD.


  METHOD get_wwwdata_url.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    CASE i_relid.
      WHEN 'HT'.
        DATA(html) = get_html_temp( i_object ).
        ASSIGN html TO <data>.
      WHEN 'MI'.
        DATA(mime) = get_html_mime( i_object ).
        ASSIGN mime TO <data>.
    ENDCASE.
    CHECK <data> IS ASSIGNED.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type                 = i_url_type
        subtype              = i_url_subtype
*        lifetime             = 'T'
      TABLES
        data                 = <data>
      CHANGING
        url                  = r_url
      EXCEPTIONS
        dp_invalid_parameter = 1
        dp_error_put_table   = 2
        dp_error_general     = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.


  METHOD html_show.
    build_html( ).
    CLEAR html_str.
    DATA error_list TYPE cl_abap_browser=>html_table.
    cl_abap_browser=>show_html(
    EXPORTING
      html = html_tab
      printing = abap_true
      context_menu = abap_true
      dialog = abap_true
    IMPORTING
      html_errors = error_list ).

  ENDMETHOD.


  METHOD INSERT_HTML_TAB.
    INSERT LINES OF it_html INTO html_tab INDEX get_insert_index(  I_FLAG ) .
  ENDMETHOD.


  METHOD set_temp.
    IF html_temp+0(6) = 'LOCAL:'.
      html_tab = get_locol_data( |{ html_temp+6 }| ).
    ELSE.
      html_tab = get_html_temp( html_temp ).
    ENDIF.
  ENDMETHOD.


  METHOD SET_TITLE.
    DATA title_html TYPE w3html_tab.
    APPEND |<title>{ title }</title>|  TO title_html.
    APPEND VALUE #( html_type = 'title' html = title_html ) TO html_type_tab.
    CLEAR title_html.
    APPEND |<h1 class="h1">{ title }</h1>|  TO title_html.
    APPEND VALUE #( html_type = 'header_text' html = title_html ) TO html_type_tab.
  ENDMETHOD.


  METHOD show_picture.
    DATA image_html TYPE cl_abap_browser=>html_table.
    DATA mime TYPE lvc_t_mime.
    DATA url(255) TYPE c.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = image_xstring
      TABLES
        binary_tab = mime.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type                 = 'IMAGE'
        subtype              = 'PNG'
      TABLES
        data                 = mime
      CHANGING
        url                  = url
      EXCEPTIONS
        dp_invalid_parameter = 1
        dp_error_put_table   = 2
        dp_error_general     = 3
        OTHERS               = 4.

    APPEND |<img CLASS="image" src="{ url }" alt="picture"</img>|  TO image_html.
    DATA error_list TYPE cl_abap_browser=>html_table.
    cl_abap_browser=>show_html(
    EXPORTING
      html = image_html
*      html_string = html_str
      printing = abap_true
      context_menu = abap_true
      dialog = 'X'
    IMPORTING
      html_errors = error_list ).
  ENDMETHOD.


  METHOD add_html.
    READ TABLE html_tab TRANSPORTING NO FIELDS WITH KEY table_line = flag.
    CHECK sy-subrc EQ 0.
    INSERT LINES OF html INTO  html_tab INDEX sy-tabix.
  ENDMETHOD.


  METHOD add_image.
    DATA image_html TYPE w3html_tab.
    DATA(url) = get_wwwdata_url( i_relid = 'MI'
                                                                i_object = pic
                                                                i_url_type = 'IMAGE'
                                                                i_url_subtype = 'PNG'
                                                                  ).

    APPEND |<img class="image" src="{ url }" alt=""</img>|  TO image_html.
    APPEND VALUE #( html_type = 'image' html = image_html ) TO html_type_tab.

  ENDMETHOD.


  METHOD add_js.
    IF html_js+0(6) = 'LOCAL:'.
      DATA(js)  = get_locol_data( |{ html_js+6 }| ).
    ELSE.
      js  = get_html_temp( html_js ).
    ENDIF.
    APPEND VALUE #( html_type = 'js' html = js ) TO html_type_tab.
  ENDMETHOD.


  METHOD add_style.
    IF html_css+0(6) = 'LOCAL:'.
      DATA(css)  = get_locol_data( |{ html_css+6 }| ).
    ELSE.
      css  = get_html_temp( html_css ).
    ENDIF.

    APPEND VALUE #( html_type = 'css' html = css ) TO html_type_tab.
  ENDMETHOD.


  METHOD html_viewer_show.
    DATA:url(255) TYPE c.
    repid = COND #( WHEN i_repid IS INITIAL THEN sy-cprog ELSE i_repid ).
    build_html( ).
    DATA(ob_html) = NEW cl_ew_html_viewer( parent = COND #( WHEN i_parent IS INITIAL THEN cl_gui_container=>screen0 ELSE i_parent )
                                                                           ).
    ob_html->set_ui_flag( ob_html->uiflag_no3dborder ).
    ob_html->set_registered_events( VALUE #( ( eventid = ob_html->m_id_sapevent
                                                                         appl_event = 'x'  ) ) ).
    SET HANDLER me->on_sapevent FOR ob_html.
    ob_html->load_data(  IMPORTING assigned_url = url CHANGING data_table = html_tab ).
    ob_html->show_url( url = url ).
  ENDMETHOD.


  METHOD on_sapevent.
    DATA callback TYPE callback.
    callback-action = action.
    callback-frame = frame.
    callback-getdata = getdata.
    callback-postdata = postdata.
    callback-query_table = query_table.
    PERFORM frm_html_callback IN PROGRAM (repid) IF FOUND USING callback.
  ENDMETHOD.


  METHOD set_button.
    line = |<button|.
    IF class IS NOT INITIAL.
      line = |{ line } class="{ class }"|.
    ENDIF.
    IF onclick IS NOT INITIAL.
      line = |{ line } onclick="{ onclick }"|.
    ENDIF.
    line = |{ line }>|.
    IF name2 IS NOT INITIAL.
      line = |{ line }<span>{ name2 }</span>|.
    ENDIF.
    IF name1 IS NOT INITIAL.
      line = |{ line }<span>{ name1 }</span>|.
    ENDIF.
    line = |{ line }</button>|.
  ENDMETHOD.
ENDCLASS.
