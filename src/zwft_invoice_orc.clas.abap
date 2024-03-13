class ZWFT_INVOICE_ORC definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF words_result ,
        invoicetypeorg       TYPE string,
        invoicecode          TYPE string,
        invoicenum           TYPE string,
        invoicedate          TYPE string,
        purchasername        TYPE string,
        purchaserregisternum TYPE string,
        purchaseraddress     TYPE string,
        purchaserbank        TYPE string,
        province             TYPE string,
        city                 TYPE string,
        sheetnum             TYPE string,
        commodityunit        TYPE string,
        sellername           TYPE string,
        sellerregisternum    TYPE string,
        selleraddress        TYPE string,
        sellerbank           TYPE string,
        totalamount          TYPE string,
        totaltax             TYPE string,
        amountinwords        TYPE string,
        remarks              TYPE string,
      END OF words_result .
  types:
    BEGIN OF ty_invoice,
        words_result_num TYPE string,
        words_result     TYPE words_result,
      END OF ty_invoice .

  data API_KEY type STRING value '1Bf4y1kOpejGp4u5fbtCYYwB' ##NO_TEXT.
  data SECRET_KEY type STRING value 'XW8x6VdBHwSZ2b1ikRca4SVRQamP5M3c' ##NO_TEXT.
  data IMAGE_XSTRING type XSTRING .

  methods CONSTRUCTOR
    importing
      !L_API_KEY type STRING optional
      !L_SECRET_KEY type STRING optional .
  methods GET_INVOICE
    exporting
      !INVOICE type TY_INVOICE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_token ,
        refresh_token TYPE string,
        access_token  TYPE string,
      END OF ty_token .

    DATA token_url TYPE string VALUE 'https://aip.baidubce.com/oauth/2.0/token?grant_type=client_credentials' ##NO_TEXT.
    DATA api_url TYPE string VALUE 'https://aip.baidubce.com/rest/2.0/ocr/v1/vat_invoice' ##NO_TEXT.

    METHODS get_image
      RETURNING
        VALUE(image_urlencoded) TYPE string .
    METHODS get_token
      RETURNING
        VALUE(token) TYPE string .
ENDCLASS.



CLASS ZWFT_INVOICE_ORC IMPLEMENTATION.


  METHOD constructor.

    IF l_api_key IS INITIAL.
      api_key = l_api_key.
    ENDIF.

    IF l_api_key IS INITIAL.
      secret_key = l_secret_key.
    ENDIF.

  ENDMETHOD.


  METHOD get_image.
    image_xstring = zwft_common=>file_upload_bin( ).
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = image_xstring
      IMPORTING
        output = image_urlencoded.
    image_urlencoded = cl_http_utility=>escape_url( image_urlencoded ).
  ENDMETHOD.


  METHOD get_invoice.
    DATA:lo_http_client TYPE REF TO if_http_client.
    DATA(image_data) = get_image( ).
    CHECK image_data IS NOT INITIAL.
    image_data = |image={ image_data }|.

    DATA(token) = get_token( ).
    CHECK token IS NOT INITIAL.

    api_url = |{ api_url }?access_token={ token }|.

    cl_http_client=>create_by_url( EXPORTING url = api_url IMPORTING client = lo_http_client ).

    lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
    lo_http_client->request->set_header_field( name = if_http_header_fields=>content_type value = 'application/x-www-form-urlencoded' ).
    lo_http_client->request->set_cdata( data = image_data length = strlen( image_data ) ).
    lo_http_client->send( EXCEPTIONS http_communication_failure = 1 http_invalid_state = 2 http_processing_failed = 3 ).
    lo_http_client->receive( EXCEPTIONS http_communication_failure = 1 http_invalid_state = 2 http_processing_failed = 3 ).
    lo_http_client->response->get_status( IMPORTING code = DATA(lv_subrc) reason = DATA(return) ).


    IF lv_subrc = 200.
      DATA(cdata) = lo_http_client->response->get_cdata( ).
      cl_fdt_json=>json_to_data( EXPORTING iv_json = cdata CHANGING ca_data = invoice ).
    ELSE.
      MESSAGE 'invoice get error' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD get_token.
    DATA:lo_http_client TYPE REF TO if_http_client.
    DATA ls_token TYPE ty_token.

    token_url = |{ token_url }&client_id={ api_key }&client_secret={ secret_key }|.
    cl_http_client=>create_by_url( EXPORTING url = token_url IMPORTING client = lo_http_client ).
    lo_http_client->request->set_header_field( name = if_http_header_fields=>content_type value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).
    lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
    lo_http_client->send( EXCEPTIONS http_communication_failure = 1 http_invalid_state = 2 http_processing_failed = 3 ).
    lo_http_client->receive( EXCEPTIONS http_communication_failure = 1 http_invalid_state = 2 http_processing_failed = 3 ).

    lo_http_client->response->get_status( IMPORTING code = DATA(lv_subrc) reason = DATA(return) ).
    IF lv_subrc = 200.
      cl_fdt_json=>json_to_data( EXPORTING iv_json = lo_http_client->response->get_cdata( ) CHANGING ca_data = ls_token ).
      token = ls_token-access_token.
    ELSE.
      MESSAGE 'token get error' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
