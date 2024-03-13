class ZWFT_SINGLE_READ definition
  public
  final
  create public .

public section.

  class-methods LFA1
    importing
      !LIFNR type LIFNR
    returning
      value(LFA1) type LFA1 .
  class-methods LFM1
    importing
      !LIFNR type LIFNR
      !EKORG type EKORG
    returning
      value(LFM1) type LFM1 .
  class-methods LFB1
    importing
      !LIFNR type LIFNR
      !BUKRS type BUKRS
    returning
      value(LFB1) type LFB1 .
  class-methods MARA
    importing
      !MATNR type MATNR
    returning
      value(MARA) type MARA .
  class-methods MARC
    importing
      !MATNR type MATNR
      !WERKS type WERKS_D
    returning
      value(MARC) type MARC .
  class-methods MBEW
    importing
      !MATNR type MATNR
      !BWKEY type BWKEY
      !BWTAR type BWTAR optional
    returning
      value(MBEW) type MBEW .
  class-methods KNA1
    importing
      !KUNNR type KUNNR
    returning
      value(KNA1) type KNA1 .
  class-methods KNVV
    importing
      !KUNNR type KUNNR
      !VKORG type VKORG
      !VTWEG type VTWEG
      !SPART type SPART
    returning
      value(KNVV) type KNVV .
  class-methods KNB1
    importing
      !KUNNR type KUNNR
      !BUKRS type BUKRS
    returning
      value(KNB1) type KNB1 .
  class-methods EKKO
    importing
      !EBELN type EBELN
    returning
      value(EKKO) type EKKO .
  class-methods EKPO
    importing
      !EBELN type EBELN
      !EBELP type EBELP
    returning
      value(EKPO) type EKPO .
  class-methods T001
    importing
      !BUKRS type BUKRS
    returning
      value(T001) type T001 .
  class-methods T001W
    importing
      !WERKS type WERKS_D
    returning
      value(T001W) type T001W .
  class-methods T001L
    importing
      !WERKS type WERKS_D
      !LGORT type LGORT_D
    returning
      value(T001L) type T001L .
  class-methods T001K
    importing
      !BWKEY type BWKEY
    returning
      value(T001K) type T001K .
protected section.
private section.
ENDCLASS.



CLASS ZWFT_SINGLE_READ IMPLEMENTATION.


  METHOD ekko.
    CALL FUNCTION 'ME_EKKO_SINGLE_READ'
      EXPORTING
        pi_ebeln         = ebeln
*       PI_BYPASSING_BUFFER       =
*       PI_REFRESH_BUFFER         =
      IMPORTING
        po_ekko          = ekko
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD ekpo.
    CALL FUNCTION 'ME_EKPO_SINGLE_READ'
      EXPORTING
        pi_ebeln         = ebeln
        pi_ebelp         = ebelp
*       PI_BYPASSING_BUFFER       =
*       PI_REFRESH_BUFFER         =
      IMPORTING
        po_ekpo          = ekpo
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.



  ENDMETHOD.


  METHOD kna1.
    CALL FUNCTION 'KNA1_SINGLE_READ'
      EXPORTING
*       KZRFB         = ' '
        kna1_kunnr    = kunnr
*       CVP_BEHAVIOR  =
      IMPORTING
        wkna1         = kna1
      EXCEPTIONS
        not_found     = 1
        kunnr_blocked = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD knb1.
    CALL FUNCTION 'KNB1_SINGLE_READ'
      EXPORTING
        i_kunnr         = kunnr
        i_bukrs         = bukrs
*       I_RESET_BUFFER  =
*       I_BYPASSING_BUFFER       =
*       I_CVP_BEHAVIOR  =
      IMPORTING
        o_knb1          = knb1
      EXCEPTIONS
        not_found       = 1
        parameter_error = 2
        kunnr_blocked   = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD knvv.
    CALL FUNCTION 'KNVV_SINGLE_READ'
      EXPORTING
        i_kunnr         = kunnr
        i_vkorg         = vkorg
        i_vtweg         = vtweg
        i_spart         = spart
*       I_RESET_BUFFER  =
*       I_BYPASSING_BUFFER       =
*       I_CVP_BEHAVIOR  =
      IMPORTING
        o_knvv          = knvv
      EXCEPTIONS
        not_found       = 1
        parameter_error = 2
        kunnr_blocked   = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD lfa1.
    CALL FUNCTION 'LFA1_SINGLE_READ'
      EXPORTING
        lfa1_lifnr    = lifnr
      IMPORTING
        wlfa1         = lfa1
      EXCEPTIONS
        not_found     = 1
        lifnr_blocked = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD lfb1.
    CALL FUNCTION 'LFB1_SINGLE_READ'
      EXPORTING
        i_lifnr = lifnr
        i_bukrs = bukrs
*       I_RESET_BUFFER           =
*       I_BYPASSING_BUFFER       =
*       I_CVP_BEHAVIOR           =
      IMPORTING
        o_lfb1  = lfb1
*     EXCEPTIONS
*       NOT_FOUND                = 1
*       PARAMETER_ERROR          = 2
*       LIFNR_BLOCKED            = 3
*       OTHERS  = 4
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD lfm1.
    CALL FUNCTION 'LFM1_SINGLE_READ'
      EXPORTING
        i_lifnr       = lifnr
        i_ekorg       = ekorg
*       I_RESET_BUFFER           =
*       I_BYPASSING_BUFFER       =
*       I_CVP_BEHAVIOR           =
      IMPORTING
        o_lfm1        = lfm1
*       O_EKORZ       =
      EXCEPTIONS
        not_found     = 1
        lifnr_blocked = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD mara.
    CALL FUNCTION 'MARA_SINGLE_READ'
      EXPORTING
*       KZRFB             = ' '
*       MAXTZ             = 0
        matnr             = matnr
*       SPERRMODUS        = ' '
*       STD_SPERRMODUS    = ' '
*       OUTPUT_NO_MESSAGE =
      IMPORTING
        wmara             = mara
      EXCEPTIONS
        lock_on_material  = 1
        lock_system_error = 2
        wrong_call        = 3
        not_found         = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD marc.
    CALL FUNCTION 'MARC_SINGLE_READ'
      EXPORTING
*       KZRFB             = ' '
*       MAXTZ             = 0
        matnr             = matnr
        werks             = werks
*       SPERRMODUS        = ' '
*       STD_SPERRMODUS    = ' '
*       STAWN_READ        = 'X'
      IMPORTING
        wmarc             = marc
*       O_MARC            =
      EXCEPTIONS
        lock_on_marc      = 1
        lock_system_error = 2
        wrong_call        = 3
        not_found         = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


METHOD mbew.
  CALL FUNCTION 'MBEW_SINGLE_READ'
    EXPORTING
*     KZRFB             = ' '
*     MAXTZ             = 0
      matnr             = matnr
      bwkey             = bwkey
      bwtar             = bwtar
*     SPERRMODUS        = ' '
*     STD_SPERRMODUS    = ' '
    IMPORTING
      wmbew             = mbew
*     O_MBEW            =
    EXCEPTIONS
      lock_on_mbew      = 1
      lock_system_error = 2
      wrong_call        = 3
      not_found         = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.


  METHOD t001.
    CALL FUNCTION 'T001_SINGLE_READ'
      EXPORTING
*       KZRFB      = ' '
*       MAXTZ      = 0
        bukrs      = bukrs
      IMPORTING
        wt001      = t001
      EXCEPTIONS
        not_found  = 1
        wrong_call = 2
        OTHERS     = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  method T001K.
    CALL FUNCTION 'T001K_SINGLE_READ'
      EXPORTING
*       KZRFB            = ' '
*       MAXTZ            = 0
        bwkey            = BWKEY
     IMPORTING
       WT001K           = T001K
     EXCEPTIONS
       NOT_FOUND        = 1
       WRONG_CALL       = 2
       OTHERS           = 3
              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  endmethod.


  METHOD t001l.
    CALL FUNCTION 'T001L_SINGLE_READ'
      EXPORTING
*       KZRFB       = ' '
        t001l_werks = werks
        t001l_lgort = lgort
      IMPORTING
        wt001l      = t001l
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD t001w.
    CALL FUNCTION 'T001W_SINGLE_READ'
      EXPORTING
*       KZRFB       = ' '
        t001w_werks = werks
      IMPORTING
        wt001w      = t001w
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
ENDCLASS.
