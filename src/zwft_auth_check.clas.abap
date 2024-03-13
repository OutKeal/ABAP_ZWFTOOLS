class ZWFT_AUTH_CHECK definition
  public
  final
  create public .

public section.

  class-methods WERKS
    importing
      !I_OBJECT type XUOBJECT default 'M_MATE_WRK'
      !I_ACTVT type ACTIV_AUTH default '03'
      !I_WERKS type WERKS_D optional
    returning
      value(R_BOOL) type ABAP_BOOL .
  class-methods WERKS_RANGE
    importing
      !I_OBJECT type XUOBJECT default 'M_MATE_WRK'
      !I_ACTVT type ACTIV_AUTH default '03'
      !I_RANGES_WERKS type RANGE_T_WERKS optional
    exporting
      value(E_RANGES_WERKS) type RANGE_T_WERKS
    returning
      value(R_BOOL) type ABAP_BOOL .
  class-methods BUKRS
    importing
      !I_OBJECT type XUOBJECT default 'M_RECH_BUK'
      !I_ACTVT type ACTIV_AUTH default '03'
      !I_BUKRS type BUKRS optional
    returning
      value(R_BOOL) type ABAP_BOOL .
  class-methods BUKRS_RANGE
    importing
      !I_OBJECT type XUOBJECT default 'M_RECH_BUK'
      !I_ACTVT type ACTIV_AUTH default '03'
      !I_RANGES_BUKRS type RANGES_BURKS_TT optional
    exporting
      value(E_RANGES_BUKRS) type RANGES_BURKS_TT
    returning
      value(R_BOOL) type ABAP_BOOL .
  class-methods EKORG
    importing
      !I_OBJECT type XUOBJECT default 'M_RECH_BUK'
      !I_ACTVT type ACTIV_AUTH default '03'
      !I_EKORG type EKORG optional
    returning
      value(R_BOOL) type ABAP_BOOL .
  class-methods EKORG_RANGE
    importing
      !I_OBJECT type XUOBJECT default 'M_RECH_BUK'
      !I_ACTVT type ACTIV_AUTH default '03'
      !I_RANGES_EKORG type MMPURUI_EKORG_RANGE_TTY optional
    exporting
      value(E_RANGES_EKORG) type MMPURUI_EKORG_RANGE_TTY
    returning
      value(R_BOOL) type ABAP_BOOL .
protected section.
private section.
ENDCLASS.



CLASS ZWFT_AUTH_CHECK IMPLEMENTATION.


  METHOD BUKRS.
    r_bool = abap_false.

    AUTHORITY-CHECK OBJECT i_object ID 'BUKRS' FIELD i_bukrs
                                     ID 'ACTVT' FIELD i_actvt.
    IF sy-subrc EQ 0.
      r_bool = abap_true.
    ELSE.
      MESSAGE s899(mm) WITH '缺少公司权限'  DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD BUKRS_RANGE.
    r_bool = abap_false.
    SELECT bukrs FROM t001 WHERE bukrs IN @i_ranges_bukrs
      INTO TABLE @DATA(lt_T001).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT lt_T001 INTO DATA(ls_T001).
      AUTHORITY-CHECK OBJECT i_object ID 'BUKRS' FIELD ls_T001-bukrs
                                       ID 'ACTVT' FIELD i_actvt.
      IF sy-subrc = 0.
        APPEND VALUE #( sign = 'I'
                                        option = 'EQ'
                                        low = ls_T001-bukrs ) TO e_ranges_bukrs.
      ENDIF.
    ENDLOOP.

    IF e_ranges_bukrs IS NOT INITIAL.
      r_bool = abap_true.
    ELSE.
      MESSAGE s899(mm) WITH '缺少公司权限'  DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD ekorg.
    r_bool = abap_false.

    AUTHORITY-CHECK OBJECT i_object ID 'EKORG' FIELD i_ekorg
                                     ID 'ACTVT' FIELD i_actvt.
    IF sy-subrc EQ 0.
      r_bool = abap_true.
    ELSE.
      MESSAGE s899(mm) WITH '缺少采购组织权限'  DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD EKORG_RANGE.
    r_bool = abap_false.
    SELECT ekorg FROM t024e WHERE ekorg IN @i_ranges_ekorg
      INTO TABLE @DATA(lt_t024e).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT lt_t024e INTO DATA(ls_t024e).
      AUTHORITY-CHECK OBJECT i_object ID 'EKORG' FIELD ls_t024e-ekorg
                                       ID 'ACTVT' FIELD i_actvt.
      IF sy-subrc = 0.
        APPEND VALUE #( sign = 'I'
                                        option = 'EQ'
                                        low = ls_t024e-ekorg ) TO e_ranges_ekorg.
      ENDIF.
    ENDLOOP.

    IF e_ranges_ekorg IS NOT INITIAL.
      r_bool = abap_true.
    ELSE.
      MESSAGE s899(mm) WITH '缺少公司权限'  DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD werks.
    r_bool = abap_false.

    AUTHORITY-CHECK OBJECT i_object ID 'WERKS' FIELD i_werks
                                     ID 'ACTVT' FIELD i_actvt.
    IF sy-subrc EQ 0.
      r_bool = abap_true.
    ELSE.
      MESSAGE s899(mm) WITH '缺少工厂权限'  DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD werks_range.
    r_bool = abap_false.
    SELECT werks FROM t001w WHERE werks IN @i_ranges_werks
      INTO TABLE @DATA(lt_t001w).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT lt_t001w INTO DATA(ls_t001w).
      AUTHORITY-CHECK OBJECT i_object ID 'WERKS' FIELD ls_t001w-werks
                                       ID 'ACTVT' FIELD i_actvt.
      IF sy-subrc = 0.
        APPEND VALUE #( sign = 'I'
                                        option = 'EQ'
                                        low = ls_t001w-werks ) TO e_ranges_werks.
      ENDIF.
    ENDLOOP.

    IF e_ranges_werks IS NOT INITIAL.
      r_bool = abap_true.
    ELSE.
      MESSAGE s899(mm) WITH '缺少工厂权限'  DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
