class ZWFT_INV_CLASS definition
  public
  final
  create public .

public section.

  types:
    tt_inv_diff TYPE TABLE OF zwft_inv_diff .
  types:
    BEGIN OF ty_inv_sum ,
            kalnr type ck_kalnr,
            Total   TYPE menge_d,
          END OF ty_inv_sum .
  types:
    tt_inv_sum TYPE TABLE OF ty_inv_sum .
  types:
    tt_inv_type TYPE TABLE OF  zwft_inv_type .
  types:
    tt_inv_mseg TYPE TABLE OF  zwft_inv_mseg .
  types:
    tt_inv_stock TYPE TABLE OF  zwft_inv_s_ts .

  class-data INV_TYPE type TT_INV_TYPE .

  class-methods INIT_INV_TYPE .
  class-methods MSEG_WRITE
    importing
      !XMSEG type TY_T_MSEG
      !VM07M type TY_T_VM07M optional .
  class-methods DIFF_SPLIT
    importing
      !IT_DIFF type TT_INV_DIFF
    exporting
      value(ET_MSEG) type TT_INV_MSEG .
  class-methods GET_TIMESTAMP_STOCK
    importing
      !IT_KALNR type CKF_RANGES_KALNR_TABLE
      !I_BUDAT type BUDAT
      !I_TIMESTAMP type TIMESTAMP
    exporting
      !ET_STOCK type TT_INV_STOCK .
  class-methods GET_SUM_STOCK
    importing
      !IT_INV_STOCK type TT_INV_STOCK
    exporting
      !ET_INV_SUM type TT_INV_SUM .
PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZWFT_INV_CLASS IMPLEMENTATION.


  METHOD diff_split.
    DATA it_kalnr TYPE ckf_ranges_kalnr_table.
    DATA l_budat TYPE budat.
    DATA l_wsl TYPE fins_vwcur12.
    CHECK it_diff IS NOT INITIAL.

    LOOP AT it_diff INTO DATA(is_diff)
                            GROUP BY ( timestamp = is_diff-timestamp
                                                 werks = is_diff-werks
                                                  budat = is_diff-budat )
                            INTO DATA(group_diff).
      CLEAR it_kalnr.
      LOOP AT GROUP group_diff INTO DATA(ls_diff).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_diff-kalnr ) TO it_kalnr.
      ENDLOOP.
      l_budat = zwft_common=>date_get_last_day_of_month( group_diff-budat ).

      zwft_inv_class=>get_timestamp_stock( EXPORTING it_kalnr = it_kalnr
                                                                          i_budat = l_budat
                                                                          i_timestamp = group_diff-timestamp
                                                                     IMPORTING et_stock = DATA(inv_stock)  ).


      zwft_inv_class=>get_sum_stock( EXPORTING it_inv_stock = inv_stock
                                                          IMPORTING et_inv_sum = DATA(inv_sum) ).
      SORT inv_sum BY kalnr.

      LOOP AT GROUP group_diff INTO ls_diff.
        l_wsl = ls_diff-wsl.
        READ TABLE inv_sum ASSIGNING FIELD-SYMBOL(<inv_sum>) WITH KEY kalnr = ls_diff-kalnr BINARY SEARCH.
        CHECK sy-subrc EQ 0.
        LOOP AT inv_stock INTO DATA(l_inv_stock) WHERE kalnr = ls_diff-kalnr.
          APPEND INITIAL LINE TO et_mseg ASSIGNING FIELD-SYMBOL(<mseg>).
          MOVE-CORRESPONDING l_inv_stock TO <mseg>.
          <mseg>-record_type = ls_diff-awtyp.
          <mseg>-mblnr = ls_diff-belnr.
          <mseg>-mjahr = ls_diff-gjahr.
          <mseg>-zeile = ls_diff-docln.
          <mseg>-bukrs = ls_diff-rbukrs.
          <mseg>-shkzg = ls_diff-drcrk.
          <mseg>-kalnr = ls_diff-kalnr.
          <mseg>-budat_mkpf = ls_diff-budat.
          <mseg>-timestamp = ls_diff-timestamp.
          <mseg>-bwart = 'DIF'.
          <mseg>-inv_type = 'D'.
          <mseg>-erfmg = 0.
          <mseg>-dmbtr = ls_diff-wsl * l_inv_stock-menge / <inv_sum>-total .
          l_wsl -= <mseg>-dmbtr.
        ENDLOOP.
        IF sy-subrc EQ 0 AND l_wsl IS NOT INITIAL.
          <mseg>-dmbtr += l_wsl.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    DATA sub_zeile TYPE mblpo.
    LOOP AT et_mseg INTO DATA(l_mseg) GROUP BY ( record_type = l_mseg-record_type
                                                                                      mblnr = l_mseg-mblnr
                                                                                      mjahr = l_mseg-mjahr
                                                                                      zeile = l_mseg-zeile
                                                                                      bukrs = l_mseg-bukrs )
                                                                        INTO DATA(group_mseg).
      sub_zeile = 0.
      LOOP AT GROUP group_mseg ASSIGNING <mseg>.
        sub_zeile += 1.
        <mseg>-sub_zeile = sub_zeile.
      ENDLOOP.
    ENDLOOP.

    IF et_mseg IS NOT INITIAL.
      MODIFY zwft_inv_mseg FROM TABLE et_mseg.
    ENDIF.

  ENDMETHOD.


  METHOD get_sum_stock.
    DATA l_inv_sum LIKE LINE OF et_inv_sum.
    LOOP AT it_inv_stock INTO DATA(is_inv_stock).
      l_inv_sum-kalnr = is_inv_stock-kalnr.
      l_inv_sum-total = is_inv_stock-menge.
      COLLECT l_inv_sum INTO et_inv_sum.
    ENDLOOP.

    DELETE et_inv_sum WHERE total = 0.
  ENDMETHOD.


  METHOD get_timestamp_stock.
    CHECK it_kalnr IS NOT INITIAL.
    SELECT * FROM zwft_inv_s_ts( p_enddate = @i_budat , p_timestamp = @i_timestamp ) AS a
      WHERE kalnr IN @it_kalnr
      AND menge <> 0
      INTO TABLE @et_stock.
  ENDMETHOD.


  METHOD INIT_INV_TYPE.
    IF zwft_inv_class=>inv_type IS INITIAL.
      SELECT * FROM zwft_inv_type
        INTO TABLE zwft_inv_class=>inv_type.
      IF sy-subrc NE 0.
        APPEND VALUE #( bwart = '001') TO zwft_inv_class=>inv_type.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD mseg_write.
    init_inv_type( ).
    DATA: in_mseg TYPE TABLE OF zwft_inv_mseg.
    DATA: in_mseg_ex TYPE TABLE OF zwft_inv_mseg.

    IF vm07m IS INITIAL.
      SELECT mblnr,mjahr,zeile,kalnr
        FROM matdoc
        FOR ALL ENTRIES IN @xmseg
    WHERE mblnr = @xmseg-mblnr
        INTO TABLE @DATA(lt_doc).
      SORT lt_doc BY mblnr mjahr zeile.
    ENDIF.

    CHECK xmseg[] IS NOT INITIAL.
    LOOP AT xmseg INTO DATA(l_xmseg).
      READ TABLE zwft_inv_class=>inv_type INTO DATA(l_inv_type)
                                                              WITH KEY bwart = l_xmseg-bwart
                                                                                shkzg = l_xmseg-shkzg.
      APPEND INITIAL LINE TO in_mseg ASSIGNING FIELD-SYMBOL(<in_mseg>).
      MOVE-CORRESPONDING l_xmseg TO <in_mseg>.

      <in_mseg>-satnr = zwft_single_read=>mara( <in_mseg>-matnr )-satnr.
      IF <in_mseg>-satnr IS INITIAL.
        <in_mseg>-satnr = <in_mseg>-matnr.
      ENDIF.

      IF <in_mseg>-dmbtr IS INITIAL AND l_xmseg-exbwr IS NOT INITIAL.
        <in_mseg>-dmbtr = l_xmseg-exbwr.
      ELSEIF <in_mseg>-dmbtr IS INITIAL.
        IF l_xmseg-bustw IS INITIAL AND <in_mseg>-lbkum IS NOT INITIAL.
          <in_mseg>-dmbtr = <in_mseg>-erfmg * <in_mseg>-salk3 / <in_mseg>-lbkum.
        ENDIF.
      ENDIF.

      IF <in_mseg>-shkzg = 'H'.
        <in_mseg>-erfmg  = - <in_mseg>-erfmg.
        <in_mseg>-dmbtr  = - <in_mseg>-dmbtr.
      ENDIF.

      <in_mseg>-record_type = 'MDOC'.
      <in_mseg>-inv_type = l_inv_type-inv_type.
      CLEAR l_inv_type.
      CASE <in_mseg>-sobkz.
        WHEN 'O' OR 'K'.
          <in_mseg>-ssnum = <in_mseg>-lifnr.
        WHEN 'W'.
          <in_mseg>-ssnum = <in_mseg>-kunnr.
        WHEN 'E' OR 'T'.
          <in_mseg>-ssnum = <in_mseg>-mat_kdauf && <in_mseg>-mat_kdpos.
      ENDCASE.

      CONVERT DATE l_xmseg-cpudt_mkpf TIME l_xmseg-cputm_mkpf
        INTO TIME STAMP <in_mseg>-timestamp TIME ZONE sy-zonlo.
      IF vm07m IS NOT INITIAL.
        READ TABLE vm07m INTO DATA(l_vm07m) WITH KEY zeilv = l_xmseg-zeile.
        IF sy-subrc EQ 0.
          <in_mseg>-kalnr = l_vm07m-kaln1.
        ENDIF.
      ELSEIF lt_doc IS NOT INITIAL.
        READ TABLE lt_doc INTO DATA(ls_doc) WITH KEY mblnr = l_xmseg-mblnr mjahr = l_xmseg-mjahr zeile = l_xmseg-zeile BINARY SEARCH.
        IF sy-subrc EQ 0.
          <in_mseg>-kalnr = ls_doc-kalnr.
        ENDIF.
      ENDIF.

      IF <in_mseg>-bwart = '315' OR <in_mseg>-bwart = '316'.
        APPEND INITIAL LINE TO in_mseg_ex ASSIGNING FIELD-SYMBOL(<in_mseg_ex>).
        <in_mseg_ex> = <in_mseg>.
        <in_mseg_ex>-record_type = 'MDOC_CP'.
        <in_mseg_ex>-shkzg = COND #( WHEN <in_mseg_ex>-shkzg = 'S' THEN 'H' ELSE 'H' ).
        <in_mseg_ex>-erfmg  = - <in_mseg_ex>-erfmg.
        <in_mseg_ex>-dmbtr  = - <in_mseg_ex>-dmbtr.
      ENDIF.

    ENDLOOP.

    IF in_mseg_ex IS NOT INITIAL.
      APPEND LINES OF in_mseg_ex TO in_mseg.
    ENDIF.

    IF in_mseg IS NOT INITIAL.
      MODIFY zwft_inv_mseg FROM TABLE in_mseg.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
