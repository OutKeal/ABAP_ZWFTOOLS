*&---------------------------------------------------------------------*
*& Report ZWFT_INV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_inv.

TABLES: t001w,mara,t001l.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
  s_werks      FOR t001l-werks NO-EXTENSION NO INTERVALS MEMORY ID wrk OBLIGATORY,
  s_lgort      FOR t001l-lgort ,
  s_mtart      FOR mara-mtart ,
  s_matkl      FOR mara-matkl ,
  s_matnr      FOR mara-matnr .
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:p_start TYPE sy-datum OBLIGATORY.
  PARAMETERS:p_end TYPE sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  p_end = zwft_common=>date_get_last_day_of_month( sy-datum ).
  p_start = p_end+0(6) && '01'.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  RANGES auth_werks FOR t001w-werks.
  IF zwft_auth_check=>werks_range( EXPORTING i_object = 'M_MATE_WRK' i_actvt = '03' i_ranges_werks = s_werks[]
  IMPORTING e_ranges_werks = auth_werks[]
    ) = abap_false.
    STOP.
  ENDIF.

  DATA(p_date) = p_end + 1.
  SELECT
      a~bukrs,
      a~werks,
      a~lgort,
      l~lgobe AS lgort_name,
      p~pmatn AS satnr,
      a~matnr,
     a~charg,
     a~sobkz,
     a~ssnum,
    ( CASE WHEN a~sobkz = 'E' OR a~sobkz = 'T'
    THEN left( a~ssnum , 10 ) ELSE ' ' END ) AS kdauf,
    ( CASE WHEN a~sobkz = 'E' OR a~sobkz = 'T'
    THEN right( a~ssnum , 6 ) ELSE ' ' END ) AS kdpos,
     a~menge_start  ,
     a~dmbtr_start  ,
     a~menge_in     ,
     a~dmbtr_in     ,
     a~menge_out    ,
     a~dmbtr_out    ,
     a~dmbtr_diff   ,
     a~menge_e      ,
     a~dmbtr_e      ,
     a~menge_end    ,
     a~dmbtr_end    ,
    b~maktx
    FROM zwft_cds_inv(  p_startdate = @p_start,
                                      p_enddate = @p_date ) AS a
 INNER JOIN zmat_v_mara_ext AS b ON a~matnr = b~matnr AND b~spras = @sy-langu
 LEFT JOIN vbap AS p ON a~kdauf = p~vbeln AND a~kdpos = p~posnr
 LEFT JOIN t001l AS l ON a~werks = l~werks AND a~lgort = l~lgort
WHERE a~werks IN @auth_werks
    AND a~matnr IN @s_matnr
    AND b~mtart IN @s_mtart
    AND a~lgort IN @s_lgort
    AND b~matkl IN @s_matkl
    AND ( menge_start IS NOT INITIAL
       OR menge_in IS NOT INITIAL
       OR menge_out IS NOT INITIAL
       OR menge_end IS NOT INITIAL  )
  INTO TABLE @DATA(lt_inv).
  IF sy-subrc NE 0.
    MESSAGE s899(mm) WITH '无数据' DISPLAY LIKE 'E'.
  ENDIF.

  DATA(falv) = zwft_falv=>create( CHANGING ct_table = lt_inv ).

  zwft_common=>fcat_from_config( EXPORTING i_repid = sy-repid
                                                                               i_name = 'FALV'
                                                          CHANGING ct_fcat = falv->fcat
                                                  ).
  falv->display( ).

END-OF-SELECTION .
