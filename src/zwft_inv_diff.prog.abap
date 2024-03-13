*&---------------------------------------------------------------------*
*& Report ZWFT_INV_DIFF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_inv_diff.
TABLES: bkpf,t001w,mara.
SELECT-OPTIONS: s_budat FOR bkpf-budat,
                              s_werks FOR t001w-werks,
                              s_matnr FOR mara-matnr.

START-OF-SELECTION.

  SELECT * FROM zwft_inv_diff
    WHERE budat IN @s_budat
    AND matnr IN @s_matnr
    AND werks IN @s_werks
    INTO TABLE @DATA(gt_diff).

  IF sy-subrc NE 0.
    MESSAGE '无需分摊的数据' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  zwft_inv_class=>diff_split( EXPORTING it_diff = gt_diff IMPORTING et_mseg = DATA(inv_mseg) ).
