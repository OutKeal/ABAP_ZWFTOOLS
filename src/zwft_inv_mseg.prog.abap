*&---------------------------------------------------------------------*
*& Report ZWFT_MSEG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_inv_mseg.

TABLES mseg.
DATA it_mseg TYPE ty_t_mseg.

SELECT-OPTIONS:s_werks FOR mseg-werks,
                              s_bwart FOR mseg-bwart,
                              s_mblnr FOR mseg-mblnr,
                              s_matnr FOR mseg-matnr.


START-OF-SELECTION.


  SELECT * FROM mseg WHERE werks IN @s_werks
  AND bwart IN @s_bwart
  AND mblnr IN @s_mblnr
  AND matnr IN @s_matnr
  INTO TABLE @it_mseg.
  IF sy-subrc EQ 0.
    zwft_inv_class=>mseg_write( it_mseg ).
    COMMIT WORK AND WAIT.
    MESSAGE '写入成功' TYPE 'S'.
  ELSE.
    MESSAGE '未读取物料凭证' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
