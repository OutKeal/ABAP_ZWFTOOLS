class ZCL_IM_WFT_INV_MB_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_WFT_INV_MB_BADI IMPLEMENTATION.


  METHOD if_ex_mb_document_badi~mb_document_before_update.
    CHECK xmseg IS NOT INITIAL.
    CHECK xmseg[ 1 ]-werks = 'W500' OR xmseg[ 1 ]-werks = 'W521'.
    zwft_inv_class=>mseg_write( xmseg = xmseg vm07m = xvm07m ).
  ENDMETHOD.


  method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.
  endmethod.
ENDCLASS.
