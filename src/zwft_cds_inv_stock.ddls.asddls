@AbapCatalog.sqlViewName: 'ZWFT_INV_STOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '进销存时点库存'
define view ZWFT_CDS_INV_STOCK 
  with parameters
    P_endDate  : vdm_v_end_date
as select from zwft_inv_mseg
{   
    bukrs,
    werks,
    lgort,
    satnr,
    matnr,
    charg,
    sobkz,
    ssnum,
    sum( erfmg ) as menge,
    sum( dmbtr ) as dmbtr

}

where budat_mkpf < $parameters.P_endDate //进销存需求,截止日期+1传入   
group by 
    bukrs,
    werks,
    lgort,
    satnr,
    matnr,
    charg,
    sobkz,
    ssnum

