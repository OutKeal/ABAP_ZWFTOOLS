@AbapCatalog.sqlViewName: 'ZWFT_INV_S_TS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '时间戳的库存'
define view ZWFT_CDS_INV_S_TS
  with parameters
    p_TIMESTAMP  : timestamp,
    p_endDate : vdm_v_end_date
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
    kalnr,
    sum(erfmg) as menge,
    sum(dmbtr) as dmbtr
}
where budat_mkpf <= :p_endDate
and timestamp < :p_TIMESTAMP
group by 
    bukrs,
    werks,
    lgort,
    satnr,
    matnr,
    charg,
    sobkz,
    ssnum,
    kalnr
