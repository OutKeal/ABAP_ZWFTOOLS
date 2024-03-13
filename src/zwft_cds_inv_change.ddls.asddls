@AbapCatalog.sqlViewName: 'ZWFT_INV_CHANGE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '进销存发生额'


define view zwft_cds_inv_change 
  with parameters
    P_startDate  : vdm_v_start_date,
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
    sum( case inv_type when 'I' then erfmg end ) as MENGE_I,
    sum( case inv_type when 'I' then dmbtr end ) as DMBTR_I,
    sum( case inv_type when 'O' then erfmg end ) as MENGE_O,
    sum( case inv_type when 'O' then dmbtr end ) as DMBTR_O,
    sum( case inv_type when 'D' then dmbtr end ) as dmbtr_d,
    sum( case inv_type when ' ' then erfmg end ) as MENGE_E,
    sum( case inv_type when ' ' then dmbtr end ) as DMBTR_E
    
}
where budat_mkpf >= $parameters.P_startDate
and budat_mkpf < $parameters.P_endDate
group by 
    bukrs,
    werks,
    lgort,
    satnr,
    matnr,
    charg,
    sobkz,
    ssnum
