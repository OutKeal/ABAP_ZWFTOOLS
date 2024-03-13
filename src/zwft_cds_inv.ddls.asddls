@AbapCatalog.sqlViewName: 'ZWFT_INV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '进销存报表'
define view ZWFT_CDS_INV 
  with parameters
    P_startDate  : vdm_v_start_date,
    P_endDate  : vdm_v_end_date
as select from ZWFT_CDS_INV_STOCK(P_endDate: $parameters.P_endDate ) as end
left outer join ZWFT_CDS_INV_STOCK( P_endDate: $parameters.P_startDate ) as start
on start.bukrs = end.bukrs
and start.werks = end.werks
and start.lgort = end.lgort
and start.satnr = end.satnr
and start.matnr = end.matnr
and start.charg = end.charg
and start.sobkz = end.sobkz
and start.ssnum = end.ssnum

left outer join zwft_cds_inv_change(P_startDate:$parameters.P_startDate,P_endDate:$parameters.P_endDate) as change
on change.bukrs = end.bukrs
and change.werks = end.werks
and change.lgort = end.lgort
and change.satnr = end.satnr
and change.matnr = end.matnr
and change.charg = end.charg
and change.sobkz = end.sobkz
and change.ssnum = end.ssnum
{
    end.bukrs,
    end.werks,
    end.lgort,
    end.satnr,
    end.matnr,
    end.charg,
    end.sobkz,
    end.ssnum,
    @EndUserText.label: '期初数量'
    start.menge as menge_start,
    @EndUserText.label: '期初金额'
    start.dmbtr as dmbtr_start,
    @EndUserText.label: '入库数量'
    change.MENGE_I as menge_in,
    @EndUserText.label: '入库金额'
    change.DMBTR_I as DMBTR_in,
    @EndUserText.label: '出库数量'
    change.MENGE_O as menge_out,
    @EndUserText.label: '出库金额'
    change.DMBTR_O as DMBTR_OUT,
    @EndUserText.label: '差异金额' 
    change.dmbtr_d as dmbtr_diff,
    @EndUserText.label: '异常数量'
    change.MENGE_E as menge_E,
    @EndUserText.label: '异常金额'
    change.DMBTR_E as DMBTR_E,
    @EndUserText.label: '期末数量'
    end.menge as menge_end,
    @EndUserText.label: '期末金额'
    end.dmbtr as dmbtr_end,
    case when end.sobkz = 'E' then LEFT(end.ssnum,10) end as KDAUF,
    case when end.sobkz = 'E' then RIGHT(end.ssnum,6) end as KDPOS,
    case when end.sobkz = 'T' then LEFT(end.ssnum,10) end as VBELN_VL,
    case when end.sobkz = 'T' then RIGHT(end.ssnum,6) end as POSNR_VL,
    case when end.sobkz = 'W' then LEFT(end.ssnum,10) end as KUNNR,
    case when end.sobkz = 'O' then LEFT(end.ssnum,10) end as LIFNR
}

