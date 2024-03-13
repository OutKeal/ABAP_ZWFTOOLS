@AbapCatalog.sqlViewName: 'ZWFT_INV_DIFF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '获取进销存差异'
define view ZWFT_CDS_INV_DIFF as select from acdoca
inner join bkpf
    on acdoca.rbukrs = bkpf.bukrs
    and acdoca.belnr = bkpf.belnr
    and acdoca.gjahr = bkpf.gjahr
{
    acdoca.awtyp,
    acdoca.belnr,
    acdoca.gjahr,
    acdoca.docln,
    acdoca.rbukrs,
    acdoca.drcrk,
    acdoca.budat,
    acdoca.matnr,
    acdoca.werks,
    acdoca.timestamp,
    acdoca.kalnr,
    acdoca.wsl
}
where ( acdoca.awtyp <> 'MKPF' or ( acdoca.awtyp = 'MKPF' and acdoca.blart = 'PR' ) )
and acdoca.ktosl = 'BSX'

