* @ValidationCode : MjotNTk0OTkxNzk3OkNwMTI1MjoxNjIxNTAzODUwMTk2OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 20 May 2021 15:44:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE OFS.TEST
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER

    $USING EB.Foundation
    $USING EB.Interface
    $USING EB.TransactionControl
*-----------------------------------------------------------------------------
    AppName = 'FUNDS.TRANSFER'
    Ofsfunct = 'I'
    Process = 'PROCESS'
    Ofsversion = 'FUNDS.TRANSFER,'
    Gtsmode = ''
    NoOfAuth = ''
    TransactionId = ''
    Record = ''
    Ofsrecord = ''
    Record<FT.TRANSACTION.TYPE> = 'AC'
    Record<FT.DEBIT.ACCT.NO> = 35254
    Record<FT.DEBIT.CURRENCY> = 'GBP'
    Record<FT.CREDIT.ACCT.NO> = 35262
    Record<FT.CREDIT.CURRENCY> = 'GBP'
    EB.Foundation.OfsBuildRecord(AppName, Ofsfunct, Process, Ofsversion, Gtsmode, NoOfAuth, TransactionId, Record, Ofsrecord)

    OfsSourceId = 'GENERIC.OFS.PROCESS'
    OfsRec = ''
    Options = ''
    EB.Interface.OfsPostMessage(OfsMsgId, OfsRec, OfsSourceId, Options)
    EB.TransactionControl.JournalUpdate('')
END