* @ValidationCode : MjoyMTU4NTAyMDE6Q3AxMjUyOjE2MjU3MjA5MzA5Njk6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 08 Jul 2021 11:08:50
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.I.DEBIT.AMT.CHECK
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
*Subroutine Description: This routine FT debit amt check
*Subroutine Type       : Input
*Attached To           : version
*Attached As           : ROUTINE
*Developed by          : S.M. Sayeed
*Designation           : Technical Consultant
*Email                 : s.m.sayeed@fortress-global.com
*Incoming Parameters   :
*Outgoing Parameters   :
*-----------------------------------------------------------------------------
* Modification History : Y.COMPANY and Unnecessary open files are commented out.
* 1)
*    Date : 08 JULY 2021
*    Modification Description : LINE NO: 59,60,63,64,74,75 & 90 are COMMENTED.
*    Modified By  : MD SHIBLI MOLLAH - FDS BD
*
*-----------------------------------------------------------------------------
*
*1/S----Modification Start
*
*1/E----Modification End
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BD.MBL.LPC.CHARGE.REC
    
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING FT.Contract
    $USING EB.DataAccess
    $USING AC.AccountOpening
    $USING EB.Updates
    $USING AA.PaymentSchedule
    $USING AA.TermAmount
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*---------------------------------------------------------------------
******
INIT:
******
*    FN.LPC.CRG = 'F.BD.MBL.LPC.CHARGE.REC'
*    F.LPC.CRG = ''
    FN.ACCT = 'F.ACCOUNT'
    F.ACCT = ''
*    FN.AA.AC = 'F.AA.ACCOUNT.DETAILS'
*    F.AA.AC = ''
    Y.APP ='FUNDS.TRANSFER'
    Y.FIELD = 'LT.FT.LPC.CRG':VM:'LT.FT.TOT.AMT'
    Y.LPC.SETTLE.POS=''
    Y.TOT.FUND.AMT.POS = ''
RETURN
**********
OPENFILES:
**********
    EB.DataAccess.Opf(FN.ACCT,F.ACCT)
*    EB.DataAccess.Opf(FN.LPC.CRG, F.LPC.CRG)
*    EB.DataAccess.Opf(FN.AA.AC, F.AA.AC)
RETURN
********
PROCESS:
********
    Y.CREDIT.ACCT.ID = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
    EB.DataAccess.FRead(FN.ACCT, Y.CREDIT.ACCT.ID, REC.ACCT, F.ACCT, ERR)
    Y.ARR.ID  = REC.ACCT<AC.AccountOpening.Account.ArrangementId>
   
    EB.Updates.MultiGetLocRef(Y.APP, Y.FIELD, Y.POSITION)
    Y.LPC.SETTLE.POS = Y.POSITION<1,1>
    Y.TOT.FUND.AMT.POS = Y.POSITION<1,2>
    Y.TOT.LOCAL.FIELD = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
    Y.LPC.ADJUST.AMT = Y.TOT.LOCAL.FIELD<1,Y.LPC.SETTLE.POS>
    Y.TOTAL.SETTLE.AMT = Y.TOT.LOCAL.FIELD<1,Y.TOT.FUND.AMT.POS>
* Y.COMPANY = EB.SystemTables.getIdCompany()

    Y.CREDIT.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAmount)
    Y.WORKING.BAL = REC.ACCT<AC.AccountOpening.Account.WorkingBalance>
    
    IF Y.WORKING.BAL LT Y.TOTAL.SETTLE.AMT THEN
        EB.SystemTables.setEtext("Debit Amount is Greater Than Working Balance")
    END
    
RETURN
END
