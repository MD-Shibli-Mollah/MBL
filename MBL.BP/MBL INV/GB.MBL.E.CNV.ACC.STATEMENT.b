* @ValidationCode : MjotMTY4MzYwNDAwMDpDcDEyNTI6MTYxMDAxNzM1MzA1Mzp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 07 Jan 2021 17:02:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.E.CNV.ACC.STATEMENT
* Modification History :
*                    CREATED BY: SHAFIUL AZAM --- FDS BD. 19TH OCT 2020
*                    MODIFIED BY:
*                    MD SHIBLI MOLLAH FDS BANGLADESH LTD. 07TH JAN 2021
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
     
    $USING ST.Customer
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING AA.PaymentSchedule
    $USING AA.Interest
    $USING EB.Reports
    $USING AA.Framework
    $USING AA.TermAmount
    $USING AA.ProductManagement
    $USING ST.CompanyCreation
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    EB.Reports.setOData(Y.RETURN)
RETURN
 
*----
INIT:
*----

    Y.COMP = EB.SystemTables.getIdCompany()
    Y.TODAY = EB.SystemTables.getToday()
    
    Y.ACCOUNT.NUMBER = EB.Reports.getOData()
    
    Y.COUNT = 0
     
    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    FN.CUST = 'F.CUSTOMER'
    F.CUST = ''
    FN.ACCT.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.ACCT.DETAILS = ''
    FN.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.ARRANGEMENT = ''
    FN.COMP = 'F.COMPANY'
    F.COMP = ''
    
    FN.PRODUCT = 'F.AA.PRODUCT'
    F.PRODUCT = ''
    
RETURN
*----------
OPENFILES:
*----------
    EB.DataAccess.Opf(FN.COMP, F.COMP)
    EB.DataAccess.Opf(FN.AC, F.AC)
    EB.DataAccess.Opf(FN.PRODUCT, F.PRODUCT)
    EB.DataAccess.Opf(FN.CUST, F.CUST)
    EB.DataAccess.Opf(FN.ACCT.DETAILS, F.ACCT.DETAILS)
    EB.DataAccess.Opf(FN.ARRANGEMENT, F.ARRANGEMENT)
RETURN
*--------
PROCESS:
*--------
    
    EB.DataAccess.FRead(FN.AC,Y.ACCOUNT.NUMBER,R.AC.REC,F.AC,Y.ERR)
    Y.CUS.ID = R.AC.REC<AC.AccountOpening.Account.Customer>
    Y.ACCT.SHORT.TITLE = R.AC.REC<AC.AccountOpening.Account.ShortTitle>
    Y.ARRANGEMENT.ID = R.AC.REC<AC.AccountOpening.Account.ArrangementId>
    Y.PC.ID = R.AC.REC<AC.AccountOpening.Account.AltAcctId>

    EB.DataAccess.FRead(FN.ARRANGEMENT, Y.ARRANGEMENT.ID, R.ARRANGEMENT.REC,F.ARRANGEMENT, Er)
    Y.PRODUCT = R.ARRANGEMENT.REC<AA.Framework.Arrangement.ArrProduct>
    Y.PRODUCT.GROUP = R.ARRANGEMENT.REC<AA.Framework.Arrangement.ArrProductGroup>
    Y.CURRENCY = R.ARRANGEMENT.REC<AA.Framework.Arrangement.ArrCurrency>
    Y.ACTIVE.STATUS = R.ARRANGEMENT.REC<AA.Framework.Arrangement.ArrArrStatus>
    Y.OPENING.DATE =R.ARRANGEMENT.REC<AA.Framework.Arrangement.ArrStartDate>
    
    EB.DataAccess.FRead(FN.PRODUCT, Y.PRODUCT, PRD.REC, F.PRODUCT, Er)
    Y.PRODUCT.NAME = PRD.REC<AA.ProductManagement.Product.PdtDescription>
    
    EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.ARRANGEMENT.ID, R.ACCT.DET.REC, F.ACCT.DETAILS, Er)
    Y.MATURITY.DATE = R.ACCT.DET.REC<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
    Y.TOT.BL.TYPE = R.ACCT.DET.REC<AA.PaymentSchedule.AccountDetails.AdBillType>
    Y.TOT.SET.STATUS = R.ACCT.DET.REC<AA.PaymentSchedule.AccountDetails.AdSetStatus>
    CONVERT SM TO VM IN Y.TOT.BL.TYPE
    CONVERT SM TO VM IN Y.TOT.SET.STATUS
    Y.DCOUNT = DCOUNT(Y.TOT.BL.TYPE,VM)
    FOR I = 1 TO Y.DCOUNT
        Y.BL.TYPE = Y.TOT.BL.TYPE<1,I>
        Y.SET.STATUS = Y.TOT.SET.STATUS<1,I>
        IF Y.BL.TYPE EQ 'EXPECTED'  AND Y.SET.STATUS EQ 'PAID' THEN
            Y.COUNT = Y.COUNT + 1
        END
    NEXT I
    Y.BILL.PAID = Y.COUNT
    
    PROP.CLASS.TA = 'TERM.AMOUNT'
    AA.Framework.GetArrangementConditions(Y.ARRANGEMENT.ID, PROP.CLASS.TA, PROPERTY, Effectivedate, RETURN.IDS, RETURN.TERM.VALUES, ERR.MSG)
    R.ACC.REC.TA = RAISE(RETURN.TERM.VALUES)
    Y.TERM.AMT.RATE = R.ACC.REC.TA<AA.TermAmount.TermAmount.AmtAmount>
    Y.TERM = R.ACC.REC.TA<AA.TermAmount.TermAmount.AmtTerm>
    Y.TERM.M.Y = (RIGHT(Y.TERM,1))
    Y.TERM.EX.VAL = LEFT(Y.TERM,LEN(Y.TERM)-LEN(Y.TERM.M.Y))
    IF(Y.TERM.M.Y EQ 'M') THEN
        Y.TERM = Y.TERM.EX.VAL - Y.BILL.PAID
    END ELSE
        Y.TERM  = Y.TERM.EX.VAL * 12 - Y.BILL.PAID
    END
    PROP.CLASS.PS = 'PAYMENT.SCHEDULE'
    AA.Framework.GetArrangementConditions(Y.ARRANGEMENT.ID, PROP.CLASS.PS, PROPERTY, Effectivedate, RETURN.IDS, RETURN.PAYMENT.VALUES, ERR.MSG)
    R.ACC.REC2.PS = RAISE(RETURN.PAYMENT.VALUES)
    Y.ACTUAL.AMT = R.ACC.REC2.PS<AA.PaymentSchedule.PaymentSchedule.PsActualAmt>
    
    PROP.CLASS.INT = 'INTEREST'
    AA.Framework.GetArrangementConditions(Y.ARRANGEMENT.ID, PROP.CLASS.INT, PROPERTY, Effectivedate, RETURN.IDS, RETURN.INTEREST.VALUES, ERR.MSG)
    R.ACC.REC3.INT = RAISE(RETURN.INTEREST.VALUES)
    Y.INTEREST.RATE = R.ACC.REC3.INT<AA.Interest.Interest.IntEffectiveRate>
    
******************************

*------------------------READ COMPANY----------------------------------------------------
    EB.DataAccess.FRead(FN.COMP, Y.COMP, REC.COMP, F.COMP, Er)
    Y.COMP.FINMNE = REC.COMP<ST.CompanyCreation.Company.EbComFinancialMne>
*-----------------------------end--------------------------------------------------------
*-----------------------Principal--------------------------------
    RequestType<2> = 'ALL'  ;* Unauthorised Movements required.
    RequestType<3> = 'ALL'  ;* Projected Movements requierd
    RequestType<4> = 'ECB'  ;* Balance file to be used
    RequestType<4,2> = 'END'    ;* Balance required as on TODAY - though Activity date can be less than today
    
    IF Y.COMP.FINMNE EQ 'BNK' THEN
        BaseBalance = 'CURACCOUNT'
    END
     
    IF Y.COMP.FINMNE EQ 'ISL' THEN
        BaseBalance = 'CURISACCOUNT'
    END
    Y.PAYMENT.DATE = Y.TODAY
    AA.Framework.GetPeriodBalances(Y.ACCOUNT.NUMBER, BaseBalance, RequestType, Y.PAYMENT.DATE, Y.PAYMENT.DATE, Y.PAYMENT.DATE, BalDetails, ErrorMessage)
*
    Y.CREDIT.MVMT = BalDetails<2>
    Y.DEBIT.MVMT = BalDetails<3>
    Y.PRINCIPAL.AMT = BalDetails<4>
*----------------------------------END----------------------------------------------------

*Mudarabah Masik Sanchay IS.MBL.MMSP.DP  View    Amend   New Product Products
*Mudaraba Term Deposit   IS.MBL.MTD.DP

    Y.GT.DT = TIMEDATE()
    Y.GT.DT.DISP = "Generation Time & Date: ":Y.GT.DT
************************************
    
    Y.RETURN = Y.CUS.ID:"*":Y.ACCOUNT.NUMBER:"*":Y.PC.ID:"*":Y.PRODUCT.NAME:"*":Y.CURRENCY:"*":Y.ACTIVE.STATUS:"*":Y.OPENING.DATE:"*":Y.MATURITY.DATE:"*":Y.BILL.PAID:"*":Y.TERM.AMT.RATE:"*":Y.TERM:"*":Y.ACTUAL.AMT:"*":Y.INTEREST.RATE:"*":Y.ACCT.SHORT.TITLE:"*":Y.PRODUCT.GROUP:"*":Y.PRINCIPAL.AMT:"*":Y.GT.DT.DISP
*                  1                2               3              4                5                6                   7                    8               9               10               11              12               13                   14                     15                   16                17
RETURN
END