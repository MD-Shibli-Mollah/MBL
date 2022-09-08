* @ValidationCode : MjoxNDcwMjc5OTpDcDEyNTI6MTYwMDMyODAyNjgyMDp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Sep 2020 13:33:46
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
* @AUTHOR         : MD SHIBLI MOLLAH

SUBROUTINE CR.MBL.E.NOF.BD.LOAN.REPORT(Y.DATA)
*-----------------------------------------------------------------------------
*
    $INSERT  I_COMMON
    $INSERT  I_EQUATE

    $USING EB.Reports
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING AA.Framework
    $USING RE.ConBalanceUpdates
    $USING AA.Account
    $USING AA.Limit
    $USING ST.Customer
    $USING LI.Config
    $USING AA.Interest
    $USING ST.CompanyCreation
    $USING AA.PaymentSchedule
    $USING AA.ProductManagement
    $USING ST.Config
    
    GOSUB INTT
    GOSUB OPNFILE
    GOSUB PROCESS
RETURN

*============*
INTT:
*============*

*    LOCATE "CUSTOMER" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
**Customer ID
*        Y.CUS.ID=EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
*END
    
    FN.CUS="F.CUSTOMER"
    F.CUS=""
    
    FN.BILL.DETAILS= "F.AA.BILL.DETAILS"
    F.BILL.DETAILS = ""
    
    FN.CUSTOMER.ACCOUNT = "F.CUSTOMER.ACCOUNT"
    F.CUSTOMER.ACCOUNT = ""
    
    FN.AA.ARRANGEMENT ="F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT= ""
    FN.AA.PRODUCT="F.AA.PRODUCT"
    F.AA.PRODUCT=""
    
    FN.ACCT.DETAILS = "F.AA.ACCOUNT.DETAILS"
    F.ACCT.DETAILS =""
    
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    
    FN.AA.ARR.ACCT = "F.AA.ARR.ACCOUNT"
    F.AA.ARR.ACCT =""
    
    FN.LIMIT="F.AA.ARR.LIMIT"
    F.LIMIT=""
    
    FN.LIM="F.LIMIT"
    F.LIM=""
    
    FN.COM="F.COMPANY"
    F.COM=""
    
    FN.CAT = 'F.CATEGORY'
    F.CAT = ''
    
RETURN
*============*
OPNFILE:
*============*
    EB.DataAccess.Opf(FN.CUSTOMER.ACCOUNT, F.CUSTOMER.ACCOUNT)
    EB.DataAccess.Opf(FN.BILL.DETAILS, F.BILL.DETAILS)
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.ACCT.DETAILS, F.ACCT.DETAILS)
    EB.DataAccess.Opf(FN.AA.ARR.ACCT, F.AA.ARR.ACCT)
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.LIM,F.LIM)
    EB.DataAccess.Opf(FN.COM,F.COM)
    EB.DataAccess.Opf(FN.AA.PRODUCT,F.AA.PRODUCT)
    EB.DataAccess.Opf(FN.CAT,F.CAT)
    EB.DataAccess.Opf(FN.ACC, F.ACC)

RETURN

PROCESS:
    
*---------------------------------------------------------------------
    Y.MNEMONIC = FN.ACCT.DETAILS[2,3]
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.OD.PROPER = "INTONOD"
        Y.INT.PROPER = "PRINCIPALINT"
        Y.CR.ACC = 'CURACCOUNT'
    END
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.OD.PROPER = "PFTONOD"
        Y.INT.PROPER = "DEFERREDPFT"
        Y.CR.ACC = 'CURISACCOUNT'
    END

*---------------------------------------------------------------------------------------
    SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH PRODUCT.LINE EQ LENDING"
    EB.DataAccess.Readlist(SEL.CMD.ARR, SEL.LIST,"",NO.OF.RECORD,RET.CODE)

    FOR I=1 TO NO.OF.RECORD
        Y.ARR.ID = SEL.LIST<I>
        GOSUB ARR.PRINT
    NEXT I

RETURN

ARR.PRINT:
*============*
*Loan ID
    EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.AA, F.AA.ARRANGEMENT, ERR.ARR)
    Y.LN.LC.NUM = Y.ARR.ID
    Y.CUS.ID = REC.AA<AA.Framework.Arrangement.ArrCustomer>
    Y.ACC.NUM = REC.AA<AA.Framework.Arrangement.ArrLinkedApplId>
    Y.PROD.LINE = REC.AA<AA.Framework.Arrangement.ArrProductLine>
    Y.CURRENCY = REC.AA<AA.Framework.Arrangement.ArrCurrency>
    Y.ARR.STATUS = REC.AA<AA.Framework.Arrangement.ArrArrStatus>
    Y.LINK.ACC.NO = REC.AA<AA.Framework.Arrangement.ArrLinkedApplId>
    
*-----------------------------------------------------------------------------
*Customer Name
    
    EB.DataAccess.FRead(FN.CUS, Y.CUS.ID, R.CUS, F.CUS, CUS.ERR)
    Y.CUS.TITLE= R.CUS<ST.Customer.Customer.EbCusShortName>
    
*---------------------------------------------------------------------------------------
*Limit Ref.
    SEL.LIMIT.CMD = "SELECT ":FN.LIMIT:" WITH @ID LIKE ":Y.ARR.ID:"-":"LIMIT-..."
    EB.DataAccess.Readlist(SEL.LIMIT.CMD, S.LIST,"",NO.OF.REC,R.CODE)
    Y.LN.ID = S.LIST<1,NO.OF.REC>
    EB.DataAccess.FRead(FN.LIMIT, Y.LN.ID, R.ARRNGMNT, F.LIMIT, L.ERR)
    Y.LIMIT.REF= R.ARRNGMNT<AA.Limit.Limit.LimLimitReference>
    Y.LIMIT.SER= R.ARRNGMNT<AA.Limit.Limit.LimLimitSerial>
    Y.LIMIT=Y.LIMIT.REF:".":Y.LIMIT.SER
    
*---------------------------------------------------------------------------------------
*Loan Type
    EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.LN.LC.NUM, R.ARR, F.AA.ARRANGEMENT, LN.ERR.ARR)
    Y.LN.TYP=R.ARR<AA.Framework.Arrangement.ArrProduct>
    
    EB.DataAccess.FRead(FN.AA.PRODUCT, Y.LN.TYP, PRD.REC, F.AA.PRODUCT, PRD.ERR)
    Y.PRD.DES = PRD.REC<AA.ProductManagement.Product.PdtDescription>
*---------------------------------------------------------------------------------------

*Sanction Date
    Y.LN.ID = Y.CUS.ID:".":"000":Y.LIMIT
    EB.DataAccess.FRead(FN.LIM, Y.LN.ID, R.ARGMNT, F.LIM, LIM.ERR)
    Y.SANC.DATE=R.ARGMNT<LI.Config.Limit.ApprovalDate>
*Expiry Date
    Y.LIM.EXPIRY.DATE=R.ARGMNT<LI.Config.Limit.ExpiryDate>
*Limit Amount
    Y.LIMIT.AMT=R.ARGMNT<LI.Config.Limit.InternalAmount>
*----------------------------------------------------------------------------------------
*OD Rate

    PROP.CLASS = 'INTEREST'
    PROPERTY=Y.OD.PROPER
    AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    R.REC = RAISE(RETURN.VALUES)
    Y.REG.RATE = R.REC<AA.Interest.Interest.IntEffectiveRate>

*----------------------------------------------------------------------------------------------
*Regular int. Rate

    PROPERTY.P= Y.INT.PROPER
    AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY.P,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    PR.REC = RAISE(RETURN.VALUES)
    Y.INT.RATE = PR.REC<AA.Interest.Interest.IntEffectiveRate>
*-----------------------------------------------------------------------------------------------

*OD Outstanding
    
    EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.LN.LC.NUM, REC.ACCT.DET, F.ACCT.DETAILS, ERR.ACCT.DET)
    Y.TOT.BILL.ID = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBillId>
    Y.TOT.BL.STATUS = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBillStatus>
    Y.TOT.PAYMT.TYP = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdPayMethod>
    Y.VALUE.DATE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdValueDate>
    Y.AD.MATURITY.DATE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
    
    CONVERT SM TO VM IN Y.TOT.BL.STATUS
    CONVERT SM TO VM IN Y.TOT.PAYMT.TYP
    Y.STAT.DCOUNT = DCOUNT(Y.TOT.BL.STATUS,VM)


    FOR J=1 TO Y.STAT.DCOUNT
        Y.BL.STATUS = Y.TOT.BL.STATUS<1,J>
        Y.PAYMT.TYP = Y.TOT.PAYMT.TYP<1,J>
        Y.BILL.ID = Y.TOT.BILL.ID<1,J>
        IF Y.BL.STATUS NE 'SETTLED' AND Y.PAYMT.TYP EQ 'DUE' THEN
            EB.DataAccess.FRead(FN.BILL.DETAILS, Y.BILL.ID, REC.BILL.DET, F.BILL.DETAILS, ERR.BILL)
            Y.OR.AMT = REC.BILL.DET<AA.PaymentSchedule.BillDetails.BdOrPropAmount>
            Y.OR.AMT.DCOUNT= DCOUNT(Y.OR.AMT,VM)
            
            FOR X=1 TO Y.OR.AMT.DCOUNT
                Y.RES.OR.AMT = Y.OR.AMT<1,X>
                Y.TOT.OR.AMT = Y.TOT.OR.AMT+Y.RES.OR.AMT
            NEXT X
        END

    NEXT J
*---------------------------------------------------------------------------------------------------

*Regular Outstanding
    Y.CUR.ACC = Y.CR.ACC
    Y.TODAY=EB.SystemTables.getToday()
    AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.ACC, Y.TODAY, TOT.CUR.AMT, RetError)
    Y.REG.OUTSTANDING = TOT.CUR.AMT
*-----------------------------------------------------------------------------------------------------

*Total Outstanding
    Y.TOTAL.OUT= Y.REG.OUTSTANDING+Y.TOT.OR.AMT

*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
*Company Name
    Y.COM.CODE=R.ARRNGMNT<AA.Limit.Limit.LimCoCode>
    EB.DataAccess.FRead(FN.COM, Y.COM.CODE, R.COM, F.COM, C.ERR)
    Y.COM.NAME=R.COM<ST.CompanyCreation.Company.EbComCompanyName>
    
    
*--------------------------------------------------------------------------------------------------
    EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.LN.LC.NUM, REC.ACCT.DET, F.ACCT.DETAILS, ERR.ACCT.DET)
    Y.STAT=REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdAllAgeStatus>
    IF Y.STAT EQ "" THEN
        Y.STAT="STD"
        
    END
    ELSE
        Y.STAT=Y.STAT
    END
*---------------------------------------------------------------------------------------------
*----------------------ACCOUNT PROPERTY READ-----------------------------------------------------------
    PROP.CLASS.1 = 'ACCOUNT'
    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.LN.LC.NUM,PROP.CLASS.1,PROPERTY,'',RETURN.IDS,RETURN.VALUES.1,ERR.MSG)
    R.ACC.REC = RAISE(RETURN.VALUES.1)
* Y.ACC.NO = R.ACC.REC<AA.Account.Account.AcAccountReference>

*---------------------------------------------------------------------------------------------
    EB.DataAccess.FRead(FN.ACC, Y.LINK.ACC.NO, REC.ACC, F.ACC, Er.RRR)
    Y.ACC.CATEGORY = REC.ACC<AC.AccountOpening.Account.Category>
   
*----------------------CATEGORY READ-----------------------------------------------------------
    EB.DataAccess.FRead(FN.CAT, Y.ACC.CATEGORY, R.CAT, F.CAT, CAT.ERR)
    Y.CAT.DES = R.CAT<ST.Config.Category.EbCatDescription>
    
    Y.GT.DT = TIMEDATE()
*----------------------------------END----------------------------------------------------

    Y.DATA<-1>= Y.CUS.ID:'*':Y.LN.LC.NUM:'*':Y.CUS.TITLE:'*':Y.ACC.CATEGORY:'*':Y.CAT.DES:'*':Y.VALUE.DATE:'*':Y.AD.MATURITY.DATE:'*':Y.LIMIT.AMT:'*':Y.REG.RATE:'*':Y.INT.RATE:'*':Y.REG.OUTSTANDING:'*':Y.TOT.OR.AMT:'*':Y.TOTAL.OUT:'*':Y.STAT:'*':Y.COM.NAME:'*':Y.CURRENCY:'*':Y.GT.DT:'*':Y.ARR.STATUS
*                   1           2                3               4                   5              6                7                     8               9             10              11                    12              13            14          15              16            17            18
    Y.LN.LC.NUM = ''
    Y.CAT.DES = ''
    Y.VALUE.DATE = ''
    Y.AD.MATURITY.DATE = ''
    Y.LIMIT = ''
    Y.PRD.DES = ''
    Y.SANC.DATE= ''
    Y.LIM.EXPIRY.DATE= ''
    Y.LIMIT.AMT= ''
    Y.REG.RATE= ''
    Y.OD.RATE= ''
    Y.REG.OUTSTANDING= ''
    Y.TOT.OR.AMT= ''
    Y.TOTAL.OUT= ''
    Y.STAT= ''
    Y.INT.RATE = ''
    Y.COM.NAME= ''
    Y.GT.DT = ''
    Y.ARR.STATUS = ''

RETURN
END