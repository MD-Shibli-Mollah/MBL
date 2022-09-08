* @ValidationCode : MjotODIwNjg3MzE2OkNwMTI1MjoxNjAwNzU4NDU3NDI2OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 22 Sep 2020 13:07:37
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
 
SUBROUTINE CR.MBL.E.NOF.LOAN.LIST(Y.DATA)
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
    $USING AC.EntryCreation
    
    GOSUB INTT
    GOSUB OPNFILE
    GOSUB PROCESS
RETURN

*============*
INTT:
*============*

    LOCATE "CATEGORY" IN EB.Reports.getEnqSelection()<2,1> SETTING CAT.POS THEN
        Y.CAT.ID = EB.Reports.getEnqSelection()<4,CAT.POS>
    END
    LOCATE "FROM.DATE" IN EB.Reports.getEnqSelection()<2,1> SETTING FROM.POS THEN
        Y.FROM.DATE = EB.Reports.getEnqSelection()<4,FROM.POS>
    END
    LOCATE "TO.DATE" IN EB.Reports.getEnqSelection()<2,1> SETTING TO.POS THEN
        Y.TO.DATE = EB.Reports.getEnqSelection()<4,TO.POS>
    END
    
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
    F.ACCT.DETAILS = ""
    
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    
    FN.AA.ARR.ACCT = "F.AA.ARR.ACCOUNT"
    F.AA.ARR.ACCT =""
    
    FN.AC.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AC.DETAILS = ''
    
    FN.AA.INT.ACCR = "F.AA.INTEREST.ACCRUALS"
    F.AA.INT.ACCR = ""
    
    FN.LIMIT="F.AA.ARR.LIMIT"
    F.LIMIT=""
    
    FN.LIM="F.LIMIT"
    F.LIM=""
    
    FN.COM="F.COMPANY"
    F.COM=""
    
    FN.CAT = 'F.CATEGORY'
    F.CAT = ''
    
    FN.STMT = 'F.STMT.ENTRY'
    F.STMT = ''
    
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
    EB.DataAccess.Opf(FN.AA.INT.ACCR, F.AA.INT.ACCR)
    EB.DataAccess.Opf(FN.STMT, F.STMT)
    
RETURN
 
PROCESS:

*---------------------------------------------------------------------
    Y.MNEMONIC = FN.ACCT.DETAILS[2,3]
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.PD.PROPER = "INTONOD"
        Y.INT.PROPER = "PRINCIPALINT"
        Y.CR.ACC = 'CURACCOUNT'
        Y.PR.INT = 'ACCPRINCIPALINT'
        Y.PD.BAL.TYPE.ALL = 'DELACCOUNT':VM:'DOFACCOUNT':VM:'DUEACCOUNT':VM:'GRCACCOUNT':VM:'NABACCOUNT':VM:'SMAACCOUNT':VM:'STDACCOUNT':VM:'SUBACCOUNT'
        Y.PD.PFT.ALL = 'ACCINTONOD':VM:'DELPRINCIPALINT':VM:'DOFPRINCIPALINT':VM:'DUEPRINCIPALINT':VM:'GRCPRINCIPALINT':VM:'NABPRINCIPALINT':VM:'SMAPRINCIPALINT':VM:'STDPRINCIPALINT':VM:'SUBPRINCIPALINT'
        
    END
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.PD.PROPER = "PFTONOD"
        Y.INT.PROPER = "DEFERREDPFT"
        Y.CR.ACC = 'CURISACCOUNT'
        Y.PR.INT = 'ACCDEFERREDPFT'
        Y.PD.BAL.TYPE.ALL = 'DELISACCOUNT':VM:'DOFISACCOUNT':VM:'DUEISACCOUNT':VM:'GRCISACCOUNT':VM:'NABISACCOUNT':VM:'SMAISACCOUNT':VM:'STDISACCOUNT':VM:'SUBISACCOUNT'
        Y.PD.PFT.ALL = 'ACCPFTONOD'VM:'DELDEFERREDPFT':VM:'DOFDEFERREDPFT':VM:'DUEDEFERREDPFT':VM:'GRCDEFERREDPFT':VM:'NABDEFERREDPFT':VM:'SMADEFERREDPFT':VM:'STDDEFERREDPFT':VM:'SUBDEFERREDPFT'
    END

*Loan ID
    SEL.CMD = 'SELECT ':FN.ACC:' WITH CATEGORY EQ ':Y.CAT.ID
*
    EB.DataAccess.Readlist(SEL.CMD, SEL.LIST, '', NO.OF.REC, SystemReturnCode)
    
    LOOP
        REMOVE Y.ACC.ID FROM SEL.LIST SETTING POS
    WHILE Y.ACC.ID:POS
    
        EB.DataAccess.FRead(FN.ACC, Y.ACC.ID, REC.ACC, F.ACC, Er.RRR.1)
        Y.ARR.ID = REC.ACC<AC.AccountOpening.Account.ArrangementId>
            
        SEL.CMD2 = 'SELECT ':FN.STMT:' WITH OUR.REFERENCE EQ ':Y.ACC.ID:' AND VALUE.DATE GE ':Y.FROM.DATE:' AND VALUE.DATE LE ':Y.TO.DATE
        
        EB.DataAccess.Readlist(SEL.CMD2, SEL.LIST2, '', NO.OF.REC2, SystemReturnCode2)
        
        LOOP
            REMOVE Y.STMT.ID FROM SEL.LIST2 SETTING POS
        WHILE Y.STMT.ID:POS
    
            EB.DataAccess.FRead(FN.STMT, Y.STMT.ID, REC.STMT, F.STMT, Er.RRR.2)
            Y.AMT.LCY = REC.STMT<AC.EntryCreation.StmtEntry.SteAmountLcy>
        
            IF Y.AMT.LCY GT 0 THEN
                Y.PRIN.CR = Y.AMT.LCY
            END
            IF Y.AMT.LCY LT 0 THEN
                Y.PRIN.DR = Y.AMT.LCY
            END
        
            EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.AA, F.AA.ARRANGEMENT, ERR.ARR.3)
            Y.LN.NUM = Y.ARR.ID
            Y.LN.TYP= REC.AA<AA.Framework.Arrangement.ArrProduct>
            Y.CUS.ID = REC.AA<AA.Framework.Arrangement.ArrCustomer>
            Y.ACC.NUM = REC.AA<AA.Framework.Arrangement.ArrLinkedApplId>
            Y.PROD.LINE = REC.AA<AA.Framework.Arrangement.ArrProductLine>
            Y.CURRENCY = REC.AA<AA.Framework.Arrangement.ArrCurrency>
            Y.ARR.STATUS = REC.AA<AA.Framework.Arrangement.ArrArrStatus>
    
*-----------------------------------------------------------------------------
*Customer Name
    
            EB.DataAccess.FRead(FN.CUS, Y.CUS.ID, R.CUS, F.CUS, CUS.ERR)
            Y.CUS.TITLE= R.CUS<ST.Customer.Customer.EbCusShortName>
   
*---------------------------------------------------------------------------------------
*PRODUCT DESC
            EB.DataAccess.FRead(FN.AA.PRODUCT, Y.LN.TYP, PRD.REC, F.AA.PRODUCT, PRD.ERR)
            Y.PRD.DES = PRD.REC<AA.ProductManagement.Product.PdtDescription>
*---------------------------------------------------------------------------------------
*VALUE DATE, MATURITY DATE
            EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.ARR.ID, REC.ACC.DET, F.ACCT.DETAILS, Er)
            Y.VALUE.DATE = REC.ACC.DET<AA.PaymentSchedule.AccountDetails.AdBaseDate>
            Y.MATURITY.DATE = REC.ACC.DET<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
*----------------------------------------------------------------------------------------
*DEFERREDPFT Rate
            PROP.CLASS = 'INTEREST'
            PROPERTY.P= Y.INT.PROPER
            AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY.P,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
            PR.REC = RAISE(RETURN.VALUES)
            Y.INT.RATE = PR.REC<AA.Interest.Interest.IntEffectiveRate>
*-----------------------------------------------------------------------------------------------
*
*Regular Outstanding
            Y.CUR.ACC = Y.CR.ACC
            Y.TODAY=EB.SystemTables.getToday()
            AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.ACC, Y.TODAY, TOT.CUR.AMT, RetError)
            Y.CUR.BAL = TOT.CUR.AMT
*--------------------------DEFERRED PROFIT ACCRUAL----------------------------------------------
            AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PR.INT, Y.TODAY, TOT.PR.INT.AMT, RetError1)
            Y.CUR.INT = TOT.PR.INT.AMT
*-------------------------------------Y.PD.PFT.ALL----------------------------------------------------------------
            Y.TOT.PD.PFT.DCOUNT = DCOUNT(Y.PD.PFT.ALL,VM)
    
            FOR I=1 TO Y.TOT.PD.PFT.DCOUNT
                Y.PD.PFT = Y.PD.PFT.ALL<1,I>
                AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PD.PFT, Y.TODAY, TOT.PD.PFT, RetError2)
                Y.PD.PFT.AMT = TOT.PD.PFT
                Y.TOT.PD.PFT.AMT = Y.TOT.PD.PFT.AMT + Y.PD.PFT.AMT
                Y.PD.PFT = ''
                Y.PD.PFT.AMT = ''
            NEXT I
    
            Y.PD.INT.AMT = Y.TOT.PD.PFT.AMT
*--------------------------------PD BALANCE-----Y.PD.BAL.TYPE----------------------------------------------------
            Y.TOT.PD.BAL.TYPE.DCOUNT = DCOUNT(Y.PD.BAL.TYPE.ALL,VM)
    
            FOR I=1 TO Y.TOT.PD.BAL.TYPE.DCOUNT
                Y.PD.BAL.TYPE = Y.PD.BAL.TYPE.ALL<1,I>
                AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM,Y.PD.BAL.TYPE, Y.TODAY, TOT.PD.BAL.AMT, RetError3)
                Y.PD.AMT = TOT.PD.BAL.AMT
                Y.TOT.PD.BAL.AMT = Y.TOT.PD.BAL.AMT + Y.PD.AMT
                Y.PD.BAL.TYPE = ''
                Y.PD.AMT = ''
            NEXT I
    
            Y.CUR.BAL = ABS(Y.CUR.BAL)
            Y.PRIN.OUT = Y.CUR.BAL
            Y.CUR.INT = ABS(Y.CUR.INT)
            Y.PD.BAL.AMT = Y.TOT.PD.BAL.AMT
            Y.PD.BAL.AMT = ABS(Y.PD.BAL.AMT)
            Y.PD.INT.AMT = ABS(Y.PD.INT.AMT)
            Y.PD.OUT = Y.PD.BAL.AMT + Y.PD.INT.AMT
    
            Y.CUR.TOT.OUT = Y.CUR.BAL + Y.CUR.INT + Y.PD.OUT
*----------------------------------END----------------------------------------------------

            Y.DATA<-1>= Y.LN.NUM:'*':Y.CUS.TITLE:'*':Y.PRIN.DR:'*':Y.PRIN.CR:'*':Y.PRIN.OUT:'*':Y.INT.DR:'*':Y.INT.CR:'*':Y.INT.OUT:'*':Y.PD.OUT:'*':Y.TOT.OUT
*                           1            2               3            4              5                6          7            8             9              10
            Y.LN.NUM = ''
            Y.CAT.DES = ''
            Y.VALUE.DATE = ''
            Y.PRIN.OUT = ''
            Y.TOT.OUT = ''
            Y.INT.OUT = ''
            Y.INT.DR = ''
            Y.INT.CR = ''
            Y.PRIN.DR = ''
            Y.PRIN.CR = ''
    
        REPEAT
    REPEAT
RETURN
END