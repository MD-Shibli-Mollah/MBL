* @ValidationCode : MjotMjA1NjA2ODU1OTpDcDEyNTI6MTYwNjgwOTIwNTMxNjp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 01 Dec 2020 13:53:25
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

SUBROUTINE CR.MBL.E.NOF.LOAN.SETTLEMENT(Y.DATA)
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

    LOCATE "ARR.ID" IN EB.Reports.getEnqSelection()<2,1> SETTING ARR.POS THEN
        Y.ARR.ID = EB.Reports.getEnqSelection()<4,ARR.POS>
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
    
RETURN

PROCESS:

*---------------------------------------------------------------------
    Y.MNEMONIC = FN.ACCT.DETAILS[2,3]
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.PD.PROPER = "INTONOD"
        Y.INT.PROPER = "PRINCIPALINT"
        Y.CR.ACC = 'CURACCOUNT'
        Y.PR.INT = 'ACCPRINCIPALINT'
        Y.DUE.ACC = "DUEACCOUNT"
        Y.PD.BAL.TYPE.ALL = 'DELACCOUNT':VM:'DOFACCOUNT':VM:'GRCACCOUNT':VM:'NABACCOUNT':VM:'SMAACCOUNT':VM:'STDACCOUNT':VM:'SUBACCOUNT'
        Y.PD.PFT.ALL = 'ACCINTONOD':VM:'DELPRINCIPALINT':VM:'DOFPRINCIPALINT':VM:'GRCPRINCIPALINT':VM:'NABPRINCIPALINT':VM:'SMAPRINCIPALINT':VM:'STDPRINCIPALINT':VM:'SUBPRINCIPALINT'
        
    END
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.PD.PROPER = "PFTONOD"
        Y.INT.PROPER = "DEFERREDPFT"
        Y.CR.ACC = 'CURISACCOUNT'
        Y.PR.INT = 'ACCDEFERREDPFT'
        Y.DUE.ACC = "DUEISACCOUNT"
        Y.PD.BAL.TYPE.ALL = 'DELISACCOUNT':VM:'DOFISACCOUNT':VM:'DUEISACCOUNT':VM:'GRCISACCOUNT':VM:'NABISACCOUNT':VM:'SMAISACCOUNT':VM:'STDISACCOUNT':VM:'SUBISACCOUNT'
        Y.PD.PFT.ALL = 'ACCPFTONOD':VM:'DELDEFERREDPFT':VM:'DOFDEFERREDPFT':VM:'DUEDEFERREDPFT':VM:'GRCDEFERREDPFT':VM:'NABDEFERREDPFT':VM:'SMADEFERREDPFT':VM:'STDDEFERREDPFT':VM:'SUBDEFERREDPFT':VM:'DELMARKUPPFT':VM:'DOFMARKUPPFT':VM:'DUEMARKUPPFT':VM:'GRCMARKUPPFT':VM:'NABMARKUPPFT':VM:'SMAMARKUPPFT':VM:'STDMARKUPPFT':VM:'SUBMARKUPPFT'
    
        Y.MARK.DEF.PFT.ALL = 'ACCDEFERREDPFT':VM:'DELDEFERREDPFT':VM:'DOFDEFERREDPFT':VM:'GRCDEFERREDPFT':VM:'NABDEFERREDPFT':VM:'SMADEFERREDPFT':VM:'STDDEFERREDPFT':VM:'SUBDEFERREDPFT':VM:'RECMARKUPPFT':VM:'STDMARKUPPFT':VM:'DELMARKUPPFT':VM:'DOFMARKUPPFT':VM:'GRCMARKUPPFT':VM:'NABMARKUPPFT':VM:'SMAMARKUPPFT':VM:'SUBMARKUPPFT'
        Y.PENAL.PFT.ALL = 'ACCPENALTYPFT'
        
*-----------------Pft on OD IS ONLY ACCPFTONOD--------------------------****
        Y.PFT.OD.BAL.TYPE = 'ACCPFTONOD'
*---------------------MARKUP/DEFERRED PFT ALL BAL TYPE----------------------***
        Y.PD.PFT.ALL = Y.MARK.DEF.PFT.ALL
    END
*Loan ID
* Y.ARR.ID = 'AA21017VYV7H'
    EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.AA, F.AA.ARRANGEMENT, ERR.ARR)
    Y.LN.LC.NUM = Y.ARR.ID
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
*Limit Ref.
    SEL.LIMIT.CMD = "SELECT ":FN.LIMIT:" WITH @ID LIKE ":Y.ARR.ID:"-":"LIMIT-..."
    EB.DataAccess.Readlist(SEL.LIMIT.CMD, S.LIST,"",NO.OF.REC,R.CODE)
    Y.LN.ID = S.LIST<1,NO.OF.REC>
    EB.DataAccess.FRead(FN.LIMIT, Y.LN.ID, R.ARRNGMNT, F.LIMIT, L.ERR)
    Y.LIMIT.REF= R.ARRNGMNT<AA.Limit.Limit.LimLimitReference>
    Y.LIMIT.SER= R.ARRNGMNT<AA.Limit.Limit.LimLimitSerial>
    
    Y.LIMIT= Y.LIMIT.REF:".":Y.LIMIT.SER
    
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
    Y.LIM.EXP.DATE = R.ARGMNT<LI.Config.Limit.ExpiryDate>
*Limit Amount
    Y.LIM.AMT = R.ARGMNT<LI.Config.Limit.InternalAmount>
*----------------------------------------------------------------------------------------
*DEFERREDPFT Rate
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
  
*------------------DUEACCOUNT/DUEISACCOUNT MUST BE CONSIDERED IN PR AMT-------------------
    Y.CUR.DUE.ACC = Y.DUE.ACC
    AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.DUE.ACC, Y.TODAY, TOT.DUE.AMT, RetError.2)
    Y.CUR.DUE.BAL = TOT.DUE.AMT

*--------------------------PFT ON OD --OVERDUE PROFIT BILLS--------------------------------
    AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PFT.OD.BAL.TYPE, Y.TODAY, Y.PFT.OD.AMT, RetError4)
    Y.PFT.OD = Y.PFT.OD.AMT
    Y.CUR.INT = Y.PFT.OD
*-------------------------------------Y.PD.PFT.ALL--------Y.MARK.DEF.PFT.ALL--------------------------------------

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

*--------------DUE PRINCIPAL PROFIT (SINGLE DAY ONLY)--------------------

    AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.DUE.PR.INT, Y.TODAY, DUE.PFT.AMT, RetError3)
    Y.DUE.PFT.AMT = DUE.PFT.AMT

*------------FOR DUE MARKUP PFT -------------------------------------
    AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.DUE.MRK.PR.INT, Y.TODAY, DUE.MRK.PFT.AMT, RetError3)
    Y.DUE.MRK.PFT.AMT = DUE.MRK.PFT.AMT
       
*----------------------------PENALTY PFT ALL-----------------------------

    Y.PENAL.PFT = Y.PENAL.PFT.ALL
    AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PENAL.PFT, Y.TODAY, TOT.PENAL.PFT.AMT, RetError3)
    Y.PENAL.PFT.AMT = TOT.PENAL.PFT.AMT
    Y.TOT.PENAL.PFT.AMT = Y.TOT.PENAL.PFT.AMT + Y.PENAL.PFT.AMT
*-----------------------------------------------------------------------------------------
*-----------------OVERDUE/BILLS ONLY------------------------------
    Y.OD.PR.AMT.ALL = Y.TOT.PD.BAL.AMT
    Y.OD.PR.AMT.ALL = Y.OD.PR.AMT.ALL * -1
*------------------------------------------------------------------
    Y.CUR.DUE.BAL = Y.CUR.DUE.BAL * -1
    Y.PR.AMT = Y.CUR.BAL + Y.CUR.DUE.BAL
*--------------------------------------------------------------------
*
    Y.CUR.BAL = Y.PR.AMT
    Y.CUR.BAL = ABS(Y.CUR.BAL)
    Y.CUR.INT = ABS(Y.CUR.INT)
    Y.PD.BAL.AMT = Y.TOT.PD.BAL.AMT
    Y.PD.BAL.AMT = ABS(Y.PD.BAL.AMT)
    Y.PD.INT.AMT = ABS(Y.PD.INT.AMT)
    Y.PD.OUT = Y.PD.BAL.AMT + Y.CUR.INT
    
*

*----------------------------------END----------------------------------------------------
*-----------------------------------Y.PD.INT.AMT >> MUST SWIPE WITH Y.CUR.INT ------------------------
    Y.DATA<-1>= Y.CUS.TITLE:'*':Y.PRD.DES:'*':Y.LN.LC.NUM:'*':Y.CUR.BAL:'*':Y.PD.INT.AMT:'*':Y.PD.BAL.AMT:'*':Y.CUR.INT:'*':Y.PD.OUT:'*':Y.PD.ID:'*':Y.LIM.AMT:'*':Y.LIM.EXP.DATE:'*':Y.INT.RATE:'*':Y.EOL.LIMIT:'*':Y.TOT.PENAL.PFT.AMT
*                   1            2               3                4           5                6                7               8            9          10              11                  12           13             14
    Y.LN.LC.NUM = ''
    Y.CAT.DES = ''
    Y.VALUE.DATE = ''
    Y.CUR.INT = ''
    Y.PD.ID = ''
    Y.PD.OUT = ''
    Y.CUR.BAL = ''
    Y.PD.INT.AMT = ''
    Y.PD.BAL.AMT = ''
    Y.AD.MATURITY.DATE = ''
    Y.LIMIT = ''
    Y.PRD.DES = ''
    Y.SANC.DATE= ''
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
    Y.EOL.LIMIT = ''
    Y.TOT.PRFT.PD.AMT = ''
    Y.TOT.PD.BAL.AMT = ''
    Y.TOT.PD.PFT.AMT = ''
    Y.TOT.PENAL.PFT.AMT = ''
RETURN
END