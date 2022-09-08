* @ValidationCode : MjotMTMyMDAwOTk3NTpDcDEyNTI6MTYzMDIyMTMyMzcyODp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 29 Aug 2021 13:15:23
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CR.MBL.E.NOF.CUSTOMER.LOAN.DETAILS(Y.DATA)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 23TH NOV 2020
*   NEW OBS FOR ACCOUNT LINE LOAN,
*   AA.ARR.LIMIT UPDATED  --             25TH Aug 2021
            
*-----------------------------------------------------------------------------

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
    
    GOSUB INIT
    GOSUB OPNFILE
    GOSUB PROCESS
RETURN

*============*
INIT:
*============*

    LOCATE "CUSTOMER" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
*Customer ID
        Y.CUS.ID = EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
        
    END
    LOCATE "DATE.FROM" IN EB.Reports.getEnqSelection()<2,1> SETTING DATE.FROM.POS THEN
        Y.DATE.FROM.ID = EB.Reports.getEnqSelection()<4,DATE.FROM.POS>
    END
    LOCATE "DATE.TO" IN EB.Reports.getEnqSelection()<2,1> SETTING DATE.TO.POS THEN
        Y.DATE.TO.ID = EB.Reports.getEnqSelection()<4,DATE.TO.POS>
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
    F.ACCT.DETAILS =""
    
    FN.AA.ARR.ACCT = "F.AA.ARR.ACCOUNT"
    F.AA.ARR.ACCT =""
    
    FN.LIM = "F.LIMIT"
    F.LIM=""
    
    FN.COM = "F.COMPANY"
    F.COM=""
    
    Y.COMP = EB.SystemTables.getIdCompany()
    
RETURN
*============*
OPNFILE:
*============*
    EB.DataAccess.Opf(FN.CUSTOMER.ACCOUNT, F.CUSTOMER.ACCOUNT)
    EB.DataAccess.Opf(FN.BILL.DETAILS, F.BILL.DETAILS)
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.ACCT.DETAILS, F.ACCT.DETAILS)
    EB.DataAccess.Opf(FN.AA.ARR.ACCT, F.AA.ARR.ACCT)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.LIM,F.LIM)
    EB.DataAccess.Opf(FN.COM,F.COM)
    EB.DataAccess.Opf(FN.AA.PRODUCT,F.AA.PRODUCT)

RETURN

PROCESS:
    
*---------------------------------------------------------------------
    Y.MNEMONIC = FN.AA.ARRANGEMENT[2,3]

    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.OD.PROPER = "INTONOD"
        Y.INT.PROPER = "PRINCIPALINT"
        Y.CR.ACC = 'CURACCOUNT'
        Y.PR.INT = 'ACCPRINCIPALINT'
        Y.DUE.PR.INT = 'DUEPRINCIPALINT'
        Y.DUE.ACC = "DUEACCOUNT"
*MUST INCLUDE IN PRINCIPAL AMT--- DUEACCOUNT****
        Y.PD.BAL.TYPE.ALL = 'DELACCOUNT':@VM:'DOFACCOUNT':@VM:'GRCACCOUNT':@VM:'NABACCOUNT':@VM:'SMAACCOUNT':@VM:'STDACCOUNT':@VM:'SUBACCOUNT'
        Y.PD.PFT.ALL = 'ACCINTONOD':@VM:'DELPRINCIPALINT':@VM:'DOFPRINCIPALINT':@VM:'GRCPRINCIPALINT':@VM:'NABPRINCIPALINT':@VM:'SMAPRINCIPALINT':@VM:'STDPRINCIPALINT':@VM:'SUBPRINCIPALINT'
    
        IF Y.CUS.ID NE '' AND Y.DATE.FROM.ID EQ '' AND Y.DATE.TO.ID EQ '' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND PRODUCT.LINE EQ 'LENDING' 'ACCOUNTS' AND CO.CODE EQ ":Y.COMP
        END

        IF Y.CUS.ID NE '' AND Y.DATE.FROM.ID NE '' AND Y.DATE.TO.ID EQ '' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND START.DATE GE ":Y.DATE.FROM.ID:" AND PRODUCT.LINE EQ 'LENDING' 'ACCOUNTS' AND CO.CODE EQ ":Y.COMP
        END
        IF Y.CUS.ID NE '' AND Y.DATE.FROM.ID NE '' AND Y.DATE.TO.ID NE '' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND START.DATE GE ":Y.DATE.FROM.ID:" AND START.DATE LE ":Y.DATE.TO.ID:" AND PRODUCT.LINE EQ 'LENDING' 'ACCOUNTS' AND CO.CODE EQ ":Y.COMP
        END
    END
     
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.OD.PROPER = "PFTONOD"
        Y.INT.PROPER = "DEFERREDPFT"
        Y.CR.ACC = 'CURISACCOUNT'
        Y.PR.INT = ''
        Y.DUE.PR.INT = 'DUEDEFERREDPFT'
* Y.MRK.PR.INT = 'RECMARKUPPFT'
        Y.DUE.MRK.PR.INT = 'DUEMARKUPPFT'
        Y.DUE.ACC = "DUEISACCOUNT"
*MUST INCLUDE IN PRINCIPAL AMT--- DUEISACCOUNT****
        Y.PD.BAL.TYPE.ALL = 'DELISACCOUNT':@VM:'DOFISACCOUNT':@VM:'GRCISACCOUNT':@VM:'NABISACCOUNT':@VM:'SMAISACCOUNT':@VM:'STDISACCOUNT':@VM:'SUBISACCOUNT'
        Y.MARK.DEF.PFT.ALL = 'ACCDEFERREDPFT':@VM:'DELDEFERREDPFT':@VM:'DOFDEFERREDPFT':@VM:'GRCDEFERREDPFT':@VM:'NABDEFERREDPFT':@VM:'SMADEFERREDPFT':@VM:'STDDEFERREDPFT':@VM:'SUBDEFERREDPFT':@VM:'RECMARKUPPFT':@VM:'STDMARKUPPFT':@VM:'DELMARKUPPFT':@VM:'DOFMARKUPPFT':@VM:'GRCMARKUPPFT':@VM:'NABMARKUPPFT':@VM:'SMAMARKUPPFT':@VM:'SUBMARKUPPFT'
        Y.PENAL.PFT.ALL = 'ACCPENALTYPFT'
*-----------------Pft on OD IS ONLY ACCPFTONOD--------------------------****
        Y.PFT.OD.BAL.TYPE = 'ACCPFTONOD'
*---------------------MARKUP/DEFERRED PFT ALL BAL TYPE----------------------***
        Y.PD.PFT.ALL = Y.MARK.DEF.PFT.ALL
*---------------------------------------------------------------------------------------

        IF Y.CUS.ID NE '' AND Y.DATE.FROM.ID EQ '' AND Y.DATE.TO.ID EQ '' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND PRODUCT.LINE EQ 'LENDING' AND CO.CODE EQ ":Y.COMP
        END

        IF Y.CUS.ID NE '' AND Y.DATE.FROM.ID NE '' AND Y.DATE.TO.ID EQ '' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND START.DATE GE ":Y.DATE.FROM.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMP
        END
        IF Y.CUS.ID NE '' AND Y.DATE.FROM.ID NE '' AND Y.DATE.TO.ID NE '' THEN
            SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND START.DATE GE ":Y.DATE.FROM.ID:" AND START.DATE LE ":Y.DATE.TO.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMP
        END
    END
*----------------------------Main Process--------------------------------------------------
    EB.DataAccess.Readlist(SEL.CMD.ARR, SEL.LIST,"",NO.OF.RECORD,RET.CODE)
    
    LOOP
        REMOVE Y.ARR.ID FROM SEL.LIST SETTING POS
    WHILE Y.ARR.ID:POS
*Loan ID
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.ARR, F.AA.ARRANGEMENT, ERR.ARR)
        Y.ACC.NUM = REC.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
        Y.PROD.LINE = REC.ARR<AA.Framework.Arrangement.ArrProductLine>
        Y.ARR.ST.DT = REC.ARR<AA.Framework.Arrangement.ArrStartDate>
        IF Y.CUS.ID EQ '' THEN
            Y.CUS.ID = REC.ARR<AA.Framework.Arrangement.ArrCustomer>
        END
*---------------------------------------------------------------------------------------
*Customer Name
    
        EB.DataAccess.FRead(FN.CUS, Y.CUS.ID, R.CUS, F.CUS, CUS.ERR)
        Y.CUS.TITLE = R.CUS<ST.Customer.Customer.EbCusShortName>
    
*---------------------------------------------------------------------------------------
*Limit Ref.

*-*Limit Ref. UPDATE***--------------------------NEW ADDITION 25TH AUG 2021-----------------------------------
        Y.PROP.CLASS = 'LIMIT'
        AA.Framework.GetArrangementConditions(Y.ARR.ID,Y.PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
        R.REC.LIM = RAISE(RETURN.VALUES)

        Y.LIMIT.REF = R.REC.LIM<AA.Limit.Limit.LimLimitReference>
        Y.LIMIT.SER = R.REC.LIM<AA.Limit.Limit.LimLimitSerial>
    
        Y.LIMIT = Y.LIMIT.REF:".":Y.LIMIT.SER
*---------------------------------------------------------------------------------------
*Loan Type
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, R.ARR, F.AA.ARRANGEMENT, LN.ERR.ARR)
        Y.LN.TYP = R.ARR<AA.Framework.Arrangement.ArrProduct>
    
        EB.DataAccess.FRead(FN.AA.PRODUCT, Y.LN.TYP, PRD.REC, F.AA.PRODUCT, PRD.ERR)
        Y.PRD.DES = PRD.REC<AA.ProductManagement.Product.PdtDescription>
*---------------------------------------------------------------------------------------

*Sanction Date
        Y.LN.ID = Y.CUS.ID:".":"000":Y.LIMIT
        EB.DataAccess.FRead(FN.LIM, Y.LN.ID, R.ARGMNT, F.LIM, LIM.ERR)
        
*----------------------SANC DATE FOR ISL---Y.SANC.DATE--Y.ARR.ST.DT--------
        Y.SANC.DATE = Y.ARR.ST.DT
*Limit Amount
        Y.LIMIT.AMT = R.ARGMNT<LI.Config.Limit.InternalAmount>
*----------------------------------------------------------------------------------------
*
*OD Rate

        PROP.CLASS = 'INTEREST'
        PROPERTY = Y.OD.PROPER
        AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
        R.REC = RAISE(RETURN.VALUES)
        Y.OD.INT.RATE = R.REC<AA.Interest.Interest.IntEffectiveRate>

*----------------------------------------------------------------------------------------------
*Regular PFT. Rate
 
        PROPERTY.P = Y.INT.PROPER
        AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY.P,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
        PR.REC = RAISE(RETURN.VALUES)
        Y.PR.INT.RATE = PR.REC<AA.Interest.Interest.IntEffectiveRate>
*-----------------------------------------------------------------------------------------------
*OD Outstanding
    
        EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.ARR.ID, REC.ACCT.DET, F.ACCT.DETAILS, ERR.ACCT.DET)
        Y.TOT.BILL.ID = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBillId>
        Y.TOT.BL.STATUS = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBillStatus>
        Y.TOT.PAYMT.TYP = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdPayMethod>
        Y.EXPIRY.DATE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
 
*Regular Outstanding
        Y.CUR.ACC = Y.CR.ACC
        Y.TODAY=EB.SystemTables.getToday()
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.ACC, Y.TODAY, TOT.CUR.AMT, RetError)
        Y.CUR.BAL = TOT.CUR.AMT
*------------------DUEACCOUNT/DUEISACCOUNT MUST BE CONSIDERED IN PR AMT-------------------
        Y.CUR.DUE.ACC = Y.DUE.ACC
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.CUR.DUE.ACC, Y.TODAY, TOT.DUE.AMT, RetError.2)
        Y.CUR.DUE.BAL = TOT.DUE.AMT
    
*--------------------------PFT ON OD ----------------------------------
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PFT.OD.BAL.TYPE, Y.TODAY, Y.PFT.OD.AMT, RetError4)
        Y.PFT.OD = Y.PFT.OD.AMT
       
*-------------------------------------Y.PD.PFT.ALL--------Y.MARK.DEF.PFT.ALL--------------------------------------

        Y.TOT.PD.PFT.DCOUNT = DCOUNT(Y.PD.PFT.ALL,@VM)
    
        FOR I=1 TO Y.TOT.PD.PFT.DCOUNT
            Y.PD.PFT = Y.PD.PFT.ALL<1,I>
            AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PD.PFT, Y.TODAY, TOT.PD.PFT, RetError2)
            Y.PD.PFT.AMT = TOT.PD.PFT
            Y.TOT.PD.PFT.AMT = Y.TOT.PD.PFT.AMT + Y.PD.PFT.AMT
            Y.PD.PFT = ''
            Y.PD.PFT.AMT = ''
        NEXT I
        
*--------------------------------PD BALANCE-----Y.PD.BAL.TYPE----------------------------------------------------
        Y.TOT.PD.BAL.TYPE.DCOUNT = DCOUNT(Y.PD.BAL.TYPE.ALL,@VM)

        FOR I=1 TO Y.TOT.PD.BAL.TYPE.DCOUNT
            Y.PD.BAL.TYPE = Y.PD.BAL.TYPE.ALL<1,I>
            AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PD.BAL.TYPE, Y.TODAY, TOT.PD.BAL.AMT, RetError3)
            Y.PD.AMT = TOT.PD.BAL.AMT
            Y.TOT.PD.BAL.AMT = Y.TOT.PD.BAL.AMT + Y.PD.AMT
            Y.PD.BAL.TYPE = ''
            Y.PD.AMT = ''
        NEXT I
    
*--------------DUE PRINCIPAL PROFIT (SINGLE DAY ONLY)--------------------

        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.DUE.PR.INT, Y.TODAY, DUE.PFT.AMT, RetErr)
        Y.DUE.PFT.AMT = DUE.PFT.AMT

*------------FOR DUE MARKUP PFT -------------------------------------
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.DUE.MRK.PR.INT, Y.TODAY, DUE.MRK.PFT.AMT, RetEr)
        Y.DUE.MRK.PFT.AMT = DUE.MRK.PFT.AMT
       
*----------------------------PENALTY PFT ALL-----------------------------

        Y.PENAL.PFT = Y.PENAL.PFT.ALL
        AA.Framework.GetEcbBalanceAmount(Y.ACC.NUM, Y.PENAL.PFT, Y.TODAY, TOT.PENAL.PFT.AMT, RetError5)
        Y.PENAL.PFT.AMT = TOT.PENAL.PFT.AMT
        Y.TOT.PENAL.PFT.AMT = Y.TOT.PENAL.PFT.AMT + Y.PENAL.PFT.AMT

*-----------------OVERDUE/BILLS ONLY------------------------------
        Y.OD.PR.AMT.ALL = Y.TOT.PD.BAL.AMT
        Y.OD.PR.AMT.ALL = Y.OD.PR.AMT.ALL * -1
*------------------------------------------------------------------
        Y.CUR.DUE.BAL = Y.CUR.DUE.BAL * -1
        Y.PR.AMT = Y.CUR.BAL + Y.CUR.DUE.BAL
        
        Y.PR.AMT = Y.PR.AMT * -1
*------------------------------------------------------------------------
*        Total Outstanding
        Y.TOTAL.OUT = Y.PR.AMT + Y.OD.PR.AMT.ALL
*-----------------------------------------------------------------------------------------------------
*Company Name
        Y.COM.CODE = R.REC.LIM<AA.Limit.Limit.LimCoCode>
        EB.DataAccess.FRead(FN.COM, Y.COM.CODE, R.COM, F.COM, C.ERR)
        Y.COM.NAME = R.COM<ST.CompanyCreation.Company.EbComCompanyName>
*--------------------------------------LIMIT STATUS(LENDING)------------------------------------------------------------
        EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.ARR.ID, REC.ACCT.DET, F.ACCT.DETAILS, ERR.ACCT.DET)
        Y.STAT = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdAllAgeStatus>
        
        IF Y.STAT EQ "" THEN
            Y.STAT="STD"
        END
    
*        ELSE
*            Y.STAT = Y.STAT
*        END
    
        IF Y.OD.INT.RATE EQ '' OR Y.OD.INT.RATE EQ 0 THEN
            Y.OD.INT.RATE = Y.PR.INT.RATE
        END
    
*---------------------------------------------------------------------------------------------
        IF Y.MNEMONIC EQ 'BNK' THEN
            Y.DATA<-1>=Y.CUS.ID:'*':Y.ARR.ID:'*':Y.CUS.TITLE:'*':Y.LIMIT:'*':Y.PRD.DES:'*':Y.SANC.DATE:'*':Y.EXPIRY.DATE:'*':Y.LIMIT.AMT:'*':Y.PR.INT.RATE:'*':Y.OD.INT.RATE:'*':Y.PR.AMT:'*':Y.OD.PR.AMT.ALL:'*':Y.TOTAL.OUT:'*':Y.STAT:'*':Y.COM.NAME
*                         1            2             3              4            5              6                7                 8              9                10                11                 12               13            14          15
        END
        IF Y.MNEMONIC EQ 'ISL' THEN
            Y.DATA<-1>=Y.SANC.DATE:'*':Y.ARR.ID:'*':Y.CUS.TITLE:'*':Y.LIMIT:'*':Y.PRD.DES:'*':Y.CUS.ID:'*':Y.EXPIRY.DATE:'*':Y.LIMIT.AMT:'*':Y.PR.INT.RATE:'*':Y.OD.INT.RATE:'*':Y.PR.AMT:'*':Y.OD.PR.AMT.ALL:'*':Y.TOTAL.OUT:'*':Y.STAT:'*':Y.COM.NAME
*                         1               2             3              4            5             6              7                 8              9                10                11               12               13            14          15
        END
       
        Y.LN.LC.NUM = ''
        Y.LIMIT = ''
        Y.PRD.DES = ''
        Y.SANC.DATE= ''
        Y.EXPIRY.DATE= ''
        Y.LIMIT.AMT= ''
        Y.PR.INT.RATE = ''
        Y.OD.INT.RATE = ''
        Y.PR.AMT = ''
        Y.TOT.PD.BAL.AMT = ''
        Y.OD.PR.AMT.ALL = ''
        Y.TOTAL.OUT= ''
        Y.STAT= ''
        Y.COM.NAME= ''
        
    REPEAT
    
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.DATA = SORT(Y.DATA)
    END
        
RETURN
END