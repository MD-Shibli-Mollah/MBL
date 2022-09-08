* @ValidationCode : MjoxMDAyMjIzMDQ2OkNwMTI1MjoxNjA2MTk3ODIxNDE2OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 24 Nov 2020 12:03:41
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CR.MBL.E.NOF.LOAN.LIST.PRODUCT(Y.RETURN)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 29TH SEP
*-----------------------------------------------------------------------------

    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    
    $USING EB.Reports
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING AA.Framework
    $USING RE.ConBalanceUpdates
    $USING AA.Limit
    $USING AA.Account
    $USING ST.Customer
    $USING LI.Config
    $USING AA.Interest
    $USING AA.PaymentSchedule
    $USING AC.AccountOpening
    $USING AA.ProductManagement
    
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
RETURN
 
INIT:

    Y.COMP = EB.SystemTables.getIdCompany()
    
    LOCATE "CUSTOMER" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
        Y.CUS.ID=EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
    END
    LOCATE "PRODUCT" IN EB.Reports.getEnqSelection()<2,1> SETTING PRODUCT.POS THEN
        Y.PRD.ID=EB.Reports.getEnqSelection()<4,PRODUCT.POS>
    END
    LOCATE "VALUE.DATE" IN EB.Reports.getEnqSelection()<2,1> SETTING VALUE.DATE.POS THEN
        Y.DATE.ID=EB.Reports.getEnqSelection()<4,VALUE.DATE.POS>
    END
    
    FN.CUS = "F.CUSTOMER"
    F.CUS = ""
*
    FN.AA.ARRANGEMENT = "F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT= ""
*
    FN.LIMIT= "F.AA.ARR.LIMIT"
    F.LIMIT= ""
*
    FN.AA.ARR.ACCT = "F.AA.ARR.ACCOUNT"
    F.AA.ARR.ACCT = ""
*
    FN.ACCT.DETAILS = "F.AA.ACCOUNT.DETAILS"
    F.ACCT.DETAILS =""
*
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
*
    FN.AA.PRODUCT = "F.AA.PRODUCT"
    F.AA.PRODUCT=""
 
    Y.CUR.ACC=''
RETURN

OPENFILE:
*
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.ACCT.DETAILS, F.ACCT.DETAILS)
    EB.DataAccess.Opf(FN.AA.ARR.ACCT, F.AA.ARR.ACCT)
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.ACC,F.ACC)
    EB.DataAccess.Opf(FN.AA.PRODUCT, F.AA.PRODUCT)
*
RETURN

PROCESS:
*
*---------------------------------------------------------------------
    Y.MNEMONIC = FN.ACCT.DETAILS[2,3]
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.CUR.ACC = 'CURACCOUNT'
    END
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.CUR.ACC = 'CURISACCOUNT'
    END
*------------------------AMENDMENT FOR PRODUCT NAME for ***ISL*** SELECTION FROM USER----------------------------
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.PRD.NAME = Y.PRD.ID
        SEL.CMD.PRD = "SELECT ":FN.AA.PRODUCT:" WITH DESCRIPTION EQ ":Y.PRD.NAME
        
        EB.DataAccess.Readlist(SEL.CMD.PRD, SEL.LIST.P, "", NO.OF.REC.PRD, RET.CODE.P)

        Y.PRD.ID = SEL.LIST.P<1>
    END
*---------------------------------------------------------------------------------------
    IF Y.CUS.ID EQ '' AND Y.PRD.ID EQ '' AND Y.DATE.ID EQ '' THEN
        SEL.CMD.ARR="SELECT ":FN.AA.ARRANGEMENT:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMP
    END
    IF Y.CUS.ID NE '' AND Y.PRD.ID EQ '' AND Y.DATE.ID EQ '' THEN
        SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMP
    END
    IF Y.CUS.ID EQ '' AND Y.PRD.ID NE '' AND Y.DATE.ID EQ '' THEN
        SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH ACTIVE.PRODUCT EQ ":Y.PRD.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMP
    END
    IF Y.CUS.ID NE '' AND Y.PRD.ID NE '' AND Y.DATE.ID EQ '' THEN
        SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND ACTIVE.PRODUCT EQ ":Y.PRD.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMP
    END
    IF Y.CUS.ID NE '' AND Y.PRD.ID NE '' AND Y.DATE.ID NE '' THEN
        SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND ACTIVE.PRODUCT EQ ":Y.PRD.ID:" AND PROD.EFF.DATE EQ ":Y.DATE.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMP
    END
    
    EB.DataAccess.Readlist(SEL.CMD.ARR, SEL.LIST, "",NO.OF.RECORD,RET.CODE)
*
    Y.ARR.DCOUNT = DCOUNT(SEL.LIST,VM)
    
    FOR I=1 TO NO.OF.RECORD
        Y.ARR.ID = SEL.LIST<I>
    
        EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.ARR, F.AA.ARRANGEMENT, ERR.ARR)
        Y.LN.LC.ID = Y.ARR.ID
  
        Y.LN.LC.NAT = REC.ARR<AA.Framework.Arrangement.ArrActiveProduct>
        EB.DataAccess.FRead(FN.AA.PRODUCT, Y.LN.LC.NAT, REC.PROD, F.AA.PRODUCT, ERR.PROD)
        Y.DESC.PROD = REC.PROD<AA.ProductManagement.Product.PdtDescription>
*
        Y.CUST = REC.ARR<AA.Framework.Arrangement.ArrCustomer>
        CONVERT VM TO FM IN Y.CUST
        EB.DataAccess.FRead(FN.CUS, Y.CUST, R.AR, F.CUS, CUS.ERR)
        Y.CUS.TITTL=R.AR<ST.Customer.Customer.EbCusNameOne>
*
        SEL.LIMIT.CMD = "SELECT ":FN.LIMIT:" WITH @ID LIKE ":Y.ARR.ID:"-":"LIMIT-..."
        EB.DataAccess.Readlist(SEL.LIMIT.CMD, S.LIST,"",NO.OF.REC,R.CODE)
        Y.LN.ID = S.LIST<1,NO.OF.REC>
        EB.DataAccess.FRead(FN.LIMIT, Y.LN.ID, R.ARRNGMNT, F.LIMIT, L.ERR)
        Y.LIMIT.REF= R.ARRNGMNT<AA.Limit.Limit.LimLimitReference>
        Y.LIMIT.SER= R.ARRNGMNT<AA.Limit.Limit.LimLimitSerial>
        Y.LIMIT=Y.LIMIT.REF:".":Y.LIMIT.SER
*
        EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.ARR.ID, REC.ACCT.DET,F.ACCT.DETAILS, ERR.ACCT.DET)
        Y.VALUE.DATE=REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdValueDate>
        Y.MAT.DATE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
*
*------------------------------------------------------------------------
        BaseBalance = Y.CUR.ACC
        ReqdDate = EB.SystemTables.getToday()
        RequestType<2> = 'ALL' ;* Unauthorised Movements required.
        RequestType<3> = 'ALL' ;* Projected Movements requierd
        RequestType<4> = 'ECB' ;* Balance file to be used
        RequestType<4,2> = 'END'

        Y.ACCT.NUM = REC.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
        EB.DataAccess.FRead(FN.ACC, Y.ACCT.NUM, REC.ACC, F.ACC, ERR.ACC)
        Y.CUS.CURR=REC.ACC<AC.AccountOpening.Account.Currency>

        AA.Framework.GetPeriodBalances(Y.ACCT.NUM, BaseBalance, RequestType, ReqdDate, EndDate, SystemDate, BalDetails, ErrorMessage)
        Y.OUTSTANDING.AMT = ABS(BalDetails<4>)
*------------------------Y.OUTSTANDING.AMT---BALANCE FOR MARKUP ISL ----Y.ACCT.NUM = '3723000000386'-------------------------------------------
        IF Y.MNEMONIC EQ 'ISL' THEN
            Y.TODAY = EB.SystemTables.getToday()

            Y.BAL.TYPE.ALL = 'CURISACCOUNT':VM:'NABISACCOUNT':VM:'DUEDEFERREDPFT':VM:'RECMARKUPPFT':VM:'DUEMARKUPPFT':VM:'STDMARKUPPFT':VM:'DUEISACCOUNT':VM:'STDDEFERREDPFT':VM:'STDISACCOUNT':VM:'DUEEXCISEDUTYFEE':VM:'STDMARKUPPFTSP':VM:'ACCPRINCIPALINTSP':VM:'ACCPFTONODSP':VM:'ACCPENALTYINTSP':VM:'STDDEFERREDPFTSP':VM:'STDPFTONODSP':VM:'STDPENALTYPFTSP':VM:'ACCPFTONOD':VM:'ACCPENALTYINT':VM:'STDPFTONOD':VM:'STDPENALTYPFT':VM:'NABDEFERREDPFT':VM:'NABMARKUPPFT':VM:'NABPENALTYPFT':VM:'NABPFTONOD':VM:'NABDEFERREDPFTSP':VM:'NABMARKUPPFTSP':VM:'NABPENALTYPFTSP':VM:'NABPFTONODSP':VM:'NABREBATEFEE':VM:'NABISBONUS':VM:'NABEXCISEDUTYFEE':VM:'ACCPENALTYPFT'
        
            Y.BAL.TYPE.ALL.DCOUNT = DCOUNT(Y.BAL.TYPE.ALL,VM)

            FOR J=1 TO Y.BAL.TYPE.ALL.DCOUNT
                Y.BAL.TYPE = Y.BAL.TYPE.ALL<1,J>
                AA.Framework.GetEcbBalanceAmount(Y.ACCT.NUM,Y.BAL.TYPE, Y.TODAY, TOT.BAL.AMT, RetError3)
                Y.AMT = TOT.BAL.AMT
                Y.OUTSTANDING.AMT = Y.OUTSTANDING.AMT + Y.AMT
                Y.BAL.TYPE = ''
                Y.AMT = ''
            NEXT J
        END
        Y.OUTSTANDING.AMT = ABS(Y.OUTSTANDING.AMT)
*----------------------------END------------------------------------------------

*
        Y.RETURN<-1> = Y.LN.LC.ID:"*":Y.CUS.TITTL:"*":Y.LIMIT:"*":Y.CUS.CURR:"*":Y.DESC.PROD:"*":Y.OUTSTANDING.AMT:"*":Y.VALUE.DATE:"*":Y.MAT.DATE
    
        Y.LN.LC.ID = ''
        Y.LIMIT = ''
        Y.CUS.CURR = ''
        Y.CUS.PRODUCT = ''
        Y.PRD.DES = ''
        Y.OUTSTANDING.AMT = ''
        Y.VALUE.DATE = ''
        Y.MAT.DATE = ''
    NEXT I
*
RETURN

END