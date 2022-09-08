* @ValidationCode : MjoxNzc2OTU0NjE0OkNwMTI1MjoxNjAyMDY5MzE1NDQ3OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 07 Oct 2020 17:15:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CR.MBL.E.NOF.LN.ACCRUALS(Y.RETURN)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 07TH OCT
*-----------------------------------------------------------------------------
    $INSERT  I_COMMON
    $INSERT  I_EQUATE

    $USING EB.Reports
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING AA.Framework
    $USING AA.Interest
    $USING AA.PaymentSchedule
    $USING ST.Config
    $USING AC.AccountOpening
    $USING ST.Customer
 
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
**-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
    LOCATE "ARR.ID" IN EB.Reports.getEnqSelection()<2,1> SETTING ARR.POS THEN
        Y.ARR.ID=EB.Reports.getEnqSelection()<4,ARR.POS>
    END
*
    FN.AA.INT.ACC='F.AA.INTEREST.ACCRUALS'
    F.AA.INT.ACC=''
    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    
    FN.ACC.DETAILS ='F.AA.ACCOUNT.DETAILS'
    F.ACC.DETAILS =''
    
    FN.CATEGORY="F.CATEGORY"
    F.CATEGORY=''
    
    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT=""
    
    FN.CUSTOMER ="F.CUSTOMER"
    F.CUSTOMER=''

RETURN
**-----------------------------------------------------------------------------
OPENFILES:
*-----------------------------------------------------------------------------
    EB.DataAccess.Opf(FN.AA.INT.ACC,F.AA.INT.ACC)
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.ACC.DETAILS,F.ACC.DETAILS)
    EB.DataAccess.Opf(FN.CATEGORY,F.CATEGORY)
    EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)
    EB.DataAccess.Opf(FN.CUSTOMER,F.CUSTOMER)
RETURN
**-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    Y.MNEMONIC = FN.AA.INT.ACC[2,3]
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.CUR.ACC = 'PRINCIPALINT'
    END
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.CUR.ACC = 'DEFERREDPFT'
    END
*
    
    EB.DataAccess.FRead(FN.ACC.DETAILS,Y.ARR.ID,REC.ACC.DET,F.ACC.DETAILS,ERR.ACC.DET)
*------------------------------VALUE DATE & MATURITY DATE-----------------------------------------------
    Y.VAL.DATE=REC.ACC.DET<AA.PaymentSchedule.AccountDetails.AdValueDate>
    Y.MAT.DATE = REC.ACC.DET<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
*-----------------------------------------------------------------------------


    EB.DataAccess.FRead(FN.AA.ARRANGEMENT,Y.ARR.ID,REC.AA.ARR,F.AA.ARRANGEMENT,ERR.AA.ARR)
*----------------------------CURRENCY & ARR STATUS-------------------------------------------------
    Y.CURRENCY= REC.AA.ARR<AA.Framework.Arrangement.ArrCurrency>
    Y.ARR.STATUS = REC.AA.ARR<AA.Framework.Arrangement.ArrArrStatus>
    Y.CUSTOMER.ID = REC.AA.ARR<AA.Framework.Arrangement.ArrCustomer>
    Y.ACCT.NUM = REC.AA.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
*-----------------------------------------------------------------------------
    EB.DataAccess.FRead(FN.ACCOUNT,Y.ACCT.NUM,REC.ACCOUNT,FN.ACCOUNT,ERR.ACCOUNT)
    Y.CATEGORY=REC.ACCOUNT<AC.AccountOpening.Account.Category>
    EB.DataAccess.FRead(FN.CATEGORY,Y.CATEGORY,REC.CATEGORY,FN.CATEGORY,ERR.CATEGORY)
    
*---------------------------------CATEGORY--------------------------------------------
    Y.CATEGORY.DESC=REC.CATEGORY<ST.Config.Category.EbCatDescription>
*-----------------------------------------------------------------------------

    EB.DataAccess.FRead(FN.CUSTOMER,Y.CUSTOMER.ID,REC.CUSTOMER,F.CUSTOMER,ERR.CUST)
*---------------------------------CUSTOMER--------------------------------------------
    Y.CUST.TITLE=REC.CUSTOMER<ST.Customer.Customer.EbCusShortName>
    
*--------------------------------INTEREST RATE--------------------------------------------
 
    PROP.CLASS = 'INTEREST'
    PROPERTY = Y.CUR.ACC
    AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    R.INT.REC = RAISE(RETURN.VALUES)
    Y.INT.RATE = R.INT.REC<AA.Interest.Interest.IntEffectiveRate>
*-------------------------------------------------------------------------
*--------------------------------AMOUNT TOT COMMITMENT--------------------------------------------
    Y.TOT.COM ='TOTCOMMITMENT'
    ReqdDate = EB.SystemTables.getToday()
    RequestType<2> = 'ALL'  ;* Unauthorised Movements required.
    RequestType<3> = 'ALL'  ;* Projected Movements requierd
    RequestType<4> = 'ECB'  ;* Balance file to be used
    RequestType<4,2> = 'END'

    
    AA.Framework.GetPeriodBalances(Y.ACCT.NUM, Y.TOT.COM, RequestType, ReqdDate, EndDate, SystemDate, BalDetails, ErrorMessage)
    TOT.OUTSTANDING = ABS(BalDetails<4>)
*-------------------------------------------------------------------------
    
    Y.ID=Y.ARR.ID:'-':Y.CUR.ACC
    EB.DataAccess.FRead(FN.AA.INT.ACC,Y.ID,REC.AA.INT.ACC,F.AA.INT.ACC,ERR.AA.INT.ACC)
    Y.ACR.AMT=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccAccrualAmt>
    Y.ACR.FROM.DATE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccFromDate>
    Y.ACR.TO.DATE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccToDate>
    Y.ACR.RATE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccRate>
    Y.ACR.DAYS=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccDays>
    Y.ACR.BALANCE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccBalance>
    Y.ACR.QUARTER.END=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccPeriodEnd>
    
    CONVERT SM TO VM IN Y.ACR.AMT
    CONVERT SM TO VM IN Y.ACR.RATE
    CONVERT SM TO VM IN Y.ACR.BALANCE
    Y.DCOUNT = DCOUNT(Y.ACR.FROM.DATE,VM)
    
    FOR X=1 TO Y.DCOUNT
        Y.FROM.DATE=Y.ACR.FROM.DATE<1,X>
        Y.TO.DATE=Y.ACR.TO.DATE<1,X>
        Y.ACCR.AMT = Y.ACR.AMT<1,X>
        Y.RATE.T = Y.ACR.RATE<1,X>
        Y.BALANCE = Y.ACR.BALANCE<1,X>
        Y.DAYS= Y.ACR.DAYS<1,X>
        
        
        GOSUB ARR.PRINT
    NEXT X
   
RETURN
**-----------------------------------------------------------------------------
ARR.PRINT:
**-----------------------------------------------------------------------------
    Y.RETURN<-1> = Y.ARR.ID:'*':Y.CURRENCY:'*':Y.VAL.DATE:'*':Y.ARR.STATUS:'*':Y.CATEGORY.DESC:'*':TOT.OUTSTANDING:'*':Y.MAT.DATE:'*':Y.CUSTOMER.ID:'*':Y.CUST.TITLE:'*':Y.INT.RATE:'*':Y.BALANCE:'*': Y.FROM.DATE:'*':Y.TO.DATE:'*':Y.DAYS:'*':Y.RATE.T:'*':Y.ACCR.AMT
*                        1            2               3               4                   5                   6               7               8               9                 10             11              12              13          14          15              16
    Y.RETURN =SORT(Y.RETURN)
    Y.ACCR.AMT = ''
    Y.RATE.T = ''
    Y.FROM.DATE=''
    Y.TO.DATE=''
    Y.DAYS=''
RETURN
END
