SUBROUTINE CR.MBL.E.NOF.LOAN.INCOME(ARRAY)
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*                                                  CREATED BY   - KAMRUL HASAN,
*  30/09/2020                                      FDS Bangladesh Limited
*-----------------------------------------------------------------------------

    $INSERT  I_COMMON
    $INSERT  I_EQUATE

    $USING EB.Reports
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING AA.Framework
    $USING RE.ConBalanceUpdates
    $USING AA.PaymentSchedule
    $USING AA.ProductManagement
    $USING AA.Interest
    
    
    
    Y.CUR.ACC = ''
    Y.BASE.BAL = ''
    Y.INT.PROPERTY = ''
    Y.ARR.ID=''
    BaseBalance=''
    TOT.OUTSTANDING=''
    Y.TOT.OR.AMT=''
    Y.SANC.LC.AMT=''
    Y.TOT.OUTSTANDING=''
    Y.PROD.LINE=''
    Y.TOT.ACC.AMT=''
    Y.PNLTY.PFT=''
    Y.CUR.ACC.AMT=''
     
    GOSUB INTT
    GOSUB OPNFILE
    GOSUB PROCESS
RETURN

*============*
INTT:
*============*

    Y.COMPANY.ID = EB.SystemTables.getIdCompany()

    LOCATE "CUSTOMER" IN EB.Reports.getEnqSelection()<2,1> SETTING CUSTOMER.POS THEN
        Y.CUS.ID=EB.Reports.getEnqSelection()<4,CUSTOMER.POS>
    END
    LOCATE "DATE.FROM" IN EB.Reports.getEnqSelection()<2,1> SETTING DATE.FROM.POS THEN
        Y.DATE.FROM=EB.Reports.getEnqSelection()<4,DATE.FROM.POS>
    END
    
    
    
    FN.CUSTOMER.ACCOUNT = "F.CUSTOMER.ACCOUNT"
    F.CUSTOMER.ACCOUNT = ""
    
    FN.AA.ARRANGEMENT ="F.AA.ARRANGEMENT"
    F.AA.ARRANGEMENT= ""
    
    FN.ACCT.DETAILS = "F.AA.ACCOUNT.DETAILS"
    F.ACCT.DETAILS =""
    
    FN.BILL.DETAILS ="F.AA.BILL.DETAILS"
    F.BILL.DETAILS = ""
    
    FN.AA.PRODUCT = "F.AA.PRODUCT"
    F.AA.PRODUCT=""
    
    FN.AA.ARR.INTEREST = "F.AA.ARR.INTEREST"
    F.AA.ARR.INTEREST =""
    
    FN.INTEREST.ACCRUALS = "F.AA.INTEREST.ACCRUALS"
    F.INTEREST.ACCRUALS = ""

    
    
RETURN
*============*
OPNFILE:
*============*
    EB.DataAccess.Opf(FN.CUSTOMER.ACCOUNT, F.CUSTOMER.ACCOUNT)
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
    EB.DataAccess.Opf(FN.ACCT.DETAILS, F.ACCT.DETAILS)
    EB.DataAccess.Opf(FN.BILL.DETAILS, F.BILL.DETAILS)
    EB.DataAccess.Opf(FN.AA.PRODUCT, F.AA.PRODUCT)
    EB.DataAccess.Opf(FN.AA.ARR.INTEREST, F.AA.ARR.INTEREST)
    EB.DataAccess.Opf(FN.INTEREST.ACCRUALS, F.INTEREST.ACCRUALS)


RETURN
*============*
PROCESS:
*============*

    Y.MNEMONIC = FN.ACCT.DETAILS[2,3]
    
     
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.CUR.ACC = 'CURACCOUNT':VM:'DELACCOUNT':VM:'DOFACCOUNT':VM:'DUEACCOUNT':VM:'GRCACCOUNT':VM:'NABACCOUNT':VM:'SMAACCOUNT':VM:'STDACCOUNT':VM:'SUBACCOUNT'
        Y.TOT.COM ='TOTCOMMITMENT'
        Y.AVL.BAL='AVLACCOUNT'
        Y.PRNC.PRP='PRINCIPALINT'
        Y.PNLTY.PRP ='PENALTYINT'
        Y.OD.PRP='INTONOD'
        Y.PD.BAL.TYPE.ALL = 'ACCINTONOD':VM:'PRINCIPALINT':VM:'DOFPRINCIPALINT':VM:'DUEPRINCIPALINT':VM:'GRCPRINCIPALINT':VM:'NABPRINCIPALINT':VM:'SMAPRINCIPALINT':VM:'STDPRINCIPALINT':VM:'SUBPRINCIPALINT'
    END
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.CUR.ACC = 'CURISACCOUNT':VM:'DELISACCOUNT':VM:'DOFISACCOUNT':VM:'DUEISACCOUNT':VM:'GRCISACCOUNT':VM:'NABISACCOUNT':VM:'SMAISACCOUNT':VM:'STDISACCOUNT':VM:'SUBISACCOUNT'
        Y.TOT.COM ='TOTCOMMITMENT'
        Y.AVL.BAL='AVLISACCOUNT'
        Y.PRNC.PRP='DEFERREDPFT'
        Y.PNLTY.PRP='PENALTYPFT'
        Y.OD.PRP='PFTONOD'
        Y.PD.BAL.TYPE.ALL ='ACCPFTONOD':VM:'DELDEFERREDPFT':VM:'DOFDEFERREDPFT':VM:'DUEDEFERREDPFT':VM:'GRCDEFERREDPFT':VM:'NABDEFERREDPFT':VM:'SMADEFERREDPFT':VM:'STDDEFERREDPFT':VM:'SUBDEFERREDPFT':VM:'DELMARKUPPFT':VM:'DOFMARKUPPFT':VM:'DUEMARKUPPFT':VM:'GRCMARKUPPFT':VM:'NABMARKUPPFT':VM:'SMAMARKUPPFT':VM:'STDMARKUPPFT':VM:'SUBMARKUPPFT'
    END
    
    IF Y.DATE.FROM EQ '' THEN
    SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMPANY.ID
END
ELSE
    SEL.CMD.ARR = "SELECT ":FN.AA.ARRANGEMENT:" WITH CUSTOMER EQ ":Y.CUS.ID:" AND PRODUCT.LINE EQ LENDING AND CO.CODE EQ ":Y.COMPANY.ID:" AND START.DATE GE ":Y.DATE.FROM
    END
   
    EB.DataAccess.Readlist(SEL.CMD.ARR, SEL.LIST, "",NO.OF.RECORD,RET.CODE)
    
   
   
    FOR I=1 TO NO.OF.RECORD

        Y.ARR.ID = SEL.LIST<I>
  
        GOSUB ARR.PRINT
        
   
        IF Y.INT.SPRD   EQ '' THEN
            Y.INT.SPRD='0'
        END
             

        
        
*                       LD ID1   ACTIVE PROD2    START DATE3    SANC LC AMT4     LC OUTSTANDING5      PD/BILLS OUTS.6      TOTAL OUTS.7          INT RATE8     SPRD RATE9      LC INCOME10      BILLS. INCOME11     12                  13                  14                   15
        ARRAY<-1> = Y.ARR.ID:'*':Y.DESC.PROD:'*':Y.OPN.DATE:'*':Y.SANC.LC.AMT:'*':Y.LC.OUTSTANDING:'*':Y.TOT.PD.BAL.AMT:'*':Y.TOT.OUTSTANDING:'*':Y.INT.RATE:'*':Y.INT.SPRD:'*':Y.PRIN.INT.AMT:'*':Y.INT.OD.AMT:'*':Y.TOT.INCOME.AMT:'*':Y.MAT.DATE:'*':Y.CURRENT.STATUS:'*':Y.PNLTY.PFT
        
        Y.ARR.ID = ''
        Y.DESC.PROD = ''
        Y.OPN.DATE = ''
        Y.SANC.LC.AMT = ''
        Y.LC.OUTSTANDING = ''
        Y.TOT.PD.BAL.AMT = ''
        Y.TOT.OUTSTANDING = ''
        Y.INT.RATE = ''
        Y.INT.SPRD = ''
        Y.PRIN.INT.AMT = ''
        Y.INT.OD.AMT = ''
        Y.TOT.INCOME.AMT = ''
        Y.MAT.DATE = ''
        Y.CURRENT.STATUS = ''
        Y.PNLTY.PFT = ''

    NEXT I
    ARRAY = SORT(ARRAY)
RETURN

*============*
ARR.PRINT:
*============*

    EB.DataAccess.FRead(FN.AA.ARRANGEMENT, Y.ARR.ID, REC.ARR, F.AA.ARRANGEMENT, ERR.ARR)
    
   
    
    Y.PROD.LINE = REC.ARR<AA.Framework.Arrangement.ArrProductLine>
*-----------------------ARR STATUS---------------------------------------------------
    Y.CURRENT.STATUS=REC.ARR<AA.Framework.Arrangement.ArrArrStatus>
*--------------------------------------------------------------------------

    Y.LN.LC.NAT = REC.ARR<AA.Framework.Arrangement.ArrActiveProduct>
    
    EB.DataAccess.FRead(FN.AA.PRODUCT, Y.LN.LC.NAT, REC.PROD, F.AA.PRODUCT, ERR.PROD)
*------------------------------LOAN/LC NATURE--------------------------------------------
    Y.DESC.PROD = REC.PROD<AA.ProductManagement.Product.PdtDescription>
*--------------------------------LC OPENING DATE------------------------------------------
    Y.OPN.DATE  = REC.ARR<AA.Framework.Arrangement.ArrStartDate>
*--------------------------------------------------------------------------
    Y.ACCT.NUM = REC.ARR<AA.Framework.Arrangement.ArrLinkedApplId>
*-------------------LC OUTSTANDING-------------------------------------------------------
*===========================================================

    BaseBalance = Y.CUR.ACC
    Y.DCOUNT.CUR.ACC=DCOUNT(Y.CUR.ACC,VM)
    FOR Z=1 TO Y.DCOUNT.CUR.ACC
        BaseBalance = Y.CUR.ACC<1,Z>
       GOSUB BASEBALANCE.READ
        Y.LC.OUTSTANDING = Y.LC.OUTSTANDING+TOT.OUTSTANDING
        BaseBalance=''
        TOT.OUTSTANDING=''
    NEXT Z 
    
    
*--------------------------------------------------------------------------
    BaseBalance = Y.TOT.COM
    GOSUB BASEBALANCE.READ
    Y.TOT.COMMITMENT = TOT.OUTSTANDING
    TOT.OUTSTANDING=''
    
    BaseBalance = Y.AVL.BAL
    GOSUB BASEBALANCE.READ
    Y.AVAIL.AMT = TOT.OUTSTANDING
    TOT.OUTSTANDING=''
*----------------------------SANCTION LC AMOUNT----------------------------------------------
    Y.SANC.LC.AMT = Y.TOT.COMMITMENT-Y.AVAIL.AMT
*--------------------------------------------------------------------------
    
    EB.DataAccess.FRead(FN.ACCT.DETAILS, Y.ARR.ID, REC.ACCT.DET, F.ACCT.DETAILS, ERR.ACCT.DET)
    Y.TOT.BILL.ID = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdBillId>
*---------------------------------EXPIRY DATE-----------------------------------------
    Y.MAT.DATE = REC.ACCT.DET<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
*--------------------------------------------------------------------------
*--------------------------------BILLS OUTSTANDING------------------------------------------
    
    Y.TOT.PD.BAL.TYPE.DCOUNT = DCOUNT(Y.PD.BAL.TYPE.ALL,VM)
    Y.TODAY = EB.SystemTables.getToday()
    FOR X=1 TO Y.TOT.PD.BAL.TYPE.DCOUNT
        Y.PD.BAL.TYPE = Y.PD.BAL.TYPE.ALL<1,X>
        AA.Framework.GetEcbBalanceAmount(Y.ACCT.NUM,Y.PD.BAL.TYPE, Y.TODAY, TOT.PD.BAL.AMT, RetError3)
        Y.PD.AMT = ABS(TOT.PD.BAL.AMT)
        Y.TOT.PD.BAL.AMT = Y.TOT.PD.BAL.AMT + Y.PD.AMT
        Y.PD.BAL.TYPE = ''
        Y.PD.AMT = ''
    NEXT X
        
*--------------------------------------------------------------------------
    Y.TOT.OUTSTANDING = Y.LC.OUTSTANDING+Y.TOT.PD.BAL.AMT
*--------------------------------------------------------------------------
    PROP.CLASS = 'INTEREST'
    PROPERTY=Y.PRNC.PRP
    AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    R.INT.REC = RAISE(RETURN.VALUES)
*-------------------------------------------------------------------------
    Y.INT.RATE = R.INT.REC<AA.Interest.Interest.IntEffectiveRate>
*-------------------------------------------------------------------------

    PROP.CLASS = 'INTEREST'
    PROPERTY= Y.PNLTY.PRP
    AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    R.INT.SPRD = RAISE(RETURN.VALUES)
*-----------------------------INT RATE--------------------------------------------
    Y.INT.SPRD = R.INT.SPRD<AA.Interest.Interest.IntEffectiveRate>
*-------------------------------------------------------------------------
    
    Y.INT.ACCRUALS.ID=Y.ARR.ID:"-":Y.PRNC.PRP
    GOSUB INCOME.READ
*---------------PRINCIPAL INCOME----------------------------------------------------------
    Y.PRIN.INT.AMT = Y.TOT.ACC.AMT
    Y.TOT.ACC.AMT=''
        
*----------------------------PENALTY PROFIT---------------------------------------------
    Y.INT.ACCRUALS.ID=Y.ARR.ID:"-":Y.PNLTY.PRP
    GOSUB INCOME.READ
    Y.PNLTY.PFT = Y.TOT.ACC.AMT
    Y.TOT.ACC.AMT=''
*---------------------------OD INCOME--------------------------------------------------------------------------------
    Y.INT.ACCRUALS.ID = Y.ARR.ID:"-":Y.OD.PRP

    GOSUB INCOME.READ
    Y.INT.OD.AMT = Y.TOT.ACC.AMT
    Y.TOT.ACC.AMT=''
    
*------------------------------NOT NEEDED THIS AMOUNT(calculate direectly in enquiry)-------------------------------------------
    Y.TOT.INCOME.AMT = Y.PRIN.INT.AMT+Y.INT.OD.AMT
*-------------------------------------------------------------------------


RETURN
*============*
BASEBALANCE.READ:
*============*
    ReqdDate = EB.SystemTables.getToday()
    RequestType<2> = 'ALL'  ;* Unauthorised Movements required.
    RequestType<3> = 'ALL'  ;* Projected Movements requierd
    RequestType<4> = 'ECB'  ;* Balance file to be used
    RequestType<4,2> = 'END'

    
    AA.Framework.GetPeriodBalances(Y.ACCT.NUM, BaseBalance, RequestType, ReqdDate, EndDate, SystemDate, BalDetails, ErrorMessage)
    TOT.OUTSTANDING = ABS(BalDetails<4>)
    
    
RETURN
*============*
INCOME.READ:
*============*
    
    EB.DataAccess.FRead(FN.INTEREST.ACCRUALS, Y.INT.ACCRUALS.ID, REC.INT.DET, F.INTEREST.ACCRUALS, ERR.INT.DET)
    
    Y.ACC.AMT = REC.INT.DET<AA.Interest.InterestAccruals.IntAccTotPosAccrAmt>
    
    CONVERT SM TO VM IN Y.ACC.AMT
    Y.ACC.AMT.DCOUNT = DCOUNT(Y.ACC.AMT,VM)
   
    FOR K=1 TO Y.ACC.AMT.DCOUNT
        Y.ARR.ACC.AMT=Y.ACC.AMT<1,K>
        Y.TOT.ACC.AMT = Y.ARR.ACC.AMT+Y.TOT.ACC.AMT
        
    NEXT K
RETURN

END
