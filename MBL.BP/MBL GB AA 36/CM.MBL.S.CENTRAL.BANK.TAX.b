SUBROUTINE CM.MBL.S.CENTRAL.BANK.TAX( PASS.CUSTOMER, PASS.DEAL.AMOUNT, PASS.DEAL.CCY, PASS.CCY.MKT, PASS.CROSS.RATE, PASS.CROSS.CCY, PASS.DWN.CCY, PASS.DATA, PASS.CUST.CDN,R.TAX,TAX.AMOUNT)
*-----------------------------------------------------------------------------
* This routine calculate TAX amount based on TIN given or not and attached in CALC.ROUTINE field of TAX Application
* Developed By-
*1. 0%=> Indetified by Bank(Manual)
*2. 5%=> Identified by Bank(Manual)
*3. 10%> Balance LT 1,00,000 for Saving Account or ( SND/CD/SB if  TIN available)
*4. 15%=> Balance GE 1,00,000 for Saving Account and TIN ID not available( SND/SB/CD)
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*Subroutine Description:
*Subroutine Type:
*Attached To    : AA.ARR.ACCOUNT,MBL.AA.AR
*Attached As    : TAX ROUTINE AT TAX CODE
*-----------------------------------------------------------------------------
* Modification History :
* 14/06/2020 -                            Retrofit   - MD. KAMRUL HASAN,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    $USING AA.Framework
    $USING EB.API
    $USING ST.Customer
    $USING AC.AccountOpening
    $INSERT I_F.ACCOUNT
    $USING AA.Customer
    $INSERT I_F.AA.CUSTOMER
    $USING EB.DataAccess
    $USING EB.LocalReferences
    $USING EB.SystemTables
    $USING AA.Account
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
************************************
INIT:
************************************
    FN.CUS= 'F.CUSTOMER'
    F.CUS = ''
    
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT= ''
   
    APPLICATION.NAME = 'AA.ARR.ACCOUNT'
    Y.TAX.MARK = 'LT.AC.TAX.RATE'
    Y.TAX.MARK.POS =''
    
    Y.MAX.AMT.ORIG=''
    Y.MAX.AMT.TEMP=''
    Y.TIN.AMOUNT=100000
    Y.ETIN = ''
    Y.CUS.NO=''
    Y.TIN.VAL=''
    Y.END.DATE=''
    Y.END.MNTH=''
    Y.START.DATE=''
    Y.ARR.ID=''
    AccountId=''
    BaseBalance=''
    Currency=''
    ProductLine=''
    ReturnError=''
    Y.PRODUCT.LINE=''
RETURN
************************************
OPENFILES:
************************************
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    
    EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)
RETURN
************************************
PROCESS:
************************************
    Y.CUS.NO = c_aalocArrangementRec<AA.Framework.Arrangement.ArrCustomer>
    EB.DataAccess.FRead(FN.CUS,Y.CUS.NO,R.CUS,F.CUS, ERR)
    Y.TIN.VAL=R.CUS<ST.Customer.Customer.EbCusTaxId>
    
*    Y.TIN.LEN = LEN(Y.TIN.VAL)
*    Y.TIN.NUM = NUM(Y.TIN.VAL)
*
*    IF (Y.TIN.LEN EQ 12) AND Y.TIN.NUM THEN
*        Y.ETIN = 'Y'
*    END

    Y.END.DATE = EB.SystemTables.getToday()
    Y.END.MNTH = Y.END.DATE[5,2] 'R%2'
    IF Y.END.MNTH LE '06' THEN
        Y.START.DATE = Y.END.DATE[1,4]:'0101'
    END ELSE
        Y.START.DATE = Y.END.DATE[1,4]:'0701'
    END
     
    Y.ARR.ID = PASS.CUSTOMER<5>

    AA.Framework.GetArrangementAccountId(Y.ARR.ID, AccountId, Currency, ReturnError)
    AA.Framework.GetArrAccountProductLine(AccountId, ProductLine, ReturnError)
    Y.PRODUCT.LINE = ProductLine
    BaseBalance = 'CURBALANCE'
    RequestType<2> = 'ALL'  ;* Unauthorised Movements required.
    RequestType<3> = 'ALL'  ;* Projected Movements requierd
    RequestType<4> = 'ECB'  ;* Balance file to be used
    RequestType<4,2> = 'END'    ;* Balance required as on TODAY - though Activity date can be less than today
    AA.Framework.GetPeriodBalances(AccountId , BaseBalance, RequestType, Y.START.DATE, Y.END.DATE, SystemDate, BalDetails, ErrorMessage)
    Y.MAX.AMT.ORIG = MAXIMUM(BalDetails<4,2>)
    IF Y.MAX.AMT.ORIG EQ '' THEN
        Y.MAX.AMT.ORIG = ABS(MAXIMUM(BalDetails<4>))
    END
    

***********************************************************
    
    EB.LocalReferences.GetLocRef(APPLICATION.NAME,Y.TAX.MARK,Y.TAX.MARK.POS)
    PROP.CLASS2 = 'ACCOUNT'
    AA.Framework.GetArrangementConditions(Y.ARR.ID, PROP.CLASS2, PROPERTY, Effectivedate, RETURN.IDS, RETURN.VALUES, ERR.MSG)
    R.ACC.REC = RAISE(RETURN.VALUES)
    Y.TAX.RATE = R.ACC.REC<AA.Account.Account.AcLocalRef,Y.TAX.MARK.POS>
    
    IF Y.TAX.RATE EQ 'ZERO' THEN
        Y.TAX.RATE = 0
    END
    IF Y.TAX.RATE EQ 'FIVE' THEN
        Y.TAX.RATE = 5
    END
    
    Y.Activity = AA.Framework.getC_aaloccurractivity()
    IF Y.TAX.RATE EQ '' THEN
*IF Y.ETIN EQ '' THEN
        IF Y.TIN.VAL THEN
            TAX.AMOUNT=(PASS.DEAL.AMOUNT*10)/100
            Case1 = '1'
        END
        ELSE
            IF Y.MAX.AMT.ORIG GE Y.TIN.AMOUNT THEN
                TAX.AMOUNT=(PASS.DEAL.AMOUNT*15)/100
                Case1 = '2'
            END
            ELSE
                TAX.AMOUNT=(PASS.DEAL.AMOUNT*10)/100
                Case1 = '3'
            END
        END
    END ELSE
        TAX.AMOUNT = (PASS.DEAL.AMOUNT*Y.TAX.RATE)/100
        Case1 = '4'
    END

    Y.TIME = EB.SystemTables.getTimeStamp()


RETURN
END
