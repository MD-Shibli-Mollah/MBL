* @ValidationCode : MjoxNTcyODk4MDkyOkNwMTI1MjoxNjAwNjgxNDQ0NTY3OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Sep 2020 15:44:04
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

*-------------ISL PROPERTY UPDATED BY MD. SHIBLI MOLLAH-------------*

SUBROUTINE CR.MBL.ISL.ONE.DAY.PFT(arrId,arrProp,arrCcy,arrRes,balanceAmount,perDat)
*-----------------------------------------------------------------------------
*
*Subroutine Description: This routine calculates One Day Interest in Closing Time
*Subroutine Type       : Calculation
*Attached To           : AA.SOURCE.CALC.TYPE()
*Attached As           : Attached to AA.SOURCE.CALC.TYPE(), which will get the Outstanding Amount for One Day Interest
*                        and also consider Bill Amount
*Developed by          : #-MASUDUR RAHMAN-#
*Incoming Parameters   : arrId  - Arrangement ID
*                        arrProp - Arrangement Property
*                        arrCcy - Arrangement currency
*                        arrRes - Arrangement record
*                        perDat -
*Outgoing Parameters   : balanceAmount
*-----------------------------------------------------------------------------
*** <region name= Arguments>
*** <desc>To define the arguments </desc>
* Incoming Arguments:
*
* Outgoing Arguments:
*
* balanceAmount - Bonus amount calculated will be passed
*
*** </region>
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $USING EB.SystemTables
    $USING AA.Framework
    $USING AA.ProductFramework
    $USING AA.Interest
    $USING AA.ActivityCharges
    $USING EB.DataAccess
    $USING AA.Limit
    $USING LI.Config
    $USING RE.ConBalanceUpdates
    $USING AA.PaymentSchedule
*
    GOSUB INIT
    GOSUB PROCESS
*
RETURN
*-----------------------------------------------------------------------------
*-------
INIT:
*-------
    ArrangementId = ''
    accountId = ''
    requestDate = ''
    balanceAmount = ''
    retError = ''
    Y.TOT.DUE.BILL='0'
    Y.CUR.AMT=''
    Y.ONE.DAY.INT.AMT = ''
RETURN
*---------
PROCESS:
*---------
    ArrangementId =arrId
*
*
    AA.Framework.GetArrangementAccountId(ArrangementId, accountId, Currency, ReturnError) ;*ACCOUNT ID
    AA.Framework.GetArrangement(ArrangementId, RArrangement, RetError)
*
    BaseBalance = 'CURISACCOUNT'
    RequestType<2> = 'ALL'
    RequestType<3> = 'ALL'
    RequestType<4> = 'ECB'
    RequestType<4,2> = 'END'
    Y.SYSTEMDATE = EB.SystemTables.getToday()
    AA.Framework.GetPeriodBalances(accountId,BaseBalance,RequestType,Y.SYSTEMDATE,Y.SYSTEMDATE,Y.SYSTEMDATE,BalDetails,ErrorMessage) ;*Balance left in the balance Type
    Y.CUR.AMT = BalDetails<4>
    Y.NET.OUTSTAND.BAL = Y.CUR.AMT
*
*    BaseBalance.all = 'ACCPRINCIPALINT':VM:'ACCINTONOD':VM:'ACCPENALTYINT'
*    BaseBalance.All.Count = DCOUNT(BaseBalance.all,VM)
*    RequestType<2> = 'ALL'
*    RequestType<3> = 'ALL'
*    RequestType<4> = 'ECB'
*    RequestType<4,2> = 'END'
*    Y.SYSTEMDATE = EB.SystemTables.getToday()
*    FOR Y.BASE.BAL.ALL.CNT =1 TO BaseBalance.All.Count
*        AA.Framework.GetPeriodBalances(accountId,BaseBalance.all<1,Y.BASE.BAL.ALL.CNT>,RequestType,Y.SYSTEMDATE,Y.SYSTEMDATE,Y.SYSTEMDATE,BalDetails,ErrorMessage) ;*Balance left in the balance Type
*        Y.BASE.BAL.ALL = BalDetails<4>
*        Y.NET.OUTSTAND.BAL = Y.NET.OUTSTAND.BAL + Y.BASE.BAL.ALL
*    NEXT Y.BASE.BAL.ALL.CNT
*
*IF Y.CUR.AMT EQ '0' OR Y.CUR.AMT EQ '' THEN
    Y.PD.BAL.TYPE.ALL = 'DELISACCOUNT':VM:'DOFISACCOUNT':VM:'DUEISACCOUNT':VM:'GRCISACCOUNT':VM:'NABISACCOUNT':VM:'SMAISACCOUNT':VM:'STDISACCOUNT':VM:'SUBISACCOUNT'
    Y.TOT.PD.BAL.TYPE.DCOUNT = DCOUNT(Y.PD.BAL.TYPE.ALL,VM)
    FOR Y.BAL.TYP.CNT = 1 TO Y.TOT.PD.BAL.TYPE.DCOUNT
        Y.PD.BAL.TYPE = Y.PD.BAL.TYPE.ALL<1,Y.BAL.TYP.CNT>
        AA.Framework.GetEcbBalanceAmount(accountId,Y.PD.BAL.TYPE, Y.SYSTEMDATE, Y.PD.AMT, RetError3)
        IF Y.PD.BAL.TYPE EQ 'DUEISACCOUNT' THEN
            Y.NET.OUTSTAND.BAL = Y.NET.OUTSTAND.BAL + Y.PD.AMT
            Y.PD.AMT = ''
        END
        Y.TOT.PD.BAL.AMT = Y.TOT.PD.BAL.AMT + Y.PD.AMT
        Y.PD.BAL.TYPE = ''
        Y.PD.AMT = ''
    NEXT Y.BAL.TYP.CNT
*END
    Y.ARR.STATUS = RArrangement<AA.Framework.Arrangement.ArrArrStatus>
    IF Y.ARR.STATUS EQ 'EXPIRED' THEN
*
        PROP.CLASS.TRM = 'INTEREST'
        AA.Framework.GetArrangementConditions(ArrangementId,PROP.CLASS.TRM,'PFTONOD','',RETURN.IDS.TRM,RETURN.VALUES.TRM,ERR.MSG.TRM)
        RETURN.VALUES.TRM = RAISE(RETURN.VALUES.TRM)
        Y.INTONOD.RT = RETURN.VALUES.TRM<AA.Interest.Interest.IntFixedRate>
*
        PROP.CLASS.TRM = 'INTEREST'
        AA.Framework.GetArrangementConditions(ArrangementId,PROP.CLASS.TRM,'PENALTYPFT','',RETURN.IDS.TRM,RETURN.VALUES.TRM,ERR.MSG.TRM)
*
        RETURN.VALUES.TRM = RAISE(RETURN.VALUES.TRM)
        Y.PENALTYINT.RT = RETURN.VALUES.TRM<AA.Interest.Interest.IntFixedRate>
        Y.TOTAL.RATE = (Y.INTONOD.RT+Y.PENALTYINT.RT)/100
        IF Y.NET.OUTSTAND.BAL EQ '0' THEN
            Y.NET.OUTSTAND.BAL = Y.TOT.PD.BAL.AMT
        END
        Y.TOT.EXP.AMT = Y.NET.OUTSTAND.BAL*Y.TOTAL.RATE
        Y.ONE.DAY.INT.AMT = DROUND(Y.TOT.EXP.AMT/360,2)
    END
    ELSE
*        R.AA.ACCOUNT.DETAILS = AA.PaymentSchedule.AccountDetails.Read(ArrangementId, "")
*        Y.TOT.BILL.ID = R.AA.ACCOUNT.DETAILS<AA.PaymentSchedule.AccountDetails.AdBillId>
*        Y.TOT.SET.STATUS = R.AA.ACCOUNT.DETAILS<AA.PaymentSchedule.AccountDetails.AdSetStatus>
*        Y.BILL.STATUS=''
*        Y.BILL.ID = ''
*        CONVERT SM TO VM IN Y.TOT.SET.STATUS
*        CONVERT SM TO VM IN Y.TOT.BILL.ID
*        Y.BILL.CNT = DCOUNT(Y.TOT.BILL.ID,VM)
*
*        FIND 'UNPAID' IN Y.TOT.SET.STATUS SETTING Y.FM,Y.VM,Y.SM THEN
*            FOR I=1 TO Y.BILL.CNT
*                Y.BILL.ID = Y.TOT.BILL.ID<1,I>
*                Y.BILL.STATUS=Y.TOT.SET.STATUS<1,I>
**
*                IF Y.BILL.STATUS EQ 'UNPAID' THEN
*                    AA.PaymentSchedule.GetBillDetails(ArrangementId, Y.BILL.ID, BillDetails, RetError)
*                    Y.OS.PR.AMT = ''
*                    Y.TOT.OS.PR.AMT = BillDetails<AA.PaymentSchedule.BillDetails.BdOsPrAmt>
*                    CONVERT SM TO VM IN Y.TOT.OS.PR.AMT
*                    Y.OS.DCOUNT = DCOUNT(Y.TOT.OS.PR.AMT,VM)
**
*                    FOR J=1 TO Y.OS.DCOUNT
*                        Y.OS.PR.AMT = Y.TOT.OS.PR.AMT<1,J>
*                        Y.TOT.DUE.BILL = Y.TOT.DUE.BILL + Y.OS.PR.AMT
*                    NEXT J
*                END
*            NEXT I
*        END
*
        IF Y.TOT.PD.BAL.AMT NE 0 THEN
            PROP.CLASS.TRM = 'INTEREST'
            AA.Framework.GetArrangementConditions(ArrangementId,PROP.CLASS.TRM,'DEFERREDPFT','',RETURN.IDS.TRM,RETURN.VALUES.TRM,ERR.MSG.TRM)
            RETURN.VALUES.TRM = RAISE(RETURN.VALUES.TRM)
            Y.PRIN.RT = RETURN.VALUES.TRM<AA.Interest.Interest.IntFixedRate>
*
            Y.REGULAR.AMT = Y.NET.OUTSTAND.BAL*(Y.PRIN.RT/100)
            Y.REGULAR.AMT.INT = Y.REGULAR.AMT/360;
*
            PROP.CLASS.TRM = 'INTEREST'
            AA.Framework.GetArrangementConditions(ArrangementId,PROP.CLASS.TRM,'PENALTYPFT','',RETURN.IDS.TRM,RETURN.VALUES.TRM,ERR.MSG.TRM)
            RETURN.VALUES.TRM = RAISE(RETURN.VALUES.TRM)
            Y.PENALTYINT.RT = RETURN.VALUES.TRM<AA.Interest.Interest.IntFixedRate>
*
            Y.TOT.PD.BAL.AMT = Y.TOT.PD.BAL.AMT*(Y.PENALTYINT.RT/100)
            Y.TOT.PD.BAL.AMT.INT = Y.TOT.PD.BAL.AMT/360
            Y.ONE.DAY.INT.AMT = DROUND((Y.TOT.PD.BAL.AMT.INT + Y.REGULAR.AMT.INT),2)
        END
        ELSE
            PROP.CLASS.TRM = 'INTEREST'
            AA.Framework.GetArrangementConditions(ArrangementId,PROP.CLASS.TRM,'DEFERREDPFT','',RETURN.IDS.TRM,RETURN.VALUES.TRM,ERR.MSG.TRM)
            RETURN.VALUES.TRM = RAISE(RETURN.VALUES.TRM)
            Y.PRIN.RT = RETURN.VALUES.TRM<AA.Interest.Interest.IntFixedRate>
*
            Y.REGULAR.AMT = Y.NET.OUTSTAND.BAL * Y.PRIN.RT
            Y.ONE.DAY.INT.AMT = DROUND(Y.REGULAR.AMT/360,2);
        END
    END
*
    balanceAmount = Y.ONE.DAY.INT.AMT
*
RETURN
END