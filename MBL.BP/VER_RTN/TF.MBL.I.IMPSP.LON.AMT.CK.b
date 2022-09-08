SUBROUTINE TF.MBL.I.IMPSP.LON.AMT.CK
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
* 11/24/2020 -                            Creator   - MAHMUDUR RAHMAN (UDOY),
*                                                 FDS Bangladesh Limited
* VERSION : DRAWINGS,MBL.IMPSP

    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $USING EB.SystemTables
    $USING LC.Contract
    $USING EB.DataAccess
    $USING AA.Framework
    $USING EB.LocalReferences
    $USING AA.Account
    $USING ST.CurrencyConfig
    $USING AC.AccountOpening
    $USING EB.ErrorProcessing
    
*
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
*
RETURN
*-----------------------------------------------------------------------------
*-------
INIT:
*-------

    FN.DRW = 'F.DRAWINGS'
    F.DRW = ''
*
    FN.ACCT = 'F.ACCOUNT'
    F.ACCT = ''
    Y.DR.ACCT=''
    REC.ACCT.ID=''
    
    FN.CURR = 'F.CURRENCY'
    F.CURR = ''
    
    FN.ARR = 'F.AA.ARRANGEMENT'
    F.ARR = ''
    
RETURN
*---------
OPENFILES:
*---------

    EB.DataAccess.Opf(FN.DRW,F.DRW)
    EB.DataAccess.Opf(FN.ACCT, F.ACCT)
    EB.DataAccess.Opf(FN.CURR, F.CURR)
    EB.DataAccess.Opf(FN.ARR, F.ARR)
RETURN
*---------
PROCESS:
    Y.CHEK.AMT = '0'
    Y.DR.CURR = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrDrawCurrency)
    Y.DR.ACCT = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrDrawdownAccount)
    
    EB.DataAccess.FRead(FN.ACCT,Y.DR.ACCT, REC.ACCT.ID, F.ACCT.ID, ERR.ACCT.ID)
    Y.AA.ID = REC.ACCT.ID<AC.AccountOpening.Account.ArrangementId>
    EB.DataAccess.FRead(FN.ARR, Y.AA.ID, ARR.REC, F.ARR, ARR.ERR)
    Y.PROD.GP = ARR.REC<AA.Framework.Arrangement.ArrProductGroup>
    IF Y.PROD.GP NE 'MBL.PAD.CASH.LN' THEN
        RETURN
    END
    Y.DR.AMT  = EB.SystemTables.getRNew(LC.Contract.Drawings.TfDrDocumentAmount)
    
    GOSUB LN.AMT.CHK
    
    StartDate = EB.SystemTables.getToday()
    RequestType<2> = 'ALL'
    RequestType<3> = 'ALL'
    RequestType<4> = 'ECB'
    RequestType<4,2> = 'END'
    BaseBalance = 'CURACCOUNT'
    AA.Framework.GetPeriodBalances(Y.DR.ACCT, BaseBalance, RequestType, StartDate, EndDate, SystemDate, BalDetails, ErrorMessage)
    Y.WORK.BAL = ABS(BalDetails<4>)
    
    Y.LC.EXCHANGE.RATE = 1
    Y.CCY.MKT = '1'
    EB.DataAccess.FRead(FN.CURR,Y.DR.CURR,R.LC.CCY.REC,F.CURR,Y.LC.CCY.ERR)
    Y.LC.CCY.MARKET = R.LC.CCY.REC<ST.CurrencyConfig.Currency.EbCurCurrencyMarket>
    Y.LC.MID.REVAL.RATE = R.LC.CCY.REC<ST.CurrencyConfig.Currency.EbCurMidRevalRate>
    IF R.LC.CCY.REC THEN
        LOCATE Y.CCY.MKT IN Y.LC.CCY.MARKET<1,1> SETTING Y.CCY.POS THEN
            Y.LC.EXCHANGE.RATE = Y.LC.MID.REVAL.RATE<1,Y.CCY.POS>
        END
    END
     

    Y.AVAL.LN.BAL = Y.ACCT.LN.AMT - Y.WORK.BAL
    Y.AVAL.LN.BAL.IN.DOC.CURR = Y.AVAL.LN.BAL / Y.LC.EXCHANGE.RATE
    IF Y.DR.AMT GT Y.AVAL.LN.BAL.IN.DOC.CURR THEN
        EB.SystemTables.setAf(LC.Contract.Drawings.TfDrDocumentAmount)
        ETEXT = Y.DR.AMT:' Grater Then Debit Account Limit'
        EB.ErrorProcessing.StoreEndError()
    END

RETURN
*-----------
LN.AMT.CHK:
*-----------
    PROP.CLASS.SETT = 'ACCOUNT'
    AA.Framework.GetArrangementConditions(Y.AA.ID,PROP.CLASS.SETT,PROPERTY,'',RETURN.IDS.SETT,RETURN.VALUES.SETT,ERR.MSG.SETT)
    REC.DATA = RAISE(RETURN.VALUES.SETT)
    ACCT.LN.AMT = "LT.ACCT.LN.AMT"
    ACCT.AMOUNT.POS = ""
    EB.LocalReferences.GetLocRef("AA.ARR.ACCOUNT",ACCT.LN.AMT,ACCT.AMOUNT.POS)
    Y.ACCT.LN.AMT = REC.DATA<AA.Account.Account.AcLocalRef,ACCT.AMOUNT.POS>
RETURN

END