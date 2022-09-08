* @ValidationCode : MjoxNTc1NDExNzcyOkNwMTI1MjoxNjA0NDcyNjk3MjY3OkRFTEw6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Nov 2020 12:51:37
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : DELL
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CR.BD.NPA.CLASSIFICATION(Y.ARR.ID)
*-----------------------------------------------------------------------------
* Developed By- s.azam@fortress-global.com
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.BD.NPA.PARAM
    $INSERT I_F.BD.NPA.STATUS.DETAILS
    $INSERT I_F.CR.BD.NPA.CLASSIFICATION.COMMON
    
    $USING EB.DataAccess
    $USING AA.Framework
    $USING AA.Account
    $USING AA.TermAmount
    $USING EB.SystemTables
    $USING EB.API
    $USING EB.Interface
    $USING AA.Interest
    $USING AA.Limit
    $USING EB.Utility
    $USING AA.PaymentSchedule
    $USING EB.Foundation
    $USING LI.Config
    $USING AA.Overdue
    $USING AA.ProductFramework
    $USING EB.TransactionControl
*-----------------------------------------------------------------------------
    EB.DataAccess.FRead(FN.AAD, Y.ARR.ID, R.ADD, F.AAD, ERR.ADD)
    Y.SUSP.STATUS = R.ADD<AA.PaymentSchedule.AccountDetails.AdSuspended>
    
    EB.DataAccess.FRead(FN.AA, Y.ARR.ID, R.AA, F.AA, ERR.AA)
    Y.ACCOUNT.ID = R.AA<AA.Framework.Arrangement.ArrLinkedApplId>
    Y.CUS.ID = R.AA<AA.Framework.Arrangement.ArrCustomer>
    Y.PRODUCT.LINE = R.AA<AA.Framework.Arrangement.ArrProductLine>
    Y.PRODUCT.GROUP = R.AA<AA.Framework.Arrangement.ArrProductGroup>
    Y.COM = R.AA<AA.Framework.Arrangement.ArrCoCode>
    Y.TODAY = EB.SystemTables.getToday()
    
    Y.PROP.CLASS = 'ACCOUNT'
    GOSUB GET.CONDITIONS
    Y.QJ.MARK = R.REC<AA.Account.Account.AcLocalRef><1,Y.QJ.MARK.POS>
    Y.MAINTAIN.MANUAL = R.REC<AA.Account.Account.AcLocalRef><1,Y.MAINTAIN.MANUAL.POS>
    Y.AC.MATE.DATE = R.REC<AA.Account.Account.AcLocalRef><1,Y.AC.BD.LNMADT.POS>

    Y.PROP.CLASS = 'LIMIT'
    GOSUB GET.CONDITIONS
    Y.LIMIT.REF = R.REC<AA.Limit.Limit.LimLimitReference>
    EB.DataAccess.FRead(FN.LI.REF, Y.LIMIT.REF, R.LI.REF, F.LI.REF, ERR.LI.REF)
    Y.BD.NPA.GROUP = R.LI.REF<LI.Config.LimitReference.RefLocalRef><1,Y.LN.TYPE.POS>
    IF Y.BD.NPA.GROUP EQ '' THEN
        RETURN
    END
    Y.PROP.CLASS = 'TERM.AMOUNT'
    GOSUB GET.CONDITIONS
    Y.TERM = R.REC<AA.TermAmount.TermAmount.AmtTerm>
    Y.MATURITY.DATE = R.REC<AA.TermAmount.TermAmount.AmtMaturityDate>
    IF Y.MATURITY.DATE EQ '' THEN
        Y.MATURITY.DATE = R.ADD<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
    END
    GOSUB GET.UNPAID.DAYS
    GOSUB GET.NPA.STATUS
    GOSUB UPDATE.ASSET.CLASS
    WriteData = ''
    WriteData = 'Y.AA.ID=':Y.ARR.ID:'*':'Y.BD.NPA.GROUP=':Y.BD.NPA.GROUP:'*':'Y.PRODUCT.LINE=':Y.PRODUCT.LINE:'*':'Y.COM=':Y.COM:'*':'Y.NO.OF.DUE.DAYS=':Y.NO.OF.DUE.DAYS:'*':'Y.AC.MATE.DATE=':Y.AC.MATE.DATE:'*':'Y.MATURITY.DATE=':Y.MATURITY.DATE:'*':'Y.TODAY=':Y.TODAY:'*':'Y.LIMIT.REF=':Y.LIMIT.REF:'*':'Y.ASSET.CLASS=':Y.ASSET.CLASS:'*':'Y.NPA.DET.LAST.ASSET.CLASS=':Y.NPA.DET.LAST.ASSET.CLASS
    FileName = 'NPA.csv'
    FilePath = 'MBL.DATA'
    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
    ELSE
        CREATE FileOutput ELSE
        END
    END
    WRITESEQ WriteData APPEND TO FileOutput ELSE
        CLOSESEQ FileOutput
    END
    CLOSESEQ FileOutput
    IF (Y.QJ.MARK EQ '' OR Y.QJ.MARK EQ 'YES') AND (Y.MAINTAIN.MANUAL EQ '' OR Y.MAINTAIN.MANUAL EQ 'YES') THEN
*IF Y.QJ.MARK EQ 'YES' AND Y.MAINTAIN.MANUAL EQ 'YES' THEN
        RETURN
    END ELSE
        IF Y.ASSET.CLASS EQ '50' THEN
            Y.DATE = EB.SystemTables.getToday()
            Y.CCY = 'BDT'
            GOSUB TRIGGER.CHANGE.PRINCIPALINT
        END
        IF ((Y.SUSP.STATUS NE 'YES' AND Y.ASSET.CLASS GT '20')) THEN
            Y.DATE = EB.SystemTables.getToday()
            GOSUB TRIGGER.SUSPEND.ARR
        END
        IF (((Y.ASSET.CLASS LE '10' AND Y.SUSP.STATUS EQ 'YES') OR (Y.ASSET.CLASS EQ '' AND Y.SUSP.STATUS EQ 'YES'))) THEN
            Y.DATE = EB.SystemTables.getToday()
            GOSUB TRIGGER.RESUME.ARR
        END
        GOSUB UPDATE.AA.ASSET.CLASS
    END
RETURN
*--------------
GET.CONDITIONS:
*--------------
    AA.Framework.GetArrangementConditions(Y.ARR.ID,Y.PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    R.REC = RAISE(RETURN.VALUES)
RETURN

*---------------
GET.UNPAID.DAYS:
*---------------

    Y.NO.OF.DUE.DAYS=''
    IF (Y.BD.NPA.GROUP EQ '10' OR Y.BD.NPA.GROUP EQ '20' OR Y.BD.NPA.GROUP EQ '30' OR Y.BD.NPA.GROUP EQ '40' OR Y.BD.NPA.GROUP EQ '50' OR Y.BD.NPA.GROUP EQ '60' OR Y.BD.NPA.GROUP EQ '70') AND Y.PRODUCT.LINE EQ 'ACCOUNTS' THEN
        GOSUB CON.AC.GET.UNPAID.DAYS
    END
    IF Y.BD.NPA.GROUP EQ '20' AND Y.PRODUCT.LINE EQ 'LENDING' THEN
        GOSUB TERM.GET.UNPAID.DAYS
    END
    IF (Y.BD.NPA.GROUP EQ '10' OR Y.BD.NPA.GROUP EQ '30' OR Y.BD.NPA.GROUP EQ '40' OR Y.BD.NPA.GROUP EQ '50' OR Y.BD.NPA.GROUP EQ '60' OR Y.BD.NPA.GROUP EQ '70') AND Y.PRODUCT.LINE EQ 'LENDING' THEN
        GOSUB ALL.GET.UNPAID.DAYS
    END
RETURN

*--------------
GET.NPA.STATUS:
*--------------
    EB.DataAccess.FRead(FN.BD.NPA.PARAM, Y.BD.NPA.GROUP, R.NPA.PARAM, F.BD.NPA.PARAM, ERR.NAPA.PARAM)
    Y.CAC.TYPE = R.NPA.PARAM<BD.NPA.CALC.TYPE>
    Y.TOTAL.SLAB = DCOUNT(R.NPA.PARAM<BD.NPA.ASSET.CLASS>,VM)
    FOR J = 1 TO Y.TOTAL.SLAB
        Y.OVERDUE.INSTL.FR = FIELD(R.NPA.PARAM<BD.NPA.OVERDUE.DAY.FR>,VM,J)
        Y.OVERDUE.INSTL.TO = FIELD(R.NPA.PARAM<BD.NPA.OVERDUE.DAY.TO>,VM,J)
        IF (Y.NO.OF.DUE.DAYS GE Y.OVERDUE.INSTL.FR AND Y.NO.OF.DUE.DAYS LE Y.OVERDUE.INSTL.TO) THEN
            Y.ASSET.CLASS = FIELD(R.NPA.PARAM<BD.NPA.ASSET.CLASS>,VM,J)
            BREAK
        END
    NEXT J
RETURN

*------------------
UPDATE.ASSET.CLASS:
*------------------
    EB.DataAccess.FRead(FN.BD.NPA.STATUS, Y.ARR.ID, R.NPA.DET, F.BD.NPA.STATUS, ERR.NPA.DET)
    Y.NPA.DET.LAST.ASSET.CLASS = R.NPA.DET<BD.NPA.DET.LAST.ASSET.CLASS>
    IF Y.NPA.DET.LAST.ASSET.CLASS NE Y.ASSET.CLASS THEN
        Y.TOT.STATUS.CHNG = DCOUNT(R.NPA.DET<BD.NPA.DET.START.DATE>,VM) + 1
        R.NPA.DET<BD.NPA.DET.ASSET.CLASS,Y.TOT.STATUS.CHNG> = Y.ASSET.CLASS
        R.NPA.DET<BD.NPA.DET.START.DATE,Y.TOT.STATUS.CHNG> = EB.SystemTables.getToday()
        R.NPA.DET<BD.NPA.DET.CALC.OD.DAYS,Y.TOT.STATUS.CHNG> = Y.NO.OF.DUE.DAYS
        R.NPA.DET<BD.NPA.DET.LAST.ASSET.CLASS> = Y.ASSET.CLASS
        R.NPA.DET<BD.NPA.DET.GEN.TYPE> = 'SYSTEM'
        R.NPA.DET<BD.NPA.DET.PRODUCT.GROUP> = Y.PRODUCT.GROUP
        R.NPA.DET<BD.NPA.DET.INPUTTER> = EB.SystemTables.getOperator()
        R.NPA.DET<BD.NPA.DET.DATE.TIME> = EB.SystemTables.getTimeStamp()
        R.NPA.DET<BD.NPA.DET.AUTHORISER> = EB.SystemTables.getOperator()
        R.NPA.DET<BD.NPA.DET.CO.CODE> = Y.COM
        EB.DataAccess.FWrite(FN.BD.NPA.STATUS,Y.ARR.ID,R.NPA.DET)
        EB.TransactionControl.JournalUpdate('')
        SENSITIVITY = ''
    END
RETURN

*---------------------
UPDATE.AA.ASSET.CLASS:
*---------------------
    Y.ACTIVITY = 'LENDING-UPDATE-ACCOUNT'
    Y.PROPERTY = 'ACCOUNT'
    IF Y.PRODUCT.LINE EQ 'ACCOUNTS' THEN
        Y.ACTIVITY = 'ACCOUNTS-UPDATE-ACCOUNT'
    END
    OFS.SOURCE = 'MBL.PRE.OFS'
    OFS.STRING = ''
    OFS.STRING = 'AA.ARRANGEMENT.ACTIVITY,BD.NPA/I/PROCESS,//':Y.COM
    OFS.STRING := ',,ARRANGEMENT::=':Y.ARR.ID:',ACTIVITY::=':Y.ACTIVITY
    OFS.STRING := ',EFFECTIVE.DATE::=':Y.DATE:',CURRENCY::=':Y.CCY
    OFS.STRING := ',PROPERTY:1:1=':Y.PROPERTY:',FIELD.NAME:1:1=LT.ASSET.CLASS:1:1,FIELD.VALUE:1:1=':Y.ASSET.CLASS
    OFS.MSG.ID = ''
    OPTIONS = ''
*    EB.Interface.OfsPostMessage(OFS.STRING, OFS.MSG.ID, OFS.SOURCE, OPTIONS)
    EB.Interface.OfsCallBulkManager(OFS.SOURCE, OFS.STRING, OFS.RES.PR, TXN.VAL.PR)
*    EB.TransactionControl.JournalUpdate('')
    SENSITIVITY = ''
RETURN
*---------------------------
TRIGGER.CHANGE.PRINCIPALINT:
*---------------------------
    Y.PROP.CLASS = 'INTEREST'
    GOSUB GET.CONDITIONS
    
    Y.INT.RATE = R.REC<AA.Interest.Interest.IntFixedRate><1>
    IF Y.INT.RATE NE '0' THEN
        Y.ACTIVITY = 'LENDING-CHANGE-PRINCIPALINT'
        Y.PROPERTY = 'PRINCIPALINT'
        IF Y.PRODUCT.LINE EQ 'ACCOUNTS' THEN
            Y.ACTIVITY = 'ACCOUNTS-CHANGE-DRINTEREST'
            Y.PROPERTY = 'DRINTEREST'
        END
        OFS.SOURCE = 'MBL.PRE.OFS'
        OFS.STRING = ''
        OFS.STRING = 'AA.ARRANGEMENT.ACTIVITY,BD.NPA/I/PROCESS,//':Y.COM
        OFS.STRING := ',,ARRANGEMENT::=':Y.ARR.ID:',ACTIVITY::=':Y.ACTIVITY
        OFS.STRING := ',EFFECTIVE.DATE::=':Y.DATE:',CURRENCY::=':Y.CCY
        OFS.STRING := ',PROPERTY:1:1=':Y.PROPERTY:',FIELD.NAME:1:1=FIXED.RATE:1:1,FIELD.VALUE:1:1=0'
        OFS.MSG.ID = ''
        OPTIONS = ''
*        EB.Interface.OfsPostMessage(OFS.STRING, OFS.MSG.ID, OFS.SOURCE, OPTIONS)
        EB.Interface.OfsCallBulkManager(OFS.SOURCE, OFS.STRING, OFS.RES.PR, TXN.VAL.PR)
*        EB.TransactionControl.JournalUpdate('')
        SENSITIVITY = ''
    END
RETURN
*-------------------
TRIGGER.SUSPEND.ARR:
*-------------------
    Y.ACTIVITY = 'LENDING-SUSPEND-ARRANGEMENT'
    IF Y.PRODUCT.LINE EQ 'ACCOUNTS' THEN
        Y.ACTIVITY = 'ACCOUNTS-SUSPEND-ARRANGEMENT'
    END
    OFS.STRING = ''
    OFS.SOURCE= 'MBL.PRE.OFS'
    OFS.STRING = 'AA.ARRANGEMENT.ACTIVITY,BD.NPA/I/PROCESS,//':Y.COM
    OFS.STRING := ',,ARRANGEMENT::=':Y.ARR.ID:',ACTIVITY::=':Y.ACTIVITY
    OFS.STRING := ',EFFECTIVE.DATE::=':Y.DATE
    OFS.ERR = ''
    OFS.MSG.ID = ''
*    EB.Interface.OfsPostMessage(OFS.STRING, OFS.MSG.ID, OFS.SOURCE, OPTIONS)
    EB.Interface.OfsCallBulkManager(OFS.SOURCE, OFS.STRING, OFS.RES.PR, TXN.VAL.PR)
*    EB.TransactionControl.JournalUpdate('')
    SENSITIVITY = ''
    
    Y.ACTIVITY = 'LENDING-AGE-OVERDUE*STD*INTEREST'
    IF Y.PRODUCT.LINE EQ 'ACCOUNTS' THEN
        Y.ACTIVITY = 'ACCOUNTS-AGE.OVERDRAFT-LIMIT*GRC'
    END
    OFS.STRING = ''
    OFS.SOURCE= 'MBL.PRE.OFS'
    OFS.STRING = 'AA.ARRANGEMENT.ACTIVITY,BD.NPA/I/PROCESS,//':Y.COM
    OFS.STRING := ',,ARRANGEMENT::=':Y.ARR.ID:',ACTIVITY::=':Y.ACTIVITY
    OFS.STRING := ',EFFECTIVE.DATE::=':Y.DATE
    OFS.ERR = ''
    OFS.MSG.ID = ''
*    EB.Interface.OfsPostMessage(OFS.STRING, OFS.MSG.ID, OFS.SOURCE, OPTIONS)
    EB.Interface.OfsCallBulkManager(OFS.SOURCE, OFS.STRING, OFS.RES.PR, TXN.VAL.PR)
*    EB.TransactionControl.JournalUpdate('')
    SENSITIVITY = ''
RETURN

*------------------
TRIGGER.RESUME.ARR:
*------------------
    Y.ACTIVITY = 'LENDING-RESUME-ARRANGEMENT'
    IF Y.PRODUCT.LINE EQ 'ACCOUNTS' THEN
        Y.ACTIVITY = 'ACCOUNTS-RESUME-ARRANGEMENT'
    END
    OFS.STRING = ''
    OFS.SOURCE = 'MBL.PRE.OFS'
    OFS.STRING = 'AA.ARRANGEMENT.ACTIVITY,BD.NPA/I/PROCESS,//':Y.COM
    OFS.STRING := ',,ARRANGEMENT::=':Y.ARR.ID:',ACTIVITY::=':Y.ACTIVITY
    OFS.STRING := ',EFFECTIVE.DATE::=':Y.DATE
    OFS.ERR = ''
    OFS.MSG.ID = ''
*    EB.Interface.OfsPostMessage(OFS.STRING, OFS.MSG.ID, OFS.SOURCE, OPTIONS)
    EB.Interface.OfsCallBulkManager(OFS.SOURCE, OFS.STRING, OFS.RES.PR, TXN.VAL.PR)
*    EB.TransactionControl.JournalUpdate('')
    SENSITIVITY = ''
RETURN
*----------------------
CON.AC.GET.UNPAID.DAYS:
*----------------------
    IF Y.AC.MATE.DATE EQ '' THEN
        Y.NO.OF.DUE.DAYS = '1'
        Y.AC.MATE.DATE = Y.TODAY
    END
    IF Y.AC.MATE.DATE LT Y.TODAY THEN
        Y.NO.OF.DUE.DAYS = 'C'
        EB.API.Cdd('',Y.AC.MATE.DATE,Y.TODAY,Y.NO.OF.DUE.DAYS)
    END
RETURN
*--------------------
TERM.GET.UNPAID.DAYS:
*--------------------
    Y.BILL = R.ADD<AA.PaymentSchedule.AccountDetails.AdBillId>
    CONVERT VM TO FM IN Y.BILL
    IF NOT(Y.BILL) THEN
        Y.NO.OF.DUE.DAYS = '1'
        Y.START.DUE.DATE = EB.SystemTables.getToday()
        RETURN
    END
    Y.BILL.DATES = R.ADD<AA.PaymentSchedule.AccountDetails.AdBillDate>
    CONVERT SM TO FM IN Y.BILL.DATES
    CONVERT VM TO FM IN Y.BILL.DATES
    
    Y.BILL.TYPES = R.ADD<AA.PaymentSchedule.AccountDetails.AdBillType>
    CONVERT SM TO FM IN Y.BILL.TYPES
    CONVERT VM TO FM IN Y.BILL.TYPES
    
    Y.AGING.STATUSES = R.ADD<AA.PaymentSchedule.AccountDetails.AdAgingStatus>
    CONVERT SM TO FM IN Y.AGING.STATUSES
    CONVERT VM TO FM IN Y.AGING.STATUSES
    
    Y.SET.STATUS = R.ADD<AA.PaymentSchedule.AccountDetails.AdSetStatus>
    CONVERT SM TO FM IN Y.SET.STATUS
    CONVERT VM TO FM IN Y.SET.STATUS
    Y.CNT = COUNT(Y.SET.STATUS,FM) + 1
    FOR I = 1 TO Y.CNT
        Y.UNPAID.STATUS = FIELD(Y.SET.STATUS,FM,I)
        Y.AGING.STATUS =  FIELD(Y.AGING.STATUSES,FM,I)
*        IF (Y.UNPAID.STATUS EQ 'UNPAID' AND Y.AGING.STATUS NE 'SETTLED') THEN
        IF (Y.UNPAID.STATUS EQ 'UNPAID') THEN
            Y.TEMP.DUE.DATE = FIELD(Y.BILL.DATES,FM,I)
            Y.BILL.TYPE = FIELD(Y.BILL.TYPES,FM,I)
            IF (Y.TEMP.DUE.DATE NE EB.SystemTables.getToday() AND Y.BILL.TYPE EQ 'INSTALLMENT') THEN
                Y.START.DUE.DATE = Y.TEMP.DUE.DATE
                BREAK
            END
        END
    NEXT I
    IF Y.START.DUE.DATE NE '' THEN
        Y.NO.OF.DUE.DAYS = 'C'
        Y.TODAY = EB.SystemTables.getToday()
        EB.API.Cdd('',Y.START.DUE.DATE,Y.TODAY,Y.NO.OF.DUE.DAYS)
    END
    IF Y.NO.OF.DUE.DAYS EQ '' THEN
        Y.NO.OF.DUE.DAYS = '1'
        Y.START.DUE.DATE = EB.SystemTables.getToday()
    END
RETURN

*-------------------
ALL.GET.UNPAID.DAYS:
*-------------------
    IF Y.MATURITY.DATE EQ '' THEN
        Y.MATURITY.DATE = Y.TODAY
    END
    IF Y.MATURITY.DATE LT Y.TODAY THEN
        Y.NO.OF.DUE.DAYS = 'C'
        EB.API.Cdd('',Y.MATURITY.DATE,Y.TODAY,Y.NO.OF.DUE.DAYS)
    END
RETURN
END
