* @ValidationCode : MjotMTA2NTQxMjkxMjpDcDEyNTI6MTYxMjk0MDAxNTg1Mzp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Feb 2021 12:53:35
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0


SUBROUTINE CR.MBL.E.NOF.ACC.STATEMENT(Y.RETURN)
*-----------------------------------------------------------------------------

* Modification History :
* Modified by : MD SHIBLI MOLLAH
* Designation : Software Engineer
* Email : smollah@fortress-global.com
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    
    $USING EB.Reports
    $USING EB.SystemTables
    $USING ST.AccountStatement
    $USING AA.Framework
    $USING EB.API
    $USING EB.Utility
    $USING EB.DataAccess
    $USING ST.StmtPrinting
    $USING AC.Config
    $USING AC.EntryCreation
    $USING ST.Config
    $USING ST.Customer
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*----
INIT:
*----
    FN.SE = 'F.STMT.ENTRY'
    F.SE = ''
    FN.TXN = 'F.TRANSACTION'
    F.TXN = ''
    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AAA = ''
    Y.CLOSING.BALANCE = 0
    Y.CREDIT.BAL = 0
    Y.DEBIT.BAL = 0
    
RETURN

*---------
OPENFILES:
*---------
    EB.DataAccess.Opf(FN.SE,F.SE)
    EB.DataAccess.Opf(FN.TXN,F.TXN)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.AAA,F.AAA)
RETURN

*-------
PROCESS:
*-------

    LOCATE 'ACCT.ID' IN EB.Reports.getEnqSelection()<2,1> SETTING ACCT.POS THEN
        Y.ACCT.NO = EB.Reports.getEnqSelection()<4,ACCT.POS>
    END
    LOCATE 'FROM.DATE' IN EB.Reports.getEnqSelection()<2,1> SETTING FROM.POS THEN
        Y.FROM.DATE = EB.Reports.getEnqSelection()<4,FROM.POS>
    END
    LOCATE 'END.DATE' IN EB.Reports.getEnqSelection()<2,1> SETTING FROM.POS THEN
        Y.END.DATE = EB.Reports.getEnqSelection()<4,FROM.POS>
    END
*
    ST.AccountStatement.EbAcctEntryList(Y.ACCT.NO, Y.FROM.DATE, Y.END.DATE, Y.ENTRY.LIST, Y.OPEN.BALANCE, Er)
    Y.DCOUNT = DCOUNT(Y.ENTRY.LIST,FM)
    Y.OPEN = Y.OPEN.BALANCE
    FOR I = 1 TO Y.DCOUNT
        Y.SE.ID = Y.ENTRY.LIST<I>
        EB.DataAccess.FRead(FN.SE,Y.SE.ID,R.SE,F.SE,E.SE)
        Y.BALANCE.TYPE = R.SE<AC.EntryCreation.StmtEntry.SteBalanceType>
        Y.BALANCE = R.SE<AC.EntryCreation.StmtEntry.SteAmountLcy>
        Y.AAA.ID=R.SE<AC.EntryCreation.StmtEntry.SteTransReference>
        Y.TR.CODE = R.SE<AC.EntryCreation.StmtEntry.SteTransactionCode>
        EB.DataAccess.FRead(FN.AAA,Y.AAA.ID,R.AAA,F.AAA,E.AAA)
        Y.AAA.STATUS = R.AAA<AA.Framework.ArrangementActivity.ArrActRecordStatus>
        Y.ACTIVITY = R.AAA<AA.Framework.ArrangementActivity.ArrActActivity>
*        IF (Y.ACTIVITY EQ 'DEPOSITS-REDEEM-ARRANGEMENT' OR Y.ACTIVITY EQ 'DEPOSITS-MATURE-ARRANGEMENT' OR Y.ACTIVITY EQ 'DEPOSITS-CLOSE-ARRANGEMENT') AND Y.BALANCE.TYPE[1,3] EQ 'CUR' THEN
*            CONTINUE
*        END
        IF Y.TR.CODE NE '4529' AND Y.TR.CODE NE '4528' AND Y.TR.CODE NE '4571' AND Y.TR.CODE NE '2632' AND Y.TR.CODE NE '2635' AND Y.TR.CODE NE '4574' AND Y.TR.CODE NE '864' AND Y.TR.CODE NE '4559' AND Y.TR.CODE NE '4562' AND Y.TR.CODE NE '824' AND Y.TR.CODE NE '812' AND Y.TR.CODE NE '804' THEN
            IF Y.AAA.STATUS NE 'REVE' THEN

                IF(Y.BALANCE GE 0)THEN
                    Y.CREDIT.BAL=Y.BALANCE
                    Y.DEBIT.BAL=0
                END ELSE
                    Y.DEBIT.BAL=ABS(Y.BALANCE)
                    Y.CREDIT.BAL=0
                END
                Y.CLOSING.BALANCE = ABS(Y.OPEN.BALANCE - Y.CREDIT.BAL + Y.DEBIT.BAL)
                Y.OPEN.BALANCE = Y.CLOSING.BALANCE
                Y.CUS.ID = R.SE<AC.EntryCreation.StmtEntry.SteCustomerId>
                EB.DataAccess.FRead(FN.CUS,Y.CUS.ID,R.CUS,F.CUS,E.CUS)
                Y.CUS.NAME = R.CUS<ST.Customer.Customer.EbCusShortName>
                Y.CURR = R.SE<AC.EntryCreation.StmtEntry.SteCurrency>
                Y.TXN.ID = R.SE<AC.EntryCreation.StmtEntry.SteTransactionCode>
                EB.DataAccess.FRead(FN.TXN,Y.TXN.ID,R.TXN,F.TXN,E.TXN)
                Y.TXN.NAR = R.TXN<ST.Config.Transaction.AcTraNarrative>
                Y.NAR = R.SE<AC.EntryCreation.StmtEntry.SteNarrative>
                Y.DES = Y.TXN.NAR:' ':Y.NAR
                Y.BOOK.DATE=R.SE<AC.EntryCreation.StmtEntry.SteBookingDate>
                Y.VALUE.DATE=R.SE<AC.EntryCreation.StmtEntry.SteValueDate>
                Y.REF=R.SE<AC.EntryCreation.StmtEntry.SteTransReference>
                Y.RETURN<-1>= Y.ACCT.NO:'*':Y.CUS.ID:'*':Y.CUS.NAME:'*':Y.CURR:'*':Y.OPEN:'*':Y.BOOK.DATE:'*':Y.REF:'*':Y.DES:'*':Y.VALUE.DATE:'*':Y.DEBIT.BAL:'*':Y.CREDIT.BAL:'*':Y.CLOSING.BALANCE
*                                1              2              3           4         5            6             7         8             9               10              11                12
            END
        END
    NEXT I
RETURN
END